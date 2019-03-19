module AddTransitStreams where

import qualified Data.Set                      as Set
import           Language.Fortran
import           LanguageFortranTools
import           Utils

-- Sometimes streams are required beyond the kernel after the kernel that output them. For example,
-- the un and vn streams are output streams from the dyn1 kernel in the 2D shallow water model, so the
-- dyn2 kernel can access them fine but the verniuew kernel also needs to access them. This is fine in
-- Fortran as they are stored as global variables, then passed to the dyn subroutine with Intent(InOut),
-- updated in place and then passed to the vernieuw subroutine. However when converting to streaming code
-- this doesn't work...just like everything else...as there is no notion of global scope. So if a
-- stream is used in a kernel but the previous kernel in the pipeline does not output that stream we need
-- to introduce a transit stream. A transit stream is basically a way to move a value through the pipeline
-- keeping it in sync with other streams entering the kernel where it is used. This means if the streams move
-- through a smart cache the transit stream needs to also go through the smart cache.
--
--                                                                                                        However K3 accesses B
--   _________            __________            _________            ________             _________             ________
--  |         | -- A --> |          | -- A --> |         | -- A --> |        |  -- A --> |         |  -- A --> |        |
--  |    K0   |          |   SC 0   |          |    K1   |          |   K2   |           |   SC 1  |           |   K3   |
--  |         | -- B --> |          | -- B --> |         |          |        |           |         |           |        |
--   ---------            ----------            ---------            --------             ---------             --------
--
-- In the pipeline above B is needed in K3 but the last kernel that output it was K0 therefore we need to channel
-- the stream from K1 via K2, the smart cache SC1 and finally into K3. The stream must be passed through any smart
-- caches in order to keep it in sync with other streams entering the kernel after the smart cache. To do this
-- create a stencil stream that only has one point (0, 0).
--
-- If a kernel only reads values from a stream (e.g. has it as an input stream but not an output stream) and the
-- stream is stored in global memory we don't need to transit the stream through the pipeline and can instead we can
-- emit a memory reader kernel saving saves smart cache space. However, if the stream has ever be output by a kernel
-- earlier in the pipeline the usual rules apply and the stream must be transited through the pipeline to where it
-- is accessed again.
--
--
--
--
addTransitStreams :: [Kernel] -> IO [Kernel]
addTransitStreams kernels = do
  putStrLn $ concatMap (printMismatch kernels) unmatchedStreams
  putStrLn hl
  putStrLn
    $ concatMap (printPotentialTransitStreams kernels) potentialTransitStreams
  putStrLn "Transit Streams Added"
  mapM_ print updatedKernels
  return updatedKernels
 where
  unmatchedStreams        = getUnmatchedInputStreams kernels
  potentialTransitStreams = getStreamsToTransit kernels unmatchedStreams
  updatedKernels          = insertTransitStreams potentialTransitStreams kernels

-- Using the previously identified transit streams update the itermediate
-- kernels between the producer and the consumer to have the transit streams
-- as inputs and outputs.
insertTransitStreams :: [((Int, Int), Stream Anno)] -> [Kernel] -> [Kernel]
insertTransitStreams transitStreams kernels = foldl addTransitStreamToKernels
                                                    kernels
                                                    transitStreams
 where
  addTransitStreamToKernels :: [Kernel] -> ((Int, Int), Stream Anno) -> [Kernel]
  addTransitStreamToKernels kernels transitStream = map
    (addTransitStreamToKernel transitStream)
    kernels
   where
    addTransitStreamToKernel :: ((Int, Int), Stream Anno) -> Kernel -> Kernel
    addTransitStreamToKernel ((prodIdx, consumeIdx), stream) kernel
      | order kernel <= prodIdx    = kernel
      | order kernel >= consumeIdx = kernel
      | otherwise                  = updatedKernel
     where
      smartCacheRequired = any isStencil $ inputs kernel
      updatedKernel      = if smartCacheRequired
        then kernel
          { inputs  = if inputRequired
                        then buildTransitStream stream : orgIns
                        else orgIns
          , outputs = if outputRequired then stream : orgOuts else orgOuts
          }
        else kernel
          { inputs  = if inputRequired then stream : orgIns else orgIns
          , outputs = if outputRequired then stream : orgIns else orgIns
          }
      orgIns            = inputs kernel
      orgOuts           = outputs kernel
      outputRequired    = streamName `notElem` outputStreamNames
      inputRequired     = streamName `notElem` inputStreamNames
      streamName        = getStreamName stream
      inputStreamNames  = map getStreamName $ inputs kernel
      outputStreamNames = map getStreamName $ outputs kernel

buildTransitStream (Stream name arrayName valueType dims) =
  TransitStream name arrayName valueType dims
buildTransitStream s = s

printMismatch kernels (order, stream) =
  kernelName (kernels !! order) ++ " requires:\n" ++ printStream stream ++ "\n"

printPotentialTransitStreams kernels ((producedAt, consumedAt), stream) =
  printStream stream
    ++ "\nProduced at: "
    ++ kernelName (kernels !! producedAt)
    ++ "\nConsumed at: "
    ++ kernelName (kernels !! consumedAt)
    ++ "\n"

-- Take the list of kernel ids and unmatched streams and
-- search the pipeline for the last kernel that produced
-- each stream. If a kernel producing the stream is found
-- create a ((producing kernel order, consuming kernel order),
-- stream), if it is not found remove the stream from the
-- unmatched list as it must have come from memory and therefore
-- and memory reader can be emitted at a later stage of compilation.
getStreamsToTransit
  :: [Kernel] -> [(Int, Stream Anno)] -> [((Int, Int), Stream Anno)]
getStreamsToTransit kernels = concatMap (searchForStreamProduction kernels)

-- Search through the pipeline looking for the latest kernel producing
-- the stream in question. If found return 1 element list containing
-- the order of the last kernel producing the stream otherwise return an
-- empty list. If the stream is not found as an output of a kernel
-- preceeding this one in the pipeline then it must have come from memory
searchForStreamProduction
  :: [Kernel] -> (Int, Stream Anno) -> [((Int, Int), Stream Anno)]
searchForStreamProduction kernels (consumedAt, stream) =
  [ ((foundAt, consumedAt), stencilStreamToTransitStream stream) | found ]
 where
  streamName          = getArrayNameFromStream stream
  (_, found, foundAt) = foldl go (streamName, False, 0) kernels
  go :: (String, Bool, Int) -> Kernel -> (String, Bool, Int)
  go (searchingFor, found, foundAt) kernel =
    (searchingFor, foundHere || found, nFoundAt)
   where
    outputNames = map getArrayNameFromStream $ outputs kernel
    foundHere   = searchingFor `elem` outputNames && order kernel < consumedAt
    nFoundAt    = if foundHere then order kernel else foundAt

stencilStreamToTransitStream :: Stream Anno -> Stream Anno
stencilStreamToTransitStream (StencilStream name arrayName valueType dims _) =
  TransitStream name arrayName valueType dims
stencilStreamToTransitStream stream = stream

-- Find kernels that require input streams for which the previous
-- kernel in the pipeline does not output a matching output stream
getUnmatchedInputStreams :: [Kernel] -> [(Int, Stream Anno)]
getUnmatchedInputStreams kernels = if valid
  then unmatchedStreams
  else error "Kernel ordering incorrect"
 where
  firstKernel           = head kernels
  valid                 = all (\k -> order k > order firstKernel) (tail kernels)
  streamNames           = map getStreamName $ outputs firstKernel
  initialAvailable      = Set.fromList streamNames
  (_, unmatchedStreams) = foldl go (initialAvailable, []) (tail kernels)
-- Internal workings of getUnmatchedInputStreams. Fold over the pipeline and
-- find kernels that require input streams that are not emitted as output streams
-- from the kernel proceeding them in the pipeline.
  go
    :: (Set.Set String, [(Int, Stream Anno)])
    -> Kernel
    -> (Set.Set String, [(Int, Stream Anno)])
  go (availableStreams, unmatchedStreams) kernel =
    (Set.fromList outputStreamNames, unmatchedStreams ++ newUnmatched)
   where
    outputStreamNames    = map getStreamName $ outputs kernel
    requiredInputStreams = inputs kernel
    newUnmatched         = foldl
      (\unmatched stream -> unmatched ++ checkIfStreamsMatched stream)
      []
      requiredInputStreams
    checkIfStreamsMatched :: Stream Anno -> [(Int, Stream Anno)]
    checkIfStreamsMatched s@(StencilStream name _ _ _ _) = check s name
    checkIfStreamsMatched s@(Stream name _ _ _         ) = check s name
    check s name =
      if Set.member name availableStreams then [] else [(order kernel, s)]
