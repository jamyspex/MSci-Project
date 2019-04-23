module EnsureMemoryWriteback where

import           Data.List            (foldl', nubBy)
import qualified Data.Set             as Set
import           LanguageFortranTools
import           Utils

ensureMemoryWriteBack :: [Kernel] -> IO [Kernel]
ensureMemoryWriteBack kernels = do
  putStrLn $
    "Streams to write back:\n" ++
    concatMap (\s -> "\t" ++ show s ++ "\n") streamsToWriteBack
  putStrLn $ "Last kernel updated = \n" ++ show updatedLastKernel
  return (init kernels ++ [updatedLastKernel])
  where
    readBeforeWritten = getReadBeforeWritten kernels
    updatedLastKernel =
      lastKernel
        { outputs =
            nubBy
              (\s1 s2 -> getArrayNameFromStream s1 == getArrayNameFromStream s2)
              (outputs lastKernel ++ streamsToWriteBack)
        , inputs = updatedLastInputs
        }
    lastKernel = last kernels
    updatedLastInputs =
      foldl
        (\acc cur ->
           if getArrayNameFromStream cur `Set.notMember` lastKernelCurrentInputs
             then cur : acc
             else acc)
        (inputs lastKernel)
        streamsToWriteBack
    lastKernelCurrentInputs =
      Set.fromList $ map getArrayNameFromStream $ inputs lastKernel
    lastKernelOutputs =
      Set.fromList $ map getArrayNameFromStream $ outputs lastKernel
    streamsToWriteBack =
      filter
        (\s -> getStreamName s `Set.notMember` lastKernelOutputs)
        readBeforeWritten

getReadBeforeWritten :: [Kernel] -> [Stream Anno]
getReadBeforeWritten kernels = snd $ foldl go (Set.empty, []) kernels
  where
    allButLastKernel = init kernels
    outputStreamsButLast = map outputs allButLastKernel
    lastKernelOutputNames =
      Set.fromList $ map getStreamName $ outputs lastKernel
    lastKernel = last kernels
    go ::
         (Set.Set String, [Stream Anno])
      -> Kernel
      -> (Set.Set String, [Stream Anno])
    go (readSoFar, writeBack) kernel =
      (newReadSoFar, writeBack ++ hasBeenReadNowBeingWritten)
      where
        hasBeenReadNowBeingWritten =
          filter (\s -> getStreamName s `Set.member` newReadSoFar) $
          outputs kernel
        newReadSoFar =
          foldl' (flip Set.insert) readSoFar $ map getStreamName (inputs kernel)
