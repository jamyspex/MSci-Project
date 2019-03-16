module DetectDriverLoopSize where

import           Data.List
import           LanguageFortranTools
import           Utils

-- Take all the kernels and find the stream with the largest
-- dimensions across them all.
detectDriverLoopSize :: [Kernel] -> IO (Int, Int)
detectDriverLoopSize kernels = do
  putStrLn $
    largestStreamName ++ " is largest stream size = " ++ show largestStreamSize
  return (0, largestStreamSize)
  where
    allStreams = concatMap (\k -> inputs k ++ outputs k) kernels
    streamNamesAndSizes =
      map
        (\s -> (getStreamName s, (calculateStreamSize . getStreamDimensions) s))
        allStreams
    (largestStreamName, largestStreamSize) =
      maximumBy
        (\(_, size1) (_, size2) -> size1 `compare` size2)
        streamNamesAndSizes

calculateStreamSize :: [(Int, Int)] -> Int
calculateStreamSize = foldl (\acc (lwb, upb) -> ((upb - lwb) + 1) * acc) 1

updatePipelineSharedData :: (Int, Int) -> [PipelineStage] -> [PipelineStage]
updatePipelineSharedData (lowerBound, upperBound) = map updateStage
  where
    updateStage :: PipelineStage -> PipelineStage
    updateStage (kernel, smartCache, memAccess) =
      (newKernel, newSmartCache, newMemAccess)
      where
        updatedSharedData =
          (sharedData kernel)
            { driverLoopUpperBound = upperBound
            , driverLoopLowerBound = lowerBound
            }
        newKernel = updateItem kernel
        newSmartCache = fmap updateItem smartCache
        newMemAccess = fmap updateItem memAccess
        updateItem ::
             PipelineItem SharedPipelineData -> PipelineItem SharedPipelineData
        updateItem item = item {sharedData = updatedSharedData}
