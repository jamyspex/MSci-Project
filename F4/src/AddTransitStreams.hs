module AddTransitStreams where

import           Utils
import           LanguageFortranTools


-- Sometimes streams are required beyond the kernel after the kernel that output
-- them. For example, the un and vn streams are output streams from the dyn1 kernel in the 2D
-- shallow water model, so the dyn2 kernel can access them fine but the verniuew kernel
-- also needs to access them. This is fine Fortran as they are stored as global variables, then passed
-- to the dyn subroutine with Intent(InOut), updated in place and then passed to the vernieuw subroutine. 
-- However when converting to streaming code this doesn't work...just like everything else then...as there
-- is no notion of global scope.   
--   
-- updated in place in and then passed
-- as . 
-- Due to 



addTransitStreams :: ArgumentTranslationTable -> [Kernel] -> IO [Kernel]
addTransitStreams argumentTransTable kernels = return (kernels)
 where

-- for a set of kernels to be placed in a pipeline find a list of 
-- streams that need to transitted through the smart caches in 
-- the pipeline for later use. 
findStreamsRequringTransit :: [Kernel] -> [((Int, Int), Stream Anno)]
findStreamsRequringTransit kernels = []


int
