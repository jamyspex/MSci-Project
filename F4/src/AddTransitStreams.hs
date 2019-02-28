module AddTransitStreams where

import           LanguageFortranTools
import           Utils

-- Sometimes streams are required beyond the kernel after the kernel that output them. For example,
-- the un and vn streams are output streams from the dyn1 kernel in the 2D shallow water model, so the
-- dyn2 kernel can access them fine but the verniuew kernel also needs to access them. This is fine
-- Fortran as they are stored as global variables, then passed to the dyn subroutine with Intent(InOut),
-- updated in place and then passed to the vernieuw subroutine. However when converting to streaming code
-- this doesn't work...just like everything else then...as there is no notion of global scope. So if a
-- stream is used in a kernel but the previous kernel in the pipeline does not output that stream we need
-- to introduce a transit stream. A transit stream is basically a way to move a value through the pipeline
-- keeping it in sync with other streams entering the kernel. This means if the streams move through a smart
-- cache the transit stream needs to also go through the smart cache.
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
--
--
--
addTransitStreams :: ArgumentTranslationTable -> [Kernel] -> IO [Kernel]
addTransitStreams argumentTransTable kernels = return (kernels)
  where


-- for a set of kernels to be placed in a pipeline find a list of
-- streams that need to transitted through the smart caches in
-- the pipeline for later use.
findStreamsRequringTransit :: [Kernel] -> [((Int, Int), Stream Anno)]
findStreamsRequringTransit kernels = []
