module AddSmartCaches where

import Language.Fortran
import LanguageFortranTools
import Pipeline

-- This module anlayses the list of kernel subroutines and their required
-- input streams. If it finds StencilStream inputs it constructs an appropriate smart cache
-- and inserts it into the list with the appropriate position value set.
-- insertSmartCaches :: [(Int, ProgUnit Anno)] -> [(Int, Pipeline SharedPipelineData)]
-- insertSmartCaches kernelsAndOrder = kernelsAndOrder

-- convertKernelToPipelineItems :: Kernel -> Pipeline SharedPipelineData
-- convertKernelToPipelineIte kernel =
--     where
--         getReductionVar
