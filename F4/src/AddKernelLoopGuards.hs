module StencilDetection where

import           Data.Data
import           Data.Generics         (Data, Typeable, everything, everywhere,
                                        gmapQ, gmapT, mkQ, mkT)
import           Data.List
import qualified Data.Map              as DMap
import           Debug.Trace
import           F95IntrinsicFunctions
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Parser
import           Text.Read
import           Utils

addLoopGuards :: SubRec -> SubRec
addLoopGuards subRec = 
    where
        
        
addLoopGuardToMap :: Fortran Anno -> Fortran Anno 
addLoopGuardToMap OpenCLMap _ _ _ _ loopVars _ body = 
addLopoGuardToMap OpenCLReduce _ _ _ _ loopVars _ _ body =
    where
        loopVars
        loopIndexesUsed =  
