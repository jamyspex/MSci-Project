module CodeGenUtils where

import           FortranDSL
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Utils

data KernelCallingData
  = KCD { argPositions :: [(Int, String)] }
  | NullCD
  deriving (Show)

generatePipeWriteCon :: Int -> String -> String -> String -> [Fortran Anno]
generatePipeWriteCon sourceIdx = generatePipeWrite (con sourceIdx)

generatePipeWrite :: Expr Anno -> String -> String -> String -> [Fortran Anno]
generatePipeWrite bufferIdx variableName bufferName pipeName =
  [ assign (var variableName) (arrayVar bufferName [bufferIdx])
  , writePipe [pipeName, variableName]
  ]

generatePipeReadCon :: Int -> String -> String -> String -> [Fortran Anno]
generatePipeReadCon assignmentIdx = generatePipeRead (con assignmentIdx)

generatePipeRead :: Expr Anno -> String -> String -> String -> [Fortran Anno]
generatePipeRead assignmentIdx pipeName readInVarName bufferName =
  [ readPipe [pipeName, readInVarName]
  , assign (arrayVar bufferName [assignmentIdx]) (var readInVarName)
  ]

getFortranTypeForStream (Stream _ _ streamValueType _) =
  case streamValueType of
    Float -> Real nullAnno
    _     -> Integer nullAnno

showProgUnitWithCallingData :: (ProgUnit Anno, KernelCallingData) -> IO ()
showProgUnitWithCallingData (kernel, callingData) = do
  putStrLn $ rule '-'
  putStrLn $ miniPPProgUnit kernel
  putStrLn $ rule '-'
  print callingData
  putStrLn $ rule '~'
