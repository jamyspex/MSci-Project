module CodeGenUtils where

import           MiniPP
import           FortranDSL
import           Language.Fortran
import           LanguageFortranTools
import           Utils


data KernelCallingData = KCD { argPositions :: [( Int, String)] } | NullCD deriving (Show)

generatePipeWriteCon :: Int -> String -> String -> String -> [Fortran Anno]
generatePipeWriteCon sourceIdx = generatePipeWrite (con sourceIdx)
  -- [ comment ("write pipe " ++ pipeName)
  -- , assign (var variableName) (arrayVar bufferName [con sourceIdx])
  -- , call "writePipe" [pipeName, variableName]
  -- , call "memFence"  ["CLK_CHANNEL_MEM_FENCE"]
  -- ]

generatePipeWrite :: Expr Anno -> String -> String -> String -> [Fortran Anno]
generatePipeWrite bufferIdx variableName bufferName pipeName =
  [ comment ("write pipe " ++ pipeName)
  , assign (var variableName) (arrayVar bufferName [bufferIdx])
  , call "writePipe" [pipeName, variableName]
  , call "memFence"  ["CLK_CHANNEL_MEM_FENCE"]
  ]

generatePipeReadCon :: Int -> String -> String -> String -> [Fortran Anno]
generatePipeReadCon assignmentIdx = generatePipeRead (con assignmentIdx)
    -- [ comment ("read pipe " ++ pipeName)
    -- , call "readPipe" [pipeName, readInVarName]
    -- , assign (arrayVar bufferName [con assignmentIdx]) (var readInVarName)
    -- , call "memFence" ["CLK_CHANNEL_MEM_FENCE"]
    -- ]

generatePipeRead :: Expr Anno -> String -> String -> String -> [Fortran Anno]
generatePipeRead assignmentIdx pipeName readInVarName bufferName =
  [ comment ("read pipe " ++ pipeName)
  , call "readPipe" [pipeName, readInVarName]
  , assign (arrayVar bufferName [assignmentIdx]) (var readInVarName)
  , call "memFence" ["CLK_CHANNEL_MEM_FENCE"]
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
