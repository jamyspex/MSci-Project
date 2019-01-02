module SanityChecks where

import           Data.Generics        (Data, Typeable, everything, everywhere,
                                       gmapQ, gmapT, mkQ, mkT)
import qualified Data.Map             as DMap
import           Language.Fortran
import           LanguageFortranTools
import           System.Exit

data SanityCheckResult = Success | Error String

crashWithError :: String -> a -> (a -> Bool) -> SanityCheckResult
crashWithError error input checkFunction =
    if checkFunction input
        then Success
        else Error error


printErrorOrContinue :: SanityCheckResult -> IO ()
printErrorOrContinue Success       = return ()
printErrorOrContinue (Error error) = do
    putStrLn error
    exitFailure

numberOfNodes :: (ProgUnit Anno -> [ProgUnit Anno]) -> Int -> Program Anno -> Bool
numberOfNodes query count node =
    (foldl (\acc cur -> (length $ (everything (++) (mkQ [] query) cur)) + acc) 0 node) == count

--     subAst          :: ProgUnit Anno,

checkFilesHaveOnlyOneSubroutine :: Program Anno -> SanityCheckResult
checkFilesHaveOnlyOneSubroutine prog =
    crashWithError "Program doesn't have exactly 1 subroutine." prog
        (numberOfNodes getSubs 1)
    where
        getSubs val@(Sub _ _ _ _ _ _) = [val]
        getSubs _                     = []
