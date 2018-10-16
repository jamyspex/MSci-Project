module F95StatementParser  (parseF95Statement) 

where

--import qualified Data.Map as DMap
import Language.Fortran.Parser
import Language.Fortran
import LanguageFortranTools
-- Must be compiled with ghc --make -i../language-fortran/src/

-- This function takes a single line of F95
-- It returns the string resulting from calling `show` on the parsed expression
parseF95StatementShow :: String -> String
parseF95StatementShow f95_line = show $ statement_parse f95_line

parseF95Statement :: String -> Fortran Anno
parseF95Statement f95_line = statement_parse f95_line
