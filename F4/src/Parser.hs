{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}

module Parser (Parser.parseFile) where

import           Language.Fortran
import           LanguageFortranTools as LFT

parseFile :: [String] -> [String] -> Bool -> String -> IO (Program Anno)
parseFile cppDArgs cppXArgs fixedForm filename = do
    parseOutput <- LFT.parseFile cppDArgs cppXArgs fixedForm filename
    let (parsedProgram, _, _) = parseOutput
    return (fst parsedProgram)
