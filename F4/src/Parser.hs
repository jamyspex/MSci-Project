{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}

module Parser (Parser.parseFile) where

import           Language.Fortran
import           LanguageFortranTools as LFT


--                                                           AST           Lines    Filename
parseFile :: [String] -> [String] -> Bool -> String -> IO ((Program Anno, [String], String))
parseFile cppDArgs cppXArgs fixedForm filename = do
    parseOutput <- LFT.parseFile cppDArgs cppXArgs fixedForm filename
    let ((parsedProgram, lines), _, _) = parseOutput
    return (parsedProgram, lines, filename)
