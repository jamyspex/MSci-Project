module CommandLineProcessor

where

import qualified Data.Attoparsec.Char8 as A
import           Data.List.Split
import           Data.Semigroup        ((<>))
import           Options.Applicative

data F4Opts =
    F4Opts {
        subsForFPGA :: [String],
        cppDefines  :: [String],
        cppExcludes :: [String],
        fixedForm   :: Bool,
        mainSub     :: String
    }
    deriving Show


mainSubParser :: Parser String
mainSubParser = strOption
    ( long "main"
    <> short 'm'
    <> metavar "MAIN FILE"
    <> help "Main file with time step loop.")

subsForFPGAParser :: Parser [String]
subsForFPGAParser = many ( strOption
    ( long "offload"
    <> short 'o'
    <> metavar "FILE FOR OFFLOAD"
    <> help "Files to offload to FPGA"))

cppDefinesParser :: Parser [String]
cppDefinesParser = option cppNameValueReader
    ( long "cppDefine"
    <> metavar "CPP DEFINES"
    <> short 'D'
    <> value []
    <> help "CPP #defines, format: NAME[=VALUE]" )

cppNameValueReader :: ReadM [String]
cppNameValueReader = eitherReader $ \s ->
    return $ splitOn "," s

-- singleDefineParser :: A.Parser

-- defineParser :: A.Parser [String]
-- defineParser = do
--     name <-

cppExcludesParser :: Parser [String]
cppExcludesParser = many ( strOption
    (  long "cppExclude"
    <> metavar "CPP EXCLUDES"
    <> help "CPP excludes" ))

fixedFormParser :: Parser Bool
fixedFormParser = switch
    ( long "fixedForm"
    <> help "Fixed form: limit input file lines to 72 columns")

f4Opts :: Parser F4Opts
f4Opts = F4Opts
    <$> subsForFPGAParser
    <*> cppDefinesParser
    <*> cppExcludesParser
    <*> fixedFormParser
    <*> mainSubParser

f4CmdParser :: ParserInfo F4Opts
f4CmdParser = info (f4Opts <**> helper)
    ( fullDesc
    <> progDesc "F4 is a source-to-source compiler that allows FORTRAN finite element codes to be compiled to OpenCL optimsed for execution on FPGAs."
    <> header "Compiler to convert FORTRAN finite element codes to be executed on FPGA devices" )

-- f4Opts = F4Opts {
--         mainSub = def &= argPos 0 &= typFile &= help "Main file with time step loop",
--         subsToParallelise = def &= args &= typFile,  -- &= help "",
--         cppDefines = def &= opt "cppDefine" &= typ "NAME[=VALUE]" &= help "CPP #define",
--         cppExcludes = def &= opt "cppExcludes" &= typ "NAME[=VALUE]" &= help "CPP include path",
--         fixedForm = def &= help "Fixed form: limit input file lines to 72 columns"
--     } &=
--     verbosity &=
--     help "Compiler to convert FORTRAN finite element codes to be executed on FPGA devices" &=
--     summary "F4 v0.0.0, (C) James Macdonald" &=
--     details ["F4 is a source-to-source compiler that allows FORTRAN finite element codes to be compiled to OpenCL optimsed for execution on FPGAs."]
