module CommandLineProcessor

where

import qualified Data.Attoparsec.Char8 as A
import           Data.List.Split
import           Data.Semigroup        ((<>))
import           Options.Applicative

data F4Opts = F4Opts {
        subsForFPGA :: [String],
        cppDefines  :: [String],
        cppExcludes :: [String],
        fixedForm   :: Bool,
        mainSub     :: String,
        ioSubs      :: [String],
        sourceDir   ::  String
    }

instance Show F4Opts where
    show opts =
        "The following command line values were parsed:\n\n" ++
        "Files with subroutines to be parallelised:\n" ++
        (concatMap (\file -> "\t" ++ file ++ "\n") (subsForFPGA opts)) ++
        "File containing main subroutine: \n\t" ++ (mainSub opts) ++ "\n" ++
        "Source directory:\n\t" ++ (sourceDir opts) ++ "\n" ++
        "Fixed form: " ++ (show $ fixedForm opts) ++ "\n" ++
        "CPP Defines: " ++ (concatMap (\def -> def ++ ", ") (cppDefines opts)) ++ "\n" ++
        "CPP Excludes:\n" ++
        (concatMap (\file -> "\t" ++ file ++ "\n") (cppExcludes opts))

sourceDirParser :: Parser String
sourceDirParser = strOption
    ( long "sourceDir"
    <> short 's'
    <> value "./"
    <> metavar "<MAIN SRC DIR>"
    <> help "Directory containing the FORTRAN code" )

mainSubParser :: Parser String
mainSubParser = strOption
    ( long "main"
    <> short 'm'
    <> metavar "<MAIN FILE>"
    <> help "Main file with time step loop.")

subsForFPGAParser :: Parser [String]
subsForFPGAParser = many ( strOption
    ( long "offload"
    <> short 'o'
    <> metavar "<FILE FOR OFFLOAD>"
    <> help "Files to offload to FPGA"))

otherSubsParser :: Parser [String]
otherSubsParser = many ( strOption
    ( long "othersub"
    <> short 's'
    <> metavar "<FILE CONTAINING SUBROUTINE>"
    <> help "Files containing subroutines used by program"))

cppDefinesParser :: Parser [String]
cppDefinesParser = option cppNameValueReader
    ( long "cppDefine"
    <> metavar "<CPP DEFINES>"
    <> short 'D'
    <> value []
    <> help "CPP #defines, format: NAME[=VALUE]" )

cppNameValueReader :: ReadM [String]
cppNameValueReader = eitherReader $ \s ->
    return $ splitOn "," s

cppExcludesParser :: Parser [String]
cppExcludesParser = many ( strOption
    (  long "cppExclude"
    <> metavar "<CPP EXCLUDES>"
    <> help "CPP excludes" ))

ioSubsParser :: Parser [String]
ioSubsParser = pure []
    -- many ( strOption
    -- (  long "ioSubroutines"
    -- <> metavar "<IO SUBROUTINES>"
    -- <> help "routines that performIO" ))

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
    <*> ioSubsParser
    <*> sourceDirParser

f4CmdParser :: ParserInfo F4Opts
f4CmdParser = info (f4Opts <**> helper)
    ( fullDesc
    <> progDesc "F4 is a source-to-source compiler that allows FORTRAN finite element codes to be compiled to OpenCL optimsed for execution on FPGAs."
    <> header "Compiler to convert FORTRAN finite element codes to be executed on FPGA devices" )
