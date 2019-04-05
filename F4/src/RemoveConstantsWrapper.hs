{-# LANGUAGE RecordWildCards #-}

module RemoveConstantsWrapper where

import           CommandLineProcessor
import           ConstantFolding             (foldConstants)
import qualified Data.Map                    as DMap
import           Debug.Trace
import           MiniPP
import           Parser
import           RemoveConstantsFromStencils
import           System.Directory
import           System.FilePath.Posix
import           System.IO.Temp
import           Utils

-- HA Ha ha what a mess
-- The stencil constant removal pass inserts nodes to the AST which then don't have
-- their source location properties set. This breaks Gavin's access analysis... :)
-- So take the parsed data, remove the constants write the files out to temp location
-- and then reparse them and put them back in the subroutine table...........
-- LOL
removeStencilConstantsWrapper :: F4Opts -> SubroutineTable -> IO SubroutineTable
removeStencilConstantsWrapper options subroutineTable = do
  createDirectoryIfMissing True scratchDirName
  createDirectoryIfMissing True "consts"
  writeOutSubTable withConstantsFolded "consts"
  writeOutSubTable stencilsWithNoConstants scratchDirName
  parseProgramData updatedOpts
  where
    updatedOpts = options {sourceDir = scratchDirName}
    stencilsWithNoConstants = removeStencilWrapper withConstantsFolded
    withConstantsFolded = foldConsts subroutineTable

foldConsts :: SubroutineTable -> SubroutineTable
foldConsts = DMap.map updateOne
  where
    updateOne :: SubRec -> SubRec
    updateOne subRec@MkSubRec {..} =
      if parallelise
        then subRec {subAst = foldConstants subAst}
        else subRec

removeStencilWrapper :: SubroutineTable -> SubroutineTable
removeStencilWrapper = DMap.map updateOne
  where
    updateOne :: SubRec -> SubRec
    updateOne subRec@MkSubRec {..} =
      if parallelise
        then subRec {subAst = removeConstantsFromStencils subAst}
        else subRec

writeOutSubTable :: SubroutineTable -> FilePath -> IO ()
writeOutSubTable subTable tempDir = mapM_ writeOne $ DMap.elems subTable
  where
    writeOne :: SubRec -> IO ()
    writeOne MkSubRec {..} = do
      putStrLn $ "Writing to " ++ tempFileName
      writeFile tempFileName (miniPPProgUnitNoTruncate subAst)
      where
        tempFileName = tempDir </> takeFileName subSrcFile
