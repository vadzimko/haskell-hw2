{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module VCS
  ( vcsHandler
  ) where

import           Control.Monad.Trans.State (get, put)
import qualified Data.ByteString           as S
import           Data.Foldable             (foldrM)
import qualified Data.HashMap              as HS
import           Data.Map                  (toList)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe, isJust)
import           Structures
import           System.Console.ANSI

-- | Handle VCS commands
vcsHandler :: [String] -> FileSystem [Message]
vcsHandler arguments =
  case arguments of
    ("init":_) -> vcsInit
    ("debug":_) -> do
      root <- getVcsCurDir
      return [SimpleMessage $ show root]
    ("add":files) -> vcsAddFiles files
    ("status":_) -> vcsStatus
    ("commit":args) -> vcsCommit args
    ("hist":_) -> vcsHist
    _ -> return [errorMsg "Unknown vcs command, use \"help\""]

-- | Init vcs in current folder
vcsInit :: FileSystem [Message]
vcsInit = do
  curDir <- getCurDir
  let path = getDirPath curDir
  getVcsRootDir path >>= \case
    Just _ -> return [errorMsg "Vcs already inited in this or parent folder"]
    Nothing ->
      checkChild curDir >>= \case
        True -> return [errorMsg "Vcs already inited in this or child folder"]
        False -> do
          addVcsDir curDir
          return [SimpleMessage "Vcs inited!"]
  where
    checkChild :: Dir -> FileSystem Bool
    checkChild dir = do
      let dirPath = getDirPath dir
      isJust <$> getVCSDir dirPath >>= \case
        True -> return True
        False -> do
          results <- traverse checkChild <$> getDirDirs dir
          or <$> results
    addVcsDir :: Dir -> FileSystem ()
    addVcsDir Dir {..} = do
      FS {..} <- get
      let vcsPath = dirPath
      let newVcsDir = VcsDir dirPath Map.empty HS.empty []
      let newVCSMap = HS.insert vcsPath newVcsDir vcsMap
      modifyFSVcsMap newVCSMap

-- | Find vcs for current folder
getVcsCurDir :: FileSystem (Maybe VcsDir)
getVcsCurDir = getCurDirPath >>= getVcsRootDir

-- | Find root vcs (in parent directories)
getVcsRootDir :: FilePath -> FileSystem (Maybe VcsDir)
getVcsRootDir path =
  getVCSDir path >>= \case
    Nothing ->
      case getParentDirPath path of
        Nothing         -> return Nothing
        Just parentPath -> getVcsRootDir parentPath
    Just root -> return $ Just root

-- | get vcs dir by path
getVCSDir :: FilePath -> FileSystem (Maybe VcsDir)
getVCSDir path = HS.lookup path <$> getVCSMap

-- | Get map of filepath -> vcs dir
getVCSMap :: FileSystem (HS.Map FilePath VcsDir)
getVCSMap = do
  FS {..} <- get
  return vcsMap

-- | Add files to vcs
vcsAddFiles :: [String] -> FileSystem [Message]
vcsAddFiles filePaths =
  getVcsCurDir >>= \case
    Nothing -> return [errorMsg "VCS not inited!"]
    Just vcsDir -> do
      _ <- foldrM (flip vcsAddFile) vcsDir filePaths
      return [SimpleMessage ""]
  where
    vcsAddFile :: VcsDir -> String -> FileSystem VcsDir
    vcsAddFile vcsDir@VcsDir {..} fileNameToAdd = do
      curDir <- getCurDir
      findFileInDirExact curDir fileNameToAdd >>= \case
        Nothing -> return vcsDir
        Just File {..} -> do
          let newTracked = Map.insert filePath fileContent vcsTracked
          let newVcsDir = VcsDir vcsRoot newTracked vcsRevisions vcsCommits
          modifyFSVcsDir (getDirPath curDir) newVcsDir

-- | Modify vcs
modifyFSVcsFileRevisions ::
     VcsDir -> FilePath -> [VcsFileRevision] -> FileSystem VcsDir
modifyFSVcsFileRevisions vcsDir@VcsDir {..} filePath revisions =
  modifyFSVcsRevisionsMap vcsDir (HS.insert filePath revisions vcsRevisions)

-- | Modify vcs
modifyFSVcsRevisionsMap ::
     VcsDir -> HS.Map FilePath [VcsFileRevision] -> FileSystem VcsDir
modifyFSVcsRevisionsMap VcsDir {..} revisionsMap =
  modifyFSVcsDir vcsRoot $ VcsDir vcsRoot vcsTracked revisionsMap vcsCommits

-- | Modify vcs
modifyFSVcsTrackedMap ::
     VcsDir -> Map.Map FilePath S.ByteString -> FileSystem VcsDir
modifyFSVcsTrackedMap VcsDir {..} trackedMap =
  modifyFSVcsDir vcsRoot $ VcsDir vcsRoot trackedMap vcsRevisions vcsCommits

-- | Modify vcs
modifyFSVcsCommits :: VcsDir -> [String] -> FileSystem VcsDir
modifyFSVcsCommits VcsDir {..} commits =
  modifyFSVcsDir vcsRoot $ VcsDir vcsRoot vcsTracked vcsRevisions commits

-- | Modify vcs
modifyFSVcsDir :: FilePath -> VcsDir -> FileSystem VcsDir
modifyFSVcsDir path vcsDir = do
  FS {..} <- get
  modifyFSVcsMap $ HS.insert path vcsDir vcsMap
  return vcsDir

-- | Modify vcs
modifyFSVcsMap :: HS.Map FilePath VcsDir -> FileSystem ()
modifyFSVcsMap newVcsMap = do
  FS {..} <- get
  let newFS = FS fsPath fsInitTime curDir dirsMap newVcsMap
  put newFS

-- | Info about modified files
vcsStatus :: FileSystem [Message]
vcsStatus =
  getVcsCurDir >>= \case
    Nothing -> return [errorMsg "VCS not inited!"]
    Just VcsDir {..} ->
      if null vcsTracked
        then return [SimpleMessage "No changes."]
        else return $
             ColoredMessage Blue "Modified files:" :
             map SimpleMessage (Map.keys vcsTracked)

-- | Commit files in vcs with message
vcsCommit :: [String] -> FileSystem [Message]
vcsCommit args =
  case args of
    [] -> return [errorMsg "Add commit message, please!"]
    (commitMessage:_) ->
      getVcsCurDir >>= \case
        Nothing -> return [errorMsg "VCS not inited!"]
        Just vcsDir -> checkNoChanges vcsDir >>= \case
          True -> return [errorMsg "Nothing to commit."]
          False -> do
            makeCommit vcsDir commitMessage
            return [SimpleMessage "Commited."]
  where
    makeCommit :: VcsDir -> String -> FileSystem ()
    makeCommit vcsDir@VcsDir {..} message = do
      let commitNumber = length vcsCommits + 1
      newVcsDir <-
        foldrM
          (\(path, content) dir -> makeFileCommit dir commitNumber content path)
          vcsDir $
        toList vcsTracked

      commitVcsDir <- addCommitMessage message newVcsDir
      _ <- cleanTrackedFiles commitVcsDir
      return ()
    makeFileCommit ::
         VcsDir -> Int -> S.ByteString -> FilePath -> FileSystem VcsDir
    makeFileCommit vcsDir@VcsDir {..} commitNumber content path = do
      let newRevision = VcsFileRevision path content commitNumber
      oldRevisions <- getFileRevisions vcsDir path
      modifyFSVcsFileRevisions vcsDir path (oldRevisions ++ [newRevision])
    addCommitMessage :: String -> VcsDir -> FileSystem VcsDir
    addCommitMessage message vcsDir = do
      oldCommits <- getVcsDirCommits vcsDir
      modifyFSVcsCommits vcsDir (oldCommits ++ [message])
    cleanTrackedFiles :: VcsDir -> FileSystem VcsDir
    cleanTrackedFiles vcsDir = modifyFSVcsTrackedMap vcsDir Map.empty
    checkNoChanges :: VcsDir -> FileSystem Bool
    checkNoChanges VcsDir{..} = return $ Map.null vcsTracked

-- | Get all file revisions list
getFileRevisions :: VcsDir -> FilePath -> FileSystem [VcsFileRevision]
getFileRevisions VcsDir {..} path =
  return $ fromMaybe [] $ HS.lookup path vcsRevisions

-- | Get vcs commits list
getVcsDirCommits :: VcsDir -> FileSystem [String]
getVcsDirCommits VcsDir {..} = return vcsCommits

-- | Get list of commits in current vcs
vcsHist :: FileSystem [Message]
vcsHist =
  getVcsCurDir >>= \case
    Nothing -> return [errorMsg "VCS not inited!"]
    Just VcsDir {..} -> do
      let msgs =
            foldr
              (\c acc -> (show (length acc) ++ ") " ++ c) : acc)
              []
              vcsCommits
      return $ map SimpleMessage msgs

