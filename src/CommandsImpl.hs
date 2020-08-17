{-# LANGUAGE RecordWildCards #-}

module CommandsImpl
  ( createFileInCurDir
  , removeFileFromCurDir
  , createDirInCurDir
  , removeDirFromCurDir
  , writeToFile
  , findFilesRecursive
  , changeDirectory
  ) where

import           Control.Monad.Trans.State (get, put)
import qualified Data.ByteString           as S
import qualified Data.ByteString.UTF8      as BSU
import qualified Data.HashMap              as HS
import           Data.List                 (isInfixOf)
import           Data.List.Split           (splitOn)
import           Data.Time.Clock           (UTCTime)
import           Structures

-- | Create file in current directory 
createFileInCurDir :: String -> FileSystem (Either String String)
createFileInCurDir fileName = do
  FS {..} <- get
  maybeExists <- findFileInDirExact curDir fileName
  case maybeExists of
    Just _ -> return $ Left "Such file already exists!"
    Nothing -> do
      let newFile = createNewFile curDir fsInitTime
      updateCurDir $ addFileToDir curDir newFile
      return $ Right "File created successfully!"
  where
    createNewFile :: Dir -> UTCTime -> File
    createNewFile dir@Dir {..} initTime =
      let path = getDirChildPath dir fileName
       in File fileName path 0 S.empty initTime dirPermissions

-- | Remove file from current directory
removeFileFromCurDir :: String -> FileSystem (Either String String)
removeFileFromCurDir fileName = do
  FS {..} <- get
  maybeExists <- findFileInDirExact curDir fileName
  case maybeExists of
    Nothing -> return $ Left "No such file in directory!"
    Just file -> do
      updateCurDir $ removeFileFromDir curDir file
      return $ Right "File removed successfully!"

-- | Add new file to current dir
addFileToDir :: Dir -> File -> Dir
addFileToDir dir@Dir {..} file = changeDirFiles dir (file : dirFiles)

-- | Remove file from current dir
removeFileFromDir :: Dir -> File -> Dir
removeFileFromDir dir@Dir {..} File {..} =
  changeDirFiles dir (filter (not . (\f -> getFileName f == fileName)) dirFiles)

-- | Replaces file in dir
replaceFileInDir :: Dir -> File -> Dir
replaceFileInDir dir file = addFileToDir (removeFileFromDir dir file) file

-- | Set new files in dir
changeDirFiles :: Dir -> [File] -> Dir
changeDirFiles Dir {..} files =
  Dir dirName dirPath files dirDirs dirPermissions dirSize

-- | Create new directory in current dir 
createDirInCurDir :: String -> FileSystem (Either String String)
createDirInCurDir name = do
  FS {..} <- get
  maybeExists <- findDirInDirExact curDir name
  case maybeExists of
    Just _ -> return $ Left "Such directory already exists!"
    Nothing -> do
      updateDir $ createNewDir curDir
      updateCurDir $ addDirToDir curDir name
      return $ Right "Directory created successfully!"
  where
    createNewDir :: Dir -> Dir
    createNewDir dir@Dir {..} =
      let path = getDirChildPath dir name
       in Dir name path [] [] dirPermissions 0

-- | Update dir in Fs 
updateCurDir :: Dir -> FileSystem ()
updateCurDir dir = do
  FS {..} <- get
  let updatedMap = HS.insert (getDirPath dir) dir dirsMap
  let newFs = FS fsPath fsInitTime dir updatedMap vcsMap
  put newFs

-- | Set new dir in FS 
updateDir :: Dir -> FileSystem ()
updateDir dir = do
  FS {..} <- get
  let updatedMap = HS.insert (getDirPath dir) dir dirsMap
  let newFs = FS fsPath fsInitTime curDir updatedMap vcsMap
  put newFs

-- | Remove dir from fs 
removeDir :: Dir -> FileSystem ()
removeDir dir = do
  FS {..} <- get
  let updatedMap = HS.delete (getDirPath dir) dirsMap
  let newFs = FS fsPath fsInitTime curDir updatedMap vcsMap
  put newFs

-- | Remove dir from current dir by name 
removeDirFromCurDir :: String -> FileSystem (Either String String)
removeDirFromCurDir name = do
  FS {..} <- get
  maybeExists <- findDirInDirExact curDir name
  case maybeExists of
    Nothing -> return $ Left "Such directory does not exist!"
    Just dir -> do
      removeDir dir
      updateCurDir $ removeDirFromDir curDir name
      return $ Right "Directory removed successfully!"

addDirToDir :: Dir -> String -> Dir
addDirToDir srcDir@Dir {..} newDir = changeDirDirs srcDir (newDir : dirDirs)

-- | Remove dir from dir 
removeDirFromDir :: Dir -> String -> Dir
removeDirFromDir srcDir@Dir {..} dirToRemove =
  changeDirDirs srcDir (filter (/= dirToRemove) dirDirs)

-- | Changes dirs in dir 
changeDirDirs :: Dir -> [FilePath] -> Dir
changeDirDirs Dir {..} newDirs =
  Dir dirName dirPath dirFiles newDirs dirPermissions dirSize

-- | Write contents to File 
writeToFile :: String -> String -> FileSystem (Either String String)
writeToFile name text = do
  FS {..} <- get
  maybeExists <- findFileInDirExact curDir name
  case maybeExists of
    Nothing -> return $ Left "No such file in directory!"
    Just file -> do
      let newFile = changeFileContent file (BSU.fromString text)
      updateCurDir $ replaceFileInDir curDir newFile
      return $ Right "Written to file successfully!"
  where
    changeFileContent :: File -> S.ByteString -> File
    changeFileContent File {..} content =
      File
        fileName
        filePath
        (toInteger $ S.length content)
        content
        fileModificationTime
        filePermissions

-- | Find file in directory and subdirectories 
findFilesRecursive :: String -> FileSystem [Message]
findFilesRecursive name = do
  FS {..} <- get
  files <- findRecursive curDir
  return $ map SimpleMessage files
  where
    findRecursive :: Dir -> FileSystem [FilePath]
    findRecursive dir@Dir {..} = do
      dirs <- getDirDirs dir
      innerDirsFiles <- mapM findRecursive dirs
      return $ findFilesInDirectory dir ++ concat innerDirsFiles
    findFilesInDirectory :: Dir -> [FilePath]
    findFilesInDirectory Dir {..} = map getFilePath $ filter isOkFile dirFiles
    isOkFile :: File -> Bool
    isOkFile File {..} = name `isInfixOf` fileName

-- | Change directory 
changeDirectory :: String -> FileSystem [Message]
changeDirectory directory = do
  let steps = splitOn [pathDelimiter] directory
  fs <- get
  case tryMakeSteps fs steps of
    Nothing -> return [errorMsg "Incorrect directory!"]
    Just newFs -> do
      put newFs
      return [SimpleMessage ""]
  where
    tryMakeSteps :: FS -> [String] -> Maybe FS
    tryMakeSteps curFs [] = Just curFs
    tryMakeSteps curFs [step] = tryMakeStep curFs step
    tryMakeSteps curFs (step:stepsLeft) =
      case tryMakeStep curFs step of
        Nothing    -> Nothing
        Just newFs -> tryMakeSteps newFs stepsLeft
    tryMakeStep :: FS -> String -> Maybe FS
    tryMakeStep curFs@FS {..} step =
      let curDirPath = getDirPath curDir
       in case step of
            "." -> Just curFs
            ".." ->
              case getParentDirPath curDirPath of
                Nothing -> Nothing
                Just path ->
                  case getDirInFS path curFs of
                    Nothing  -> Nothing
                    Just dir -> Just $ changeFsCurDir curFs dir
            _ ->
              let dirPath = getDirChildPath curDir step
               in case getDirInFS dirPath curFs of
                    Nothing  -> Nothing
                    Just dir -> Just $ changeFsCurDir curFs dir
    changeFsCurDir :: FS -> Dir -> FS
    changeFsCurDir FS {..} dir = FS fsPath fsInitTime dir dirsMap vcsMap
