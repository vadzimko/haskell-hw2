{-# LANGUAGE RecordWildCards #-}

module Commands
  ( ls
  , cd
  , unknownCommand
  , cat
  , fileInfo
  , dirInfo
  , touch
  , remFile
  , mkDir
  , remDir
  , write
  , find
  ) where

import           CommandsImpl
import           Control.Monad.Trans.State (get)
import           Structures
import           System.Console.ANSI

-- | Unknown command info 
unknownCommand :: [String] -> FileSystem [Message]
unknownCommand _ = return [errorMsg "Unknown command. Use \"help\" command"]

-- | List files and directories 
ls :: [String] -> FileSystem [Message]
ls _ = do
  FS {..} <- get
  (dirs, files) <- getDirContent curDir
  return [ColoredMessage Blue (unwords dirs), SimpleMessage (unwords files)]

-- | Get contents of file 
cat :: [String] -> FileSystem [Message]
cat args =
  case args of
    (name:_) -> do
      FS {..} <- get
      maybeFile <- findFileInDirExact curDir name
      case maybeFile of
        Nothing        -> return [errorMsg "No such file in directory"]
        Just File {..} -> return [SimpleMessage $ show fileContent]
    _ -> return [errorMsg "Usage: cat <file_name>"]

-- | Get file info 
fileInfo :: [String] -> FileSystem [Message]
fileInfo args =
  case args of
    (fileName:_) -> do
      FS {..} <- get
      maybeFile <- findFileInDirExact curDir fileName
      case maybeFile of
        Nothing   -> return [errorMsg "No such file in directory"]
        Just file -> return (fileDescriptionToMessages file)
    _ -> return [errorMsg "Usage: finfo <file_name>"]

-- | Get directory info 
dirInfo :: [String] -> FileSystem [Message]
dirInfo args = do
  FS {..} <- get
  case args of
    (dirName:_) -> do
      maybeDir <- findDirInDirExact curDir dirName
      case maybeDir of
        Nothing ->
          return [errorMsg "No such directory inside current directory"]
        Just dir -> return $ dirDescriptionToMessages dir
    _ -> return $ dirDescriptionToMessages curDir

-- | Create new file
touch :: [String] -> FileSystem [Message]
touch args =
  case args of
    (fileName:_) -> do
      commandRes <- createFileInCurDir fileName
      handleCommandResult commandRes
    _ -> return [errorMsg "Usage: touch <file_name>"]

-- | Remove file 
remFile :: [String] -> FileSystem [Message]
remFile args =
  case args of
    (fileName:_) -> do
      commandRes <- removeFileFromCurDir fileName
      handleCommandResult commandRes
    _ -> return [errorMsg "Usage: rmf <file_name>"]

-- | Create new dir
mkDir :: [String] -> FileSystem [Message]
mkDir args =
  case args of
    (dirName:_) -> do
      commandRes <- createDirInCurDir dirName
      handleCommandResult commandRes
    _ -> return [errorMsg "Usage: mkdir <dir_name>"]

-- | Remove dir 
remDir :: [String] -> FileSystem [Message]
remDir args =
  case args of
    (dirName:_) -> do
      commandRes <- removeDirFromCurDir dirName
      handleCommandResult commandRes
    _ -> return [errorMsg "Usage: rm <dir_name>"]

-- | Convert result message to info message or error
handleCommandResult :: Either String String -> FileSystem [Message]
handleCommandResult commandRes =
  case commandRes of
    Left err   -> return [errorMsg err]
    Right info -> return [errorMsg info]

-- | Write contents to File 
write :: [String] -> FileSystem [Message]
write args =
  case args of
    (fileName:content:_) -> do
      commandRes <- writeToFile fileName content
      handleCommandResult commandRes
    _ -> return [errorMsg "Usage: write <file_name> <content>"]

-- | Find file in directory and subdirectories 
find :: [String] -> FileSystem [Message]
find args =
  case args of
    (fileName:_) -> findFilesRecursive fileName
    _            -> return [errorMsg "Usage: find <file_name>"]

-- | Change directory 
cd :: [String] -> FileSystem [Message]
cd args =
  case args of
    (directory:_) -> changeDirectory directory
    _             -> return [errorMsg "Usage: cd <dir>"]
