module Lib
  ( fileManager
  ) where

import           Commands
import           Control.Exception.Base    (try)
import           Control.Monad.Trans.State (runStateT)
import           Data.Maybe                (fromMaybe)
import           Parser                    (parseArguments)
import           PrintUtils
import           Structures
import           System.Directory          (canonicalizePath,
                                            createDirectoryIfMissing,
                                            doesDirectoryExist,
                                            getCurrentDirectory,
                                            setCurrentDirectory)
import           VCS                       (vcsHandler)

-- | Launches file manager, loads fs part to memory and operates with commands 
fileManager :: IO ()
fileManager = do
  let testDir = "test_folder"
  createDirectoryIfMissing True testDir
  setCurrentDirectory testDir
  currentDirectory <- getCurrentDirectory
  fs <- readFS currentDirectory
  nextCommand fs
  where
    nextCommand :: FS -> IO ()
    nextCommand fs = do
      command <- readInputLine $ getFSCurDirPath fs
      let (cmd:args) = getArgs command
      eitherRes <- try (executeNextCommand fs cmd args) :: IO (Either IOError ())
      case eitherRes of
        Left err -> finishCommand fs [errorMsg $ "Error: " ++ show err]
        Right _ -> return ()
    executeNextCommand :: FS -> String -> [String] -> IO ()
    executeNextCommand fs cmd args = 
      case cmd of
        "mv-fs" -> do
          (newFs, messages) <- moveFS fs args
          finishCommand newFs messages
        "q" -> do
          _ <- runStateT sync fs
          putStrLn "Execution Finished!"
        _ -> do
          (messages, newFs) <- runStateT (executeCommand cmd args) fs
          finishCommand newFs messages
    finishCommand :: FS -> [Message] -> IO ()
    finishCommand fs messages = do
      printResultMessages messages
      nextCommand fs

-- | Available supported commands description 
help :: [String] -> FileSystem [Message]
help _ =
  return
    [ SimpleMessage "help -- show commands descriptions"
    , SimpleMessage "move-fs -- move fs"
    , SimpleMessage "cd <dir> -- change directory"
    , SimpleMessage "q -- save FS state and quit"
    , SimpleMessage "ls [dir] -- show dir content"
    , SimpleMessage "cat <file_name> -- show file content"
    , SimpleMessage "touch <file_name> -- create empty file"
    , SimpleMessage "rmf <file_name> -- remove file"
    , SimpleMessage "mkdir <dir_name> -- create directory"
    , SimpleMessage "rm <dir_name> -- remove directory"
    , SimpleMessage "write <file_name> <content> -- write content to file"
    , SimpleMessage "find <file_name> -- search for file in dir and subdirs"
    , SimpleMessage "finfo <file_name> -- file info"
    , SimpleMessage "dinfo [dir_name] -- directory info"
    , SimpleMessage "git init -- init VCS in current dir"
    , SimpleMessage "git add <file1> <file2> ... - track files"
    , SimpleMessage "git status -- show tracked modified files"
    , SimpleMessage "git commit <message> - add tracked files revision"
    , SimpleMessage "git hist -- show all revisions"
    , SimpleMessage "todo: git show <file> <revision_num> - "
    , SimpleMessage "todo: git merge <file> <revision_num> - "
    , SimpleMessage "todo: git reset <file> <revision_num> - "
    , SimpleMessage "todo: git ignore <file> - "
    ]

-- | Run command 
executeCommand :: String -> ([String] -> FileSystem [Message])
executeCommand commandName =
  case commandName of
    "help"  -> help
    "cd"    -> cd
    "ls"    -> ls
    "cat"   -> cat
    "finfo" -> fileInfo
    "dinfo" -> dirInfo
    "touch" -> touch
    "rmf"   -> remFile
    "mkdir" -> mkDir
    "rm"    -> remDir
    "write" -> write
    "find"  -> find
    "git"   -> vcsHandler
    _       -> unknownCommand

-- | Get arguments from input line 
getArgs :: String -> [String]
getArgs str =
  let args = parseArguments str
   in fromMaybe [""] args

-- | Sync FS with File System and init new fs on given path
moveFS :: FS -> [String] -> IO (FS, [Message])
moveFS fs args =
  case args of
    (path:_) -> do
      newPath <- canonicalizePath path
      exists <- doesDirectoryExist newPath
      if exists
        then do
          _ <- runStateT sync fs
          setCurrentDirectory newPath
          newFs <- readFS newPath
          return (newFs, [])
        else return (fs, [errorMsg "Such directory doesn't exist!"])
    _ -> return (fs, [errorMsg "Usage: move-fs <dir_name>"])
