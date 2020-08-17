{-# LANGUAGE RecordWildCards #-}

module Structures
  ( File(..)
  , Dir(..)
  , FS(..)
  , Message(..)
  , getDirName
  , getDirPath
  , getFileName
  , getFilePath
  , getFileNames
  , getDirContent
  , getDir
  , getDirInFS
  , getFSCurDirPath
  , getCurDir
  , getCurDirPath
  , getDirDirs
  , getDirFiles
  , getDirChildPath
  , getChildPath
  , getParentDirPath
  , errorMsg
  , getPathView
  , findFileInDirExact
  , findFileInFSExact
  , findDirInDirExact
  , findDirInFSExact
  , fileDescriptionToMessages
  , dirDescriptionToMessages
  , getDirSize
  , pathToName
  , getRealPath
  , getFakePath
  , getFileSizeFromInfo
  , FileSystem
  , pathDelimiter
  , splitLast
  , VcsDir(..)
  , VcsFileRevision(..)
  ) where

import           Control.Monad.Trans.State (StateT, get)
import qualified Data.ByteString           as S
import           Data.Foldable             (find)
import qualified Data.HashMap              as HS
import           Data.Maybe                (mapMaybe)
import           Data.Time.Clock           (UTCTime)
import           System.Console.ANSI
import           System.Directory          (Permissions)
import qualified Data.Map                   as Map

type FileSystem a = StateT FS IO a

-- | File data structure
data File =
  File
    { fileName             :: String
    , filePath             :: FilePath
    , fileSize             :: Integer
    , fileContent          :: S.ByteString
    , fileModificationTime :: UTCTime
    , filePermissions      :: Permissions
    }
  deriving (Show, Eq)

-- | Directory data structure
data Dir =
  Dir
    { dirName        :: String
    , dirPath        :: FilePath
    , dirFiles       :: [File]
    , dirDirs        :: [FilePath]
    , dirPermissions :: Permissions
    , dirSize        :: Integer
    }
  deriving (Show, Eq)

-- | File System data structure
data FS =
  FS
    { fsPath     :: String
    , fsInitTime :: UTCTime
    , curDir     :: Dir
    , dirsMap    :: HS.Map FilePath Dir
    , vcsMap     :: HS.Map FilePath VcsDir
    }
  deriving (Show)

-- | Message data structure
data Message
  = ColoredMessage
      { color :: Color
      , text  :: String
      }
  | SimpleMessage
      { text :: String
      }
  deriving (Show)

-- | Path delimiter in system
pathDelimiter :: Char
pathDelimiter = '/'

-- | Path of root directory
rootDirectoryPath :: String
rootDirectoryPath = "."

-- | Get dir name from structure
getDirName :: Dir -> String
getDirName Dir {..} = dirName

-- | Get dir size from structure
getDirSize :: Dir -> Integer
getDirSize Dir {..} = dirSize

-- | Get dir path from structure
getDirPath :: Dir -> FilePath
getDirPath Dir {..} = dirPath

-- | Get file name from structure
getFileName :: File -> String
getFileName File {..} = fileName

-- | Get file path from structure
getFilePath :: File -> FilePath
getFilePath File {..} = filePath

-- | Get file size from structure
getFileSizeFromInfo :: File -> Integer
getFileSizeFromInfo File {..} = fileSize

-- | Get files names from structures
getFileNames :: [File] -> [String]
getFileNames = map getFileName

-- | Get dir files
getDirFiles :: Dir -> [File]
getDirFiles Dir {..} = dirFiles

-- | Get dir child directories
getDirDirs :: Dir -> FileSystem [Dir]
getDirDirs Dir {..} = do
  FS {..} <- get
  return $
    mapMaybe ((`HS.lookup` dirsMap) . getChildPath (getDirPath curDir)) dirDirs

-- | Get dir content
getDirContent :: Dir -> FileSystem ([String], [String])
getDirContent Dir {..} = return (dirDirs, map getFileName dirFiles)

-- | Find file in FS with exact name
findFileInFSExact :: String -> FileSystem (Maybe File)
findFileInFSExact fileName = do
  FS {..} <- get
  findFileInDirExact curDir fileName

-- | Find file in dir with exact name
findFileInDirExact :: Dir -> String -> FileSystem (Maybe File)
findFileInDirExact Dir {..} name =
  return $ find (\f -> getFileName f == name) dirFiles

-- | Get dir by it's path
getDir :: FilePath -> FileSystem (Maybe Dir)
getDir dirPath = do
  FS {..} <- get
  return $ HS.lookup dirPath dirsMap

-- | Get dirs in fs with exact name
getDirInFS :: FilePath -> FS -> Maybe Dir
getDirInFS dirPath FS {..} = HS.lookup dirPath dirsMap

-- | View of path for prompt
getPathView :: FilePath -> String
getPathView path =
  if path == rootDirectoryPath
    then "/"
    else path

-- | Find dir in FS with exact name
findDirInFSExact :: String -> FileSystem (Maybe Dir)
findDirInFSExact dirName = do
  curDir <- getCurDir
  findDirInDirExact curDir dirName

-- | Find dir in dir with exact name
findDirInDirExact :: Dir -> String -> FileSystem (Maybe Dir)
findDirInDirExact dir name = getDir $ getDirChildPath dir name

-- | Get current dir
getCurDir :: FileSystem Dir
getCurDir = do
  FS {..} <- get
  return curDir

-- | Get path of current dir
getCurDirPath :: FileSystem FilePath
getCurDirPath = do
  Dir {..} <- getCurDir
  return dirPath

-- | Get path of current dir
getFSCurDirPath :: FS -> FilePath
getFSCurDirPath FS {..} = getDirPath curDir

-- | Get parent dir path
getParentDirPath :: FilePath -> Maybe FilePath
getParentDirPath path =
  case splitLast pathDelimiter path of
    Left _ -> Nothing
    Right (parentDir, _) ->
      if parentDir == ""
        then Just rootDirectoryPath
        else Just parentDir

-- | Get child file or dir path by dir
getDirChildPath :: Dir -> String -> FilePath
getDirChildPath Dir {..} = getChildPath dirPath

-- | Get child file or dir path
getChildPath :: FilePath -> String -> FilePath
getChildPath dirPath childName =
  if dirPath == rootDirectoryPath
    then pathDelimiter : childName
    else dirPath ++ [pathDelimiter] ++ childName

-- | Get name by path
pathToName :: String -> String
pathToName path =
  case splitLast pathDelimiter path of
    Left name       -> name
    Right (_, name) -> name

-- | Get real FS pat 
getRealPath :: FilePath -> String
getRealPath path =
  if path == rootDirectoryPath
    then rootDirectoryPath ++ [pathDelimiter]
    else tail path

-- | Convert real path to FS path
getFakePath :: FilePath -> FilePath
getFakePath relativePath =
  if relativePath == rootDirectoryPath
    then rootDirectoryPath
    else pathDelimiter : relativePath

-- | Constructs error message of red color
errorMsg :: String -> Message
errorMsg = ColoredMessage Red

-- | Convert file to messages to show info
fileDescriptionToMessages :: File -> [Message]
fileDescriptionToMessages File {..} =
  [ SimpleMessage $ "Path: " ++ filePath
  , SimpleMessage $ "Size: " ++ show fileSize
  , SimpleMessage $ "Type: " ++ getFileExtension
  , SimpleMessage $ "Modification Time: " ++ show fileModificationTime
  , SimpleMessage $ "Permissions: " ++ show filePermissions
  ]
  where
    getFileExtension :: String
    getFileExtension =
      case splitLast '.' filePath of
        Left _               -> "unknown"
        Right (_, extension) -> extension

-- | Split by last entry
splitLast :: Eq a => a -> [a] -> Either [a] ([a], [a])
splitLast c' = foldr go (Left [])
  where
    go c (Right (f, b)) = Right (c : f, b)
    go c (Left s)
      | c' == c = Right ([], s)
      | otherwise = Left (c : s)

-- | Convert directory to messages to show info
dirDescriptionToMessages :: Dir -> [Message]
dirDescriptionToMessages Dir {..} =
  [ SimpleMessage $ "Path: " ++ dirPath
  , SimpleMessage $ "Files amount: " ++ show (length dirFiles)
  , SimpleMessage $ "Size: " ++ show dirSize ++ " bytes"
  , SimpleMessage $ "Permissions: " ++ show dirPermissions
  ]

-- | Vcs directory data structure
data VcsDir =
  VcsDir
    { vcsRoot      :: FilePath
    , vcsTracked   :: Map.Map FilePath S.ByteString
    , vcsRevisions :: HS.Map FilePath [VcsFileRevision]
    , vcsCommits   :: [String]
    }
  deriving (Show)

-- | Vcs file revision data structure
data VcsFileRevision =
  VcsFileRevision
    { vcsPath           :: FilePath
    , vcsContent        :: S.ByteString
    , vcsRevisionNumber :: Int
    }
  deriving (Show, Eq)
