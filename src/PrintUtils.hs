{-# LANGUAGE RecordWildCards #-}

module PrintUtils
  ( readInputLine
  , printResultMessages
  , sync
  , readFS
  , prettyPrint
  , pathToVcsPath
  ) where

import           Control.Monad             (filterM, when)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State (get)
import qualified Data.ByteString           as S
import qualified Data.ByteString.UTF8      as BSU
import           Data.Char                 (isSpace)
import           Data.Foldable             (foldrM, forM_, traverse_)
import qualified Data.HashMap              as HS
import           Data.List                 (intercalate)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe)
import           Data.Time.Clock           (getCurrentTime)
import           Structures
import           System.Console.ANSI
import           System.Directory          (canonicalizePath,
                                            createDirectoryIfMissing,
                                            doesDirectoryExist, doesFileExist,
                                            getFileSize, getModificationTime,
                                            getPermissions, listDirectory,
                                            makeRelativeToCurrentDirectory,
                                            removeDirectoryRecursive,
                                            removeFile)
import           System.IO                 (hFlush, stdout)
import           Text.Read                 (readMaybe)
import           Text.Show.Pretty          (ppShow)
import Data.List.Split (splitOn)

prettyPrint :: Show a => a -> IO ()
prettyPrint = putStrLn . ppShow

-- | Read user input with colored prompt (which is simulating current directory)
readInputLine :: FilePath -> IO String
readInputLine curDir = do
  setSGR [SetColor Foreground Vivid Green]
  putStr $ getPathView curDir ++ "> "
  setSGR [Reset]
  hFlush stdout
  getLine

-- | Print messages (command results) to stdout
-- prints only not empty messages
-- messages might be colored
printResultMessages :: [Message] -> IO ()
printResultMessages = mapM_ printMessage
  where
    needPrintMessage :: String -> Bool
    needPrintMessage msg = not $ all isSpace msg
    printMessage :: Message -> IO ()
    printMessage ColoredMessage {..} =
      when (needPrintMessage text) $ do
        setSGR [SetColor Foreground Vivid color]
        putStrLn text
        setSGR [Reset]
        hFlush stdout
    printMessage SimpleMessage {..} =
      when (needPrintMessage text) $ do
        putStrLn text
        hFlush stdout

-- | Synchronize FS state with real world file system
-- removes all files in real file system which not present in FS
-- replaces all files content with content from given FS
sync :: FileSystem ()
sync = do
  FS {..} <- get
  liftIO $ putStrLn "Writing changes..."
  syncDir curDir
  liftIO $ putStrLn "Writing done!"
  printVCS
  where
    syncDir :: Dir -> FileSystem ()
    syncDir dir@Dir {..} = do
      realPath <- liftIO $ canonicalizePath (getRealPath dirPath)
      liftIO $ createDirectoryIfMissing False realPath
      (curDirs, curFiles) <- liftIO $ getPathElements realPath
      removeExtraObjects curFiles dir findFileInDirExact removeFile
      removeExtraObjects curDirs dir findDirInDirExact removeDirectoryRecursive
      liftIO $ writeFiles dir
      traverse_
        (\name -> do
           maybeDir <- getDir $ getChildPath dirPath name
           forM_ maybeDir syncDir)
        dirDirs

-- | Takes dir path and returns all it's not hidden directories and files
getPathElements :: FilePath -> IO ([String], [String])
getPathElements path = do
  list <- listDirectory path
  let pathList = map (getChildPath path) list
  curDirs <- filterM doesDirectoryExist pathList
  curFiles <- filterM doesFileExist pathList
  canonicalCurDirs <- mapM canonicalizePath curDirs
  canonicalCurFiles <- mapM canonicalizePath curFiles
  return (filterHidden canonicalCurDirs, filterHidden canonicalCurFiles)
  where
    filterHidden :: [String] -> [String]
    filterHidden = filter (not . isHidden . pathToName)
    isHidden :: String -> Bool
    isHidden str = head str == '.'

-- | Removes all files in real file system which not present in FS
removeExtraObjects ::
     [String]
  -> Dir
  -> (Dir -> String -> FileSystem (Maybe a))
  -> (FilePath -> IO ())
  -> FileSystem ()
removeExtraObjects objectNames dirDescription getObject removeObject =
  mapM_
    (\name -> do
       object <- getObject dirDescription name
       case object of
         Just _  -> return ()
         Nothing -> liftIO $ removeObject name)
    objectNames

-- | Write all directory files content to real file system
writeFiles :: Dir -> IO ()
writeFiles Dir {..} = mapM_ write dirFiles
  where
    write File {..} = do
      realPath <- liftIO $ canonicalizePath (getRealPath filePath)
      S.writeFile realPath fileContent

-- | Read FS from real file system
readFS :: FilePath -> IO FS
readFS path = do
  let dirsMap = HS.empty
  let vcsMap = HS.empty
  (dir, filledMap) <- readDirDescription path dirsMap
  time <- getCurrentTime
  let newFs = FS path time dir filledMap vcsMap
  readVcs newFs

--  return newFs
-- | Read directory info from real file system
readDirDescription ::
     FilePath -> HS.Map String Dir -> IO (Dir, HS.Map String Dir)
readDirDescription canonicalPath m = do
  relativePath <- makeRelativeToCurrentDirectory canonicalPath
  let fakePath = getFakePath relativePath
  (dirs, files) <- getPathElements canonicalPath
  filesDescriptions <- mapM readFileDescription files
  (dirsDescriptions, updatedMap) <- uniteDirsInfo ([], m) dirs
  permissions <- getPermissions canonicalPath
  let size = filesSizeSum filesDescriptions + dirsSizeSum dirsDescriptions
  let dirName = pathToName canonicalPath
  let dir =
        Dir
          dirName
          fakePath
          filesDescriptions
          (map getDirName dirsDescriptions)
          permissions
          size
  return (dir, HS.insert (getDirPath dir) dir updatedMap)
  where
    filesSizeSum :: [File] -> Integer
    filesSizeSum descriptions = sum $ map getFileSizeFromInfo descriptions
    dirsSizeSum :: [Dir] -> Integer
    dirsSizeSum descriptions = sum $ map getDirSize descriptions
    uniteDirsInfo ::
         ([Dir], HS.Map String Dir) -> [String] -> IO ([Dir], HS.Map String Dir)
    uniteDirsInfo acc [] = return acc
    uniteDirsInfo (dirs, t) [a] = do
      (newDir, updatedMap) <- readDirDescription a t
      return (newDir : dirs, updatedMap)
    uniteDirsInfo (dirs, t) (a:as) = do
      (newDir, updatedMap) <- readDirDescription a t
      uniteDirsInfo (newDir : dirs, updatedMap) as

-- | Read file info from real file system
readFileDescription :: FilePath -> IO File
readFileDescription canonicalPath = do
  relativePath <- makeRelativeToCurrentDirectory canonicalPath
  let fakePath = getFakePath relativePath
  size <- getFileSize canonicalPath
  content <- S.readFile canonicalPath
  modificationTime <- getModificationTime canonicalPath
  permissions <- getPermissions canonicalPath
  return $
    File
      (pathToName fakePath)
      fakePath
      size
      content
      modificationTime
      permissions

-- | Convert file name and its revision number to vcs name
fileNameToVCSName :: FilePath -> Int -> String
fileNameToVCSName name revision = name ++ "#" ++ show revision

-- | Convert vcs file name to fs name and its revision number
fileNameFromVCSName :: FilePath -> (FilePath, Int)
fileNameFromVCSName vcsName =
  case splitLast '#' vcsName of
    Left name              -> (name, 0)
    Right (name, revision) -> (name, fromMaybe 0 (readMaybe revision))

-- | VCS Folder name
vcsDirName :: String
vcsDirName = ".gut"

-- | File with commits
commitsFileName :: String
commitsFileName = ".commits"

-- | FS file path to its correspondent vcs file path
pathToVcsPath :: FilePath -> FilePath -> FilePath
pathToVcsPath p rootPath = up (down p []) (getChildPath rootPath vcsDirName)
  where
    down :: FilePath -> [String] -> [String]
    down path acc =
      if rootPath == path
        then acc
        else down
               (fromMaybe rootPath $ getParentDirPath path)
               (pathToName path : acc)
    up :: [String] -> FilePath -> FilePath
    up [] path     = path
    up [a] path    = getChildPath path a
    up (a:as) path = up as $ getChildPath path a

-- | Synchronize all VCS dirs with FS
printVCS :: FileSystem ()
printVCS = do
  FS {..} <- get
  let vcsDirs = HS.elems vcsMap
  mapM_ writeVcsDir vcsDirs

-- | Synchronize VCS folder with FS
writeVcsDir :: VcsDir -> FileSystem ()
writeVcsDir VcsDir {..} = do
  let vcsPath = pathToVcsPath vcsRoot vcsRoot
  realPath <- liftIO $ canonicalizePath (getRealPath vcsPath)
  doesExistDir <- liftIO $ doesDirectoryExist realPath
  when doesExistDir $ liftIO $ removeDirectoryRecursive realPath
  liftIO $ createDirectoryIfMissing True realPath
  writeRevisions vcsRevisions
  writeCommits vcsCommits
  where
    writeRevisions :: HS.Map FilePath [VcsFileRevision] -> FileSystem ()
    writeRevisions revisionsMap =
      mapM_ (mapM_ writeRevision) $ HS.elems revisionsMap
    writeRevision :: VcsFileRevision -> FileSystem ()
    writeRevision VcsFileRevision {..} = do
      let path =
            fileNameToVCSName (pathToVcsPath vcsPath vcsRoot) vcsRevisionNumber
      realPath <- liftIO $ canonicalizePath (getRealPath path)
      liftIO $
        createDirectoryIfMissing
          True
          (fromMaybe vcsRoot $ getParentDirPath realPath)
      liftIO $ S.writeFile realPath vcsContent
    writeCommits :: [String] -> FileSystem ()
    writeCommits commits = do
      let path = getChildPath vcsRoot commitsFileName
      realPath <- liftIO $ canonicalizePath (getRealPath path)
      liftIO $ S.writeFile realPath (BSU.fromString $ intercalate "\n" commits)

-- | Check if this is vcs root and try read VCS from vcs if true
readVcs :: FS -> IO FS
readVcs FS {..} = do
  maybeVcsDirs <- mapM readVcsDir (HS.elems dirsMap)
  let newVcsMap =
        foldr
          (\maybeDir acc ->
             case maybeDir of
               Just dir@VcsDir {..} -> HS.insert vcsRoot dir acc
               Nothing              -> acc)
          HS.empty
          maybeVcsDirs
  return $ FS fsPath fsInitTime curDir dirsMap newVcsMap
  where
    readVcsDir :: Dir -> IO (Maybe VcsDir)
    readVcsDir Dir {..} = do
      let vcsPath = getChildPath dirPath vcsDirName
      realPath <- liftIO $ canonicalizePath (getRealPath vcsPath)
      dirExists <- doesDirectoryExist realPath
      if not dirExists
        then return Nothing
        else do
          files <- readFiles HS.empty vcsPath
          commits <- readCommits dirPath
          return $ Just $ VcsDir dirPath Map.empty files commits
    readFiles ::
         HS.Map FilePath [VcsFileRevision]
      -> FilePath
      -> IO (HS.Map FilePath [VcsFileRevision])
    readFiles oldMap dirPath = do
      realPath <- canonicalizePath (getRealPath dirPath)
      (curDirs, curFiles) <- getPathElements realPath
      newMap <- insertFiles oldMap curFiles
      foldrM (flip readFiles) newMap curDirs
    insertFiles ::
         HS.Map FilePath [VcsFileRevision]
      -> [FilePath]
      -> IO (HS.Map FilePath [VcsFileRevision])
    insertFiles =
      foldrM
        (\canonicalPath acc -> do
           relativePath <- makeRelativeToCurrentDirectory canonicalPath
           let fakePath = getFakePath relativePath
           let (realName, revisionNumber) = fileNameFromVCSName fakePath
           content <- S.readFile canonicalPath
           let revision = VcsFileRevision realName content revisionNumber
           let oldRevisions = fromMaybe [] $ HS.lookup realName acc
           let newAcc = HS.insert realName (revision : oldRevisions) acc
           return newAcc)
    readCommits :: String -> IO [String]
    readCommits dirPath = do
      let filePath = getChildPath dirPath commitsFileName
      realPath <- canonicalizePath (getRealPath filePath)
      exists <- doesFileExist realPath
      if exists
        then do
          content <- S.readFile realPath
          return $ splitOn "\n" (BSU.toString content)
        else return []
