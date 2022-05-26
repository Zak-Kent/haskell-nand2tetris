import System.IO
import System.Environment
import System.Directory
import System.FilePath.Posix(replaceExtensions, takeBaseName, takeExtensions)
import qualified Data.Map as M
import qualified Control.Monad.State as S

import Parser
import CodeGen
import AST

gatherFiles :: FilePath -> IO [FilePath]
gatherFiles src = do
  isDir <- doesDirectoryExist src
  if isDir == True then do
    files <- listDirectory src
    return $ map (\f -> src ++ "/" ++ f)
           $ filter (\f -> ".jack" == (takeExtensions f)) files
  else do
    return [src]

parseFile :: FilePath -> IO (Either String Class)
parseFile f = do
  case parseJack f of
    (Right p) -> return (Right p)
    (Left e) -> do
      print e
      return (Left "parse error")

writeVM :: FilePath -> String ->  IO ()
writeVM src vm = do
  outFile <- openFile dst WriteMode
  hPutStr outFile vm
  hClose outFile
  where -- dst = replaceExtensions src "vm"
        -- for local testing so xml compare files aren't overwritten
        dst = "./" ++ (takeBaseName src) ++ ".vm"

convertVM :: FilePath -> IO ()
convertVM src = do
  contents <- readFile src
  p <- parseFile contents
  case p of
    (Right c) -> writeVM src $ S.evalState (genVM c) (M.empty, M.empty, 0, 0, "")
    (Left e) -> print e

main :: IO [()]
main = do
  fileOrDir <- getArgs
  targets <- gatherFiles $ head fileOrDir
  mapM convertVM targets


