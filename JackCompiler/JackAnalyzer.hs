import System.IO
import System.Environment
import System.Directory
import System.FilePath.Posix(replaceExtensions, takeBaseName, takeExtensions)

import Parser
import XMLGen
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

writeXML :: FilePath -> String ->  IO ()
writeXML src xml = do
  outFile <- openFile dst WriteMode
  hPutStr outFile xml
  hClose outFile
  where dst = replaceExtensions src "xml"
        -- for local testing so xml compare files aren't overwritten
        -- dst = "./" ++ (takeBaseName src) ++ ".xml"

convertXML :: FilePath -> IO ()
convertXML src = do
  contents <- readFile src
  p <- parseFile contents
  case p of
    (Right c) -> writeXML src $ xClass c
    (Left e) -> print e

main :: IO [()]
main = do
  fileOrDir <- getArgs
  targets <- gatherFiles $ head fileOrDir
  mapM convertXML targets
