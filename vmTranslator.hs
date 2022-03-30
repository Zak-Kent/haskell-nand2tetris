import System.IO
import System.Environment
import System.FilePath.Posix(replaceExtensions, takeBaseName, takeExtensions)
import System.Directory
import VMParser
import VMWriter


main :: IO ()
main = do
  -- No error handling for missing file args or checks to prevent head on []
  -- Assumes the Coursera grader is calling with correct arguments
  s <- getArgs
  let src = head s
      readAndTranslate f = do
        contents <- fmap lines $ readFile f
        return $ translateFile (takeBaseName f) (parseLines contents)
      callSysInit = translateFile "init" $ parseLines ["call Sys.init 0"]
      setSPto256 = [(unlines ["@256", "D=A", "@SP", "M=D"])]
      isVmFile = (\f -> ".vm" == (takeExtensions f))
  isDir <- doesDirectoryExist src

  if isDir == True then do
    -- TODO takeBaseName doesn't work with a trailing slash, make sure this isn't an issue
    let dst = src ++ "/" ++ (takeBaseName src) ++ ".asm"
    files <- listDirectory src
    output <- mapM readAndTranslate
      $ map (\f -> src ++ "/" ++ f)
      $ filter isVmFile files -- needed to filter out the non .vm files
    outFile <- openFile dst WriteMode
    hPutStr outFile $ concat $  setSPto256 ++ [callSysInit] ++ output
    hClose outFile
  else do
    let dst = replaceExtensions src "asm"
    output <- readAndTranslate src
    outFile <- openFile dst WriteMode
    hPutStr outFile $ output ++ (unlines endProg)
    hClose outFile
