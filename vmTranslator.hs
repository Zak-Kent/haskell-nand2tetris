import System.IO
import System.Environment
import System.FilePath.Posix(replaceExtensions)
import VMParser
import VMWriter

getDaArgs :: [String] -> String
getDaArgs [] = "da fuq"
getDaArgs (x:_) = x

main :: IO ()
main = do
  args <- getArgs
  let srcFile = getDaArgs args

  if srcFile == "da fuq" then do
    putStrLn "no src file arg"
  else do
    let dstFile = replaceExtensions srcFile "asm"

    contents <- fmap lines $ readFile srcFile
    let output = translateFile $ parseLines contents

    outFile <- openFile dstFile WriteMode
    hPutStr outFile output
    hClose outFile
