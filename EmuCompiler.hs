import System.IO
import System.Environment

outputFile sourcefilename = do
  sfHandle <- openFile sourcefilename ReadMode
  putStrLn "Hello, World!"
  sfContents <- hGetContents sfHandle
  print sfContents

main = do
  args <- getArgs
  if (length args) > 0
    then outputFile (args !! 0)
    else putStrLn "This is a Compiler For K's Emulator"
