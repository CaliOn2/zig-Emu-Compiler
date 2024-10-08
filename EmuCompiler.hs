import System.IO
import System.Environment
import Data.List

chartoHex char = elemIndex char "0123456789abcdf" 
chartoBin char = elemIndex char "01"
chartoDec char = elemIndex char "0123456789"

numStrToNum charToNum power limit string  
  | (num <= limit) = num
  | otherwise = throw ("number size error " ++ num)
  where
    num = sum (map (\x -> (chartoNum fst) * (power ^ snd)) (zip (reverse string) [0..]))

decStrToNum limit string = numStrToNum chartoDec 10 limit string 

binStrToNum limit string = numStrToNum chartoBin 2 limit string

hexStrToNum limit string = numStrToNum chartoHex 16 limit string

addrResolv :: String -> Int
addrResolv addrStr
  | ((length addrStr) < 4) && (addrStr!!0 == '$') = decStrToNum 0b1111 (tail addrStr)
  | otherwise = throw ("address format error " : addrStr)

addressCommandfill params =
  sum (map (\x -> shiftL (12 - (snd x * 4)) (addrResolv (fst x))) (zip params [0..]))

-- Functions Below :: [String] -> Int 
orI params = addressCommandfill params + 0b0000
xorI params = addressCommandfill params + 0b0001 
andI params = addressCommandfill params + 0b0010
nandI params = addressCommandfill params + 0b0011
addI params = addressCommandfill params + 0b0100
subI params = addressCommandfill params + 0b0101
multI params = addressCommandfill params + 0b0110
divI params = addressCommandfill params + 0b0111
rootI params = addressCommandfill params + 0b1000
shiftLI params = addressCommandfill (init params) + shiftL 4 (decStrToNum 0b1111 (last params)) + 0b1001
shiftRI params = addressCommandfill (init params) + shiftL 4 (decStrToNum 0b1111 (last params)) + 0b1010
setI params =
  (shiftL 8 num) + shiftL 4 (addrResolv params!!1) + 0b1011 
  where 
    num = case (params!!0)!!1 of
      'x' -> hexStrToNum 0xff params!!0
      'b' -> binStrToNum 0xff params!!0
      'd' -> decStrToNum 0xff params!!0
      otherwise -> throw "badNumber in SetI" 

memI subOpcode params = addressCommandfill (init params) + shiftL 4 subOpcode + 0b1100
getProcI params = shiftL 8 (addrResolv params!!1) + shiftL 4 0b0011 + 0b1100
compI instruction = addressCommandfill params  + 0b1101
syO subOpcode instruction = shiftL 4 subOpcode + 0b1110
syI subOpcode instruction = shiftL 4 subOpcode + 0b1111

-- don't forget to fix the data here there are unclear type returns
hexData instruction = map (hexStrToNum 0xff) params
binData instruction = map (binStrToNum 0xff) params 
decData instruction =
  map (\x -> if (length x) < 5 
  then (binStrToNum 0xff x)
  else let p = (binStrToNum 0xffff x) in [(shiftR 8 p), ((.&.) 0xff p)]
  ) params

charData instruction = map letters params

oprandInterpret :: String -> [String] -> Int
oprandInterpret operator =
  case operator of
  "or" -> orI
  "xor" -> xorI
  "and" -> andI
  "nand" -> nandI
  "add" -> addI
  "sub" -> subI
  "mult" -> multI
  "div" -> divI
  "shftl" -> shiftLI
  "shftr" -> shiftRI
  "set" -> setI
  "load" -> memI 0b0000
  "store" -> memI 0b1111
  "proc" -> getProcI
  "comp" -> compI
  "syo" -> syO
  "syi" -> syI
  "x0" -> hexData
  "b0" -> binData
  "d0" -> decData
  "c0" -> charData
  otherwise -> throw ("badInstruction " ++ operator)


inputFile :: FilePath -> IO [[String]]
inputFile sourcefilename = do
  sfHandle <- openFile sourcefilename ReadMode
  putStrLn "Hello, World!"
  sfContents <- hGetContents sfHandle
  let sfLines = filter (\x -> x!!0 != '/' && x!!1 != '/' && length x != 0) (lines sfContents)
  let sfInstructions = map words sfLines 
  print sfInstructions 
  -- hClose sfHandle
  return sfInstructions

outputFile :: FilePath [[Char]] -> IO
outputFile outputfilename binaryinstructs = do
  ofHandle <- openFile outputfilename WriteMode

main = do
  args <- getArgs
  if (length args) > 0
    then map instructionInterpreter (inputFile (args !! 0))
    else putStrLn "This is a Compiler For K's Emulator"
