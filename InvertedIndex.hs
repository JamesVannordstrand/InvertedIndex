--James Vannordstrand
--Information Storage and Retrieval
--Inverted Index part 2 
module Main where
import Data.Char
import System.Environment
import Data.List
import Control.Monad
import System.Directory

type Position = Int
type DocNumber = Int
type DocFrequency = Int
type Term = String
type Word = (Int, String)
type WordPositions = (DocNumber, [Position])
type SingleWordInfo = (Term, [WordPositions])
type AllWordInfo = [SingleWordInfo]

getWordIndex :: String -> (Int, [Word]) -> WordPositions
getWordIndex _    (i, [])   = (i, [])
getWordIndex term (i, x:xs) 
  | snd x /= term = getWordIndex term (i, xs)
  | otherwise     = (i, fst x : snd (getWordIndex term (i, xs)))

handleFiles :: [[Word]] -> String -> SingleWordInfo
handleFiles wordsWithIndex term = do
  let wordPositions = map (getWordIndex term) $ zip [1..] wordsWithIndex 
  (term, wordPositions)

getOccurences :: [[String]] -> [String] -> AllWordInfo                         
getOccurences fileWords dictionary = do                                                    
  let wordsWithIndex = map (zip [1..]) fileWords                                                           
  map (handleFiles wordsWithIndex) dictionary
 
writeOccurences :: [WordPositions] -> String
writeOccurences []            = ""
writeOccurences ((_, []): xs) = writeOccurences xs
writeOccurences ((doc, pos):xs)   = show doc ++ ", " ++ show (length pos) ++ ":" ++ show pos ++ "\n" ++ writeOccurences xs

writeBody :: AllWordInfo -> IO()
writeBody [] = return ()
writeBody ((a, c):xs) = do
  let repl char 
        | char `elem` "[]\"()," = ' '
        | otherwise             = char
  appendFile "document.pidx" . map repl $ a ++ ", " ++ (show . length $ filter (\x -> length (snd x) > 0) c) ++ ":\n" ++ writeOccurences c ++ "\n"
  writeBody xs
 
writeLegend :: [Word] -> IO()
writeLegend []     = return ()
writeLegend (x:xs) = do
  appendFile "document.pidx" $ show (fst x) ++ "\t" ++ snd x ++ "\n"
  writeLegend xs

invertedIndex :: [String] -> IO()
invertedIndex files = do
  fileWords  <- mapM (\file -> readFile file >>= return . map (removePunc . map toLower) . words) files                                     
  writeFile "document.pidx" "# INPUT DOCUMENT REFERENCE LEGEND\n"
  writeLegend $ zip [1..] files
  appendFile "document.pidx" "# INVERTED INDEX RESULTS\n"
  writeBody $ getOccurences fileWords (sort . nub $ concat fileWords)
  where removePunc = filter (`notElem` "()[]$,.?!–-;\'\"")

main :: IO()
main = getArgs >>= \args -> allFilesPresent args >>= check args 
  where allFilesPresent files = liftM and $ mapM doesFileExist files
        check args exists 
          | exists    = invertedIndex args 
          | otherwise = putStrLn "All the files specified don't exist."