{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sort)
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Process (callCommand)
import Text.Read (readMaybe)

pkgPath :: FilePath
pkgPath = "/var/cache/pacman/pkg"

fstMessage = "Which package do you want to use?(Enter a number)"

order = ["1) ","2) ","3) ","4) ","5) "]

findPkg :: String -> T.Text -> Bool
findPkg needle heystack = T.isPrefixOf needle' heystack && not (T.isSuffixOf ".sig" heystack)
  where needle' = T.pack needle

listUp :: [String] -> [T.Text] -> [(String,[T.Text])]
listUp [] pkgs = []
listUp as pkgs = map (\a -> (a, filter (findPkg a) pkgs)) as

readInt x = readMaybe x :: Maybe Int

getNum :: Int -> IO Int
getNum n = getLine >>= \x ->
  case readInt x of
    Just x' -> if x' <= n then return x'
              else putStrLn "Invalid number. Re-enter a number." >> getNum n
    Nothing -> putStrLn "Invalid input. Re-enter a number." >> getNum n

downGrade :: (String, [T.Text]) -> IO ()
downGrade result =
  if null (snd result) then putStrLn $ fst result ++ ": No such package." 
  else
    let ordered = zipWith T.append order result' in
    putStrLn fstMessage >> mapM_ TIO.putStrLn ordered >> getNum (length ordered) >>= \c ->
      case c of
        1 -> doCommand $ result'!!0
        2 -> doCommand $ result'!!1
        3 -> doCommand $ result'!!2
        4 -> doCommand $ result'!!3
        5 -> doCommand $ result'!!4
        _ -> putStrLn "Please Enter a number."
        where result' = reverse . sort . snd $ result
              doCommand x = callCommand . T.unpack 
                          . T.append "sudo pacman -U /var/cache/pacman/pkg/" $ x

main :: IO ()
main = getArgs >>= \args ->
  case length args of
    0 -> putStrLn "No args."
    _ -> listDirectory pkgPath >>= \pkgs ->
      let results = listUp args (map T.pack pkgs) in mapM_ downGrade results
