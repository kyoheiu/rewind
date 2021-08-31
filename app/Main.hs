{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Process (callCommand)
import Text.Read (readMaybe)

pkgPath :: FilePath
pkgPath = "/var/cache/pacman/pkg"

help =
  "A CLI tool to downgrade packages using local Pacman cache.\n\
  \`rewind package-name` will do it.\n\
  \`rewind name1 name2...` will also work."

message = "Which package do you want to use?(Enter a number)"

order = ["1) ", "2) ", "3) ", "4) ", "5) "]

findPkg :: String -> T.Text -> Bool
findPkg needle heystack = T.isPrefixOf needle' heystack && not (T.isSuffixOf ".sig" heystack)
  where
    needle' = T.pack needle

listUp :: [String] -> [T.Text] -> [(String, [T.Text])]
listUp as pkgs = map (\a -> (a, filter (findPkg a) pkgs)) as

getNum :: Int -> IO Int
getNum n =
  getLine >>= \x ->
    let readInt x = readMaybe x :: Maybe Int
     in case readInt x of
          Just x' ->
            if x' <= n
              then return x'
              else putStrLn "Invalid number. Re-enter a number." >> getNum n
          Nothing -> putStrLn "Invalid input. Re-enter a number." >> getNum n

downGrade :: (String, [T.Text]) -> IO ()
downGrade result =
  if null (snd result)
    then putStrLn $ fst result ++ ": No such package."
    else
      let ordered = zipWith T.append order result'
       in putStrLn message >> mapM_ TIO.putStrLn ordered >> getNum (length ordered) >>= \c ->
            doCommand $ result' !! (c - 1)
  where
    result' = reverse . sort . snd $ result
    doCommand x =
      callCommand . T.unpack
        . T.append "sudo pacman -U /var/cache/pacman/pkg/"
        $ x

main :: IO ()
main =
  getArgs >>= \args ->
    case length args of
      0 -> putStrLn help
      _ ->
        listDirectory pkgPath >>= \pkgs ->
          let results = listUp args (map T.pack pkgs) in mapM_ downGrade results
