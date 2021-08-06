{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List
import System.Process

pkgPath :: FilePath
pkgPath = "/var/cache/pacman/pkg"

findPkg :: String -> T.Text -> Bool
findPkg needle heystack = T.isPrefixOf needle' heystack && not (T.isSuffixOf ".sig" heystack)
  where needle' = T.pack needle

listUp :: [String] -> [FilePath] -> [[T.Text]]
listUp [] pkgs = []
listUp (a:as) pkgs = filter (findPkg a) pkgs' : listUp as pkgs
  where pkgs' = map T.pack pkgs

downGrade :: [T.Text] -> IO ()
downGrade result =
  if null result then putStrLn "No such package."
  else
    let ordered = zipWith T.append ["1) ","2) ","3) "] . reverse . sort $ result in
    mapM_ TIO.putStrLn ordered >> readLn >>= \c ->
      case c of
        1 -> doCommand $ result'!!0
        2 -> doCommand $ result'!!1
        3 -> doCommand $ result'!!2
        _   -> putStrLn "Please choose the number."
        where result' = reverse . sort $ result
              doCommand x = callCommand . T.unpack 
                          . T.append "sudo pacman -U /var/cache/pacman/pkg/" $ x

main :: IO ()
main = getArgs >>= \args ->
  case length args of
    0 -> putStrLn "No args."
    _ -> listDirectory pkgPath >>= \pkgs ->
      let results = listUp args pkgs in
      case length results of
        0 -> putStrLn "No such package."
        _ -> mapM_ downGrade results
