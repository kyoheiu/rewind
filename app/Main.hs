{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory
import System.Environment
import qualified Data.Text as T
import Data.List
import System.Process

pkgPath :: FilePath
pkgPath = "/var/cache/pacman/pkg"

findPkg :: [String] -> T.Text -> Bool
findPkg needle heystack = T.isPrefixOf needle' heystack && not (T.isSuffixOf ".sig" heystack)
  where needle' = T.pack . head $ needle

main :: IO ()
main = getArgs >>= \arg ->
  case length arg of
    0 -> putStrLn "No args."
    1 -> listDirectory pkgPath >>= \pkgs ->
      case length pkgs of
        0 -> putStrLn "No such package."
        1 -> putStrLn "No old packages."
        _ -> callCommand . T.unpack
                         . T.append "sudo pacman -U /var/cache/pacman/pkg/" 
                         . head . tail . reverse
                         . sort $ filter (findPkg arg) (map T.pack pkgs)
    _ -> putStrLn "Too many args."
