{-# LANGUAGE OverloadedStrings #-}
module Yesod.Test.Static
  ( checkStaleStatics
  , checkStaleStaticsWith
  , whiteList
  ) where

import           Control.Monad
import           Data.List
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Language.Haskell.TH
import           System.Directory

suffixes :: [String]
suffixes = [".hs",".cassius",".lucius",".julius",".hamlet"]

getStatics :: FilePath -> IO [(T.Text, FilePath)]
getStatics sdir = do
  fs <- getRecursiveFiles sdir
  let fs' = filter (not . isPrefixOf (sdir ++ "/tmp/")) fs
  return $ zip (map (T.pack . map replace . drop (1 + length sdir)) fs') fs'

replace :: Char -> Char
replace c
  | 'A' <= c && c <= 'Z' = c
  | 'a' <= c && c <= 'z' = c
  | '0' <= c && c <= '9' = c
  | otherwise = '_'

getRecursiveFiles :: FilePath -> IO [FilePath]
getRecursiveFiles d = do
  isDir <- doesDirectoryExist d
  if isDir
     then do fs <- getDirectoryContents d
             let fs' = filter (\f -> f /= "." && f /= ".." && take 1 f /= ".") fs
             concat <$> mapM (\f -> getRecursiveFiles $ d ++ "/" ++ f) fs'
     else return [d]

getSourceFiles :: FilePath -> IO [FilePath]
getSourceFiles f = filter (\x -> any (`isSuffixOf` x) suffixes) <$> getRecursiveFiles f

filesToText :: [FilePath] -> IO T.Text
filesToText fs = T.concat <$> mapM T.readFile fs

getStaleStatics :: FilePath -> FilePath -> IO [(T.Text, FilePath)]
getStaleStatics sdir adir = do
  src <- filesToText =<< getSourceFiles adir
  filter (\(s,_) -> not $ s `T.isInfixOf` src) <$> getStatics sdir

-- | The same function as checkStaleStatics but takes a relative path
-- to your static dir, a relative path to your application dir and a
-- whitelist of statics which will be ignored.
checkStaleStaticsWith :: FilePath -> FilePath -> [T.Text] -> Q [Dec]
checkStaleStaticsWith sdir adir wl = runIO action >> return []
  where toMsg fs = unlines $ "Following statics aren't used:": map snd fs
        action   = do xs <- getStaleStatics sdir adir
                      let xs' = filter (\(t,_) -> t `notElem` wl) xs
                      unless (null xs') (error $ toMsg xs')

-- | This function scans automatically on compile time all your yesod code and checks
-- whether you have used all your static assets.  Note that albeit it
-- does IO on compilation, it only reads your files so it should be
-- safe.  To use this function put it at the top level of Application.hs.
checkStaleStatics :: Q [Dec]
checkStaleStatics = checkStaleStaticsWith "static" "." whiteList

-- | Standard whitelist of statics which will be ignored.
whiteList :: [T.Text]
whiteList = [ "fonts_glyphicons_halflings_regular_eot"
            , "fonts_glyphicons_halflings_regular_svg"
            , "fonts_glyphicons_halflings_regular_ttf"
            , "fonts_glyphicons_halflings_regular_woff"]
