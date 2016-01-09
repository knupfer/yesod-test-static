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
  let fs' = filter (not .  isPrefixOf (sdir ++ "/tmp/")) fs
  return $ zip (map (T.pack . map replace . drop (1 + length sdir)) fs') fs'

replace :: Char -> Char
replace c
  | 'A' <= c && c <= 'Z' = c
  | 'a' <= c && c <= 'z' = c
  | '0' <= c && c <= '9' = c
  | otherwise = '_'

getRecursiveFiles :: FilePath -> IO [FilePath]
getRecursiveFiles d = do
  isD <- doesDirectoryExist d
  if isD
     then do fs <- getDirectoryContents d
             let fs' = filter (\f -> f /= "." && f /= ".." && take 1 f /= ".") fs
             fs'' <- mapM (\f -> getRecursiveFiles $ d ++ "/" ++ f) fs'
             return $ concat fs''
     else return [d]

getSourceFiles :: FilePath -> IO [FilePath]
getSourceFiles f = filter (\x -> any (`isSuffixOf` x) suffixes) <$> getRecursiveFiles f

filesToText :: [FilePath] -> IO T.Text
filesToText fs = T.concat <$> mapM T.readFile fs

getStaleStatics :: FilePath -> FilePath -> IO [(T.Text, FilePath)]
getStaleStatics sdir adir = do
  src <- filesToText =<< getSourceFiles adir
  filter (\(s,_) -> not $ s `T.isInfixOf` src) <$> getStatics sdir

checkStaleStaticsWith :: FilePath -> FilePath -> [T.Text] -> Q [Dec]
checkStaleStaticsWith sdir adir wl = runIO (do xs <- getStaleStatics sdir adir
                                               let xs' = filter (\(t,_) -> t `notElem` wl) xs
                                               unless (null xs') (error $ toMsg xs'))
                                     >> return []
  where toMsg fs = unlines $ "Following statics aren't used:": map snd fs

checkStaleStatics :: Q [Dec]
checkStaleStatics = checkStaleStaticsWith "static" "." whiteList

whiteList :: [T.Text]
whiteList = []
