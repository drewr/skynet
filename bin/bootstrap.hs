#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Text (pack, unpack)
import System.Directory (doesDirectoryExist)
import Turtle

type PackageName = Text
data Repository = Repository { repoPath :: Text
                             , repoUrl :: Text
                             , repoCommit :: Text
                             }

main :: IO ()
main = do
  let repo = Repository { repoPath = "/root/src/skynet"
                        , repoUrl = "https://github.com/drewr/skynet"
                        , repoCommit = "origin/master"
                        }
  updateApt
  package [ "facter"
          , "puppet"
          , "curl"
          , "git"
          ]
  cloneRepo repo

updateApt :: IO ()
updateApt = cmd "apt-get" [ "update" ]

package :: [PackageName] -> IO ()
package names = cmd "apt-get" ([ "install", "-f", "-y" ] ++ names)

cloneRepo :: Repository -> IO ()
cloneRepo repo = do
  exist <- doesDirectoryExist (unpack $ repoPath repo)
  if exist
    then gitCheckoutForce (repoPath repo) (repoCommit repo)
    else gitClone (repoUrl repo) (repoPath repo)

gitCheckoutForce :: Text -> Text -> IO ()
gitCheckoutForce dir commit = do
  cd (fromText dir)
  cmd "git" [ "checkout", "-f",  commit]

gitClone :: Text -> Text -> IO ()
gitClone from to = do
  mkdir (dirname . fromText $ to)
  cmd "git" [ "clone", from, to ]

cmd :: Text -> [Text] -> IO ()
cmd name args = do
  ret <- proc name args empty
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> die (name <> " failed with exit code: " <> repr n)
