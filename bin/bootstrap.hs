#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Text (pack, unpack)
import System.Directory (doesDirectoryExist)
import Turtle

type PackageName = Text
data Repository = Repository { repoPath :: Turtle.FilePath
                             , repoUrl :: Text
                             , repoCommit :: Text
                             }

main :: IO ()
main = do
  let repo = Repository { repoPath = fromText "/root/src/skynet"
                        , repoUrl = "https://github.com/drewr/skynet"
                        , repoCommit = "origin/master"
                        }
  updateApt
  package [ "facter"
          , "puppet"
          , "curl"
          , "git"
          , "rsync"
          ]
  cloneRepo repo
  syncEtc (repoPath repo)

updateApt :: IO ()
updateApt = cmd "apt-get" [ "update" ]

package :: [PackageName] -> IO ()
package names = cmd "apt-get" ([ "install", "-f", "-y" ] ++ names)

syncEtc :: Turtle.FilePath -> IO ()
syncEtc dir = do
  cd dir
  cmd "rsync" [ "-avz", "puppet", "/etc" ]

cloneRepo :: Repository -> IO ()
cloneRepo repo = do
  exist <- testdir (repoPath repo)
  if exist
    then gitCheckoutForce (repoPath repo) (repoCommit repo)
    else gitClone (repoUrl repo) (repoPath repo)

gitCheckoutForce :: Turtle.FilePath -> Text -> IO ()
gitCheckoutForce dir commit = do
  cd dir
  cmd "git" [ "fetch" ]
  cmd "git" [ "checkout", "-f",  commit]

gitClone :: Text -> Turtle.FilePath -> IO ()
gitClone from to = do
  let Right dest = (toText to)
  mktree (dirname to)
  cmd "git" [ "clone", from, dest ]

cmd :: Text -> [Text] -> IO ()
cmd name args = do
  ret <- proc name args empty
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> die (name <> " failed with exit code: " <> repr n)
