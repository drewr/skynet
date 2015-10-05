#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Text (pack, unpack)
import System.Directory (doesDirectoryExist)
import System.Environment (getEnv)
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
  file "http://s3.draines.com.s3.amazonaws.com/dist/git-crypt" "/usr/local/bin/git-crypt" "0755"
  cloneRepo repo
  unlockRepo repo
  syncEtc (repoPath repo)
  puppetApply

updateApt :: IO ()
updateApt = cmd "apt-get" [ "update" ]

package :: [PackageName] -> IO ()
package names = cmd "apt-get" ([ "install", "-f", "-y" ] ++ names)

file :: Text -> Text -> Text -> IO ()
file src dest mode = do
  mktree (dirname $ fromText dest)
  let c = format ("curl -s " % s % " >" % s) src dest
  cmdSingle c
  let m = format ("chmod " % s % " " % s) mode dest
  cmdSingle m

syncEtc :: Turtle.FilePath -> IO ()
syncEtc dir = do
  cd dir
  cmd "rsync" [ "-avz", "--delete", "puppet", "/etc" ]

unlockRepo :: Repository -> IO ()
unlockRepo repo = do
  let localKey = "/tmp/crypt.key"
  key <- getEnv "GIT_CRYPT_KEY"
  file (pack key) localKey "0600"
  cmd "git" [ "crypt", "unlock", localKey ]
  rm (fromText localKey)

cloneRepo :: Repository -> IO ()
cloneRepo repo = do
  exist <- testdir (repoPath repo)
  if exist
    then gitCheckoutForce (repoPath repo) (repoCommit repo)
    else gitClone (repoUrl repo) (repoPath repo)
  gitUpdateSubmodules (repoPath repo)

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

gitUpdateSubmodules :: Turtle.FilePath -> IO ()
gitUpdateSubmodules dir = do
  cd dir
  cmd "git" [ "submodule", "sync" ]
  cmd "git" [ "submodule", "update", "--init" ]

puppetApply :: IO ()
puppetApply = do
  cmd "puppet" [ "apply", "/etc/puppet/manifests/" ]

cmd :: Text -> [Text] -> IO ()
cmd name args = do
  ret <- proc name args empty
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> die (name <> " failed with exit code: " <> repr n)

cmdSingle :: Text -> IO ()
cmdSingle c = do
  ret <- shell c empty
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> die (c <> " failed with exit code: " <> repr n)
