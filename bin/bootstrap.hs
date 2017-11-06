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

data OS = Fedora
        | Ubuntu
        | Debian
        | RedHat
        | CentOS
        | Windows
        | Darwin
        deriving (Show)

main :: IO ()
main = do
  let os = Fedora
      repo = Repository { repoPath = fromText "/root/src/skynet"
                        , repoUrl = "https://github.com/drewr/skynet"
                        , repoCommit = "origin/master"
                        }
  update os
  package os [ "facter"
             , "curl"
             , "git"
             , "rsync"
             , "zsh"
             ]
  cloneRepo repo
  unlockRepo repo
  syncEtc (repoPath repo)
  puppetApply

update :: OS -> IO ()
update Ubuntu = cmd "apt-get" [ "update" ]
update Fedora = cmd "dnf"     [ "update" ]

file :: Text -> Text -> Text -> IO ()
file src dest mode = do
  mktree (dirname $ fromText dest)
  let c = format ("curl -s " % s % " >" % s) src dest
  cmdSingle c
  let m = format ("chmod " % s % " " % s) mode dest
  cmdSingle m

package :: OS -> [PackageName] -> IO ()
package Ubuntu p = cmd "apt-get" ([ "install", "-f", "-y" ] ++ p)
package Fedora p = cmd "dnf" ([ "install", "-y" ] ++ p)

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
