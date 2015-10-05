#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack)
import Data.List
import System.FilePath (dropFileName)
import Turtle

type PackageName = Text
data Repository = Repository { repoPath :: String
                             , repoUrl :: String
                             }

main :: IO ()
main = do
  let repo = Repository { repoPath = "/root/src/skynet"
                        , repoUrl = "https://github.com/drewr/skynet"
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
  mkdir (dropFileName $ repoPath repo)
  cmd "git" [ "clone", (pack $ repoUrl repo), (pack $ repoPath repo) ]

cmd :: Text -> [Text] -> IO ()
cmd name args = do
  ret <- proc name args empty
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> die (name <> " failed with exit code: " <> repr n)
