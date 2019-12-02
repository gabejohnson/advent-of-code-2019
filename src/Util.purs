module Util (readInput) where

import Prelude
import Effect (Effect)
import Node.Encoding as Node.Encoding
import Node.FS.Sync as Node.FS.Sync

readInput :: String -> Effect String
readInput path = Node.FS.Sync.readTextFile Node.Encoding.ASCII ("input/" <> path)
