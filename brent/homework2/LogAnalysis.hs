{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage line = case (words line) of
  "E":num:ts:msg -> LogMessage (Error (read num)) (read ts) (unwords msg)
  "W":ts:msg -> LogMessage Warning (read ts) (unwords msg)
  "I":ts:msg -> LogMessage Info (read ts) (unwords msg)
  _ -> Unknown line


parse :: String -> [LogMessage]
parse logLines = map parseMessage (lines logLines) 
