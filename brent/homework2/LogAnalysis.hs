{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage line = case words line of
  "E" : num : ts : msg -> LogMessage (Error (read num)) (read ts) (unwords msg)
  "W" : ts : msg -> LogMessage Warning (read ts) (unwords msg)
  "I" : ts : msg -> LogMessage Info (read ts) (unwords msg)
  _ -> Unknown line

parse :: String -> [LogMessage]
parse logLines = map parseMessage (lines logLines)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert lm@(LogMessage _ _ _) t@Leaf = Node t lm Leaf
insert lmNew@(LogMessage _ tsNew _) (Node lt lm@(LogMessage _ ts _) rt)
  | tsNew > ts = Node lt lm (insert lmNew rt) -- na prawo
  | tsNew < ts = Node (insert lmNew lt) lm rt -- na lewo
  | otherwise = undefined -- czy ts może być równy tsNew?
insert _ t = t
