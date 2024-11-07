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
insert msg t@Leaf = Node t msg Leaf
insert msgNew (Node lt msg rt)
  | tsNew >= ts = Node lt msg (insert msgNew rt) -- to the right
  | tsNew < ts = Node (insert msgNew lt) msg rt -- to the left
  where
    LogMessage _ ts _ = msg
    LogMessage _ tsNew _ = msgNew
insert _ t = t

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt lm rt) = inOrder lt ++ [lm] ++ inOrder rt

errFilter :: LogMessage -> Bool
errFilter (LogMessage (Error severity) _ _)
  | severity >= 50 = True
  | otherwise = False
errFilter _ = False

onlyBigError :: [LogMessage] -> [LogMessage]
onlyBigError list = [e | e <- list, errFilter e]

getStr :: LogMessage -> String
getStr (LogMessage _ _ msg) = msg
getStr (Unknown msg) = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong list = [getStr e | e <- list, errFilter e]
