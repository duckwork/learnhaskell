{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s
    | (take 1 fields) == ["I"] = LogMessage Info      timestamp message
    | (take 1 fields) == ["W"] = LogMessage Warning   timestamp message
    | (take 1 fields) == ["E"] = LogMessage (Error l) errtime   errmessage
    | otherwise                = Unknown                        unkmessage
    where fields      = words s
          readField x = read $ unwords $ take 1 $ drop x fields
          readPast x  = unwords $ drop x fields
          timestamp   = readField 1
          message     = readPast 2
          l           = readField 1
          errtime     = readField 2
          errmessage  = readPast 3
          unkmessage  = readPast 0

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- MessageTree
getMType :: LogMessage -> MessageType
getMType (LogMessage t _ _) = t
-- probably not the best implementation ...
getMType (Unknown        _) = error "no message type"

getMTime :: LogMessage -> Int
getMTime (LogMessage _ t _) = t
getMTime (Unknown        _) = error "no message timestamp"

getMMsg :: LogMessage -> String
getMMsg (LogMessage _ _ m) = m
getMMsg (Unknown        m) = m

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
-- at bottom
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node earlier msg' later)
    | getMTime msg < getMTime msg' = Node (insert msg earlier) msg' later
    | getMTime msg > getMTime msg' = Node earlier msg' (insert msg later)
    | otherwise                    = error "Duplicate timestamp"

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node earlier msg later) = (inOrder earlier) ++ [msg] ++ (inOrder later)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMMsg . filter isMajorError . inOrder . build
    where isMajorError (LogMessage (Error l) _ _)
            | l >= 50   = True
            | otherwise = False
          isMajorError _ = False
