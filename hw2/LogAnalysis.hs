module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg = convert_msg (words msg)
    where
        convert_msg ("I" : timestamp : xs) = LogMessage Info (read timestamp) (unwords xs)
        convert_msg ("W" : timestamp : xs) = LogMessage Warning (read timestamp) (unwords xs)
        convert_msg ("E" : priority : timestamp : xs) = LogMessage (Error (read priority)) (read timestamp) (unwords xs)
        convert_msg x = Unknown (unwords x)

parse :: String -> [LogMessage]
parse msg = map parseMessage msg_lines
    where msg_lines = lines msg
