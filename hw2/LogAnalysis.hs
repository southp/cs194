module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg = convert_msg (words msg)
    where
        convert_msg ("I" : timestamp : xs) = LogMessage Info
                                                        (read timestamp)
                                                        (unwords xs)
        convert_msg ("W" : timestamp : xs) = LogMessage Warning
                                                        (read timestamp)
                                                        (unwords xs)
        convert_msg ("E" : priority : timestamp : xs) = LogMessage (Error (read priority))
                                                                   (read timestamp)
                                                                   (unwords xs)
        convert_msg x = Unknown (unwords x)

parse :: String -> [LogMessage]
parse msg = map parseMessage msg_lines
    where msg_lines = lines msg

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert log@(LogMessage _ t1 _) (Node left log2@(LogMessage _ t2 _) right)
    | t1 <= t2 = Node (insert log left) log2 right
    | t1 >  t2 = Node left              log2 (insert log right)

build :: [LogMessage] -> MessageTree
build x = do_build x Leaf
    where
        do_build [] tree = tree
        do_build (x:xs) tree = do_build xs (insert x tree)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = map get_msg severe_ordered_logs
    where
        ordered_logs        = inOrder (build logs)
        severe_ordered_logs = pick_over_50 (inOrder (build logs))

        get_msg (LogMessage _ _ msg) = msg

        pick_over_50 [] = []
        pick_over_50 (log@(LogMessage (Error severity) _ _) : xs)
            | severity >= 50 = log : pick_over_50 xs
            | otherwise      = pick_over_50 xs
        pick_over_50 (x : xs) = pick_over_50 xs

