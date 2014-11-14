import Data.List

import System.Environment

import Employee
import Party

guestListString :: GuestList -> String
guestListString (GL es f) = "Total fun: " ++ show f ++ "\n" ++ estrs
    where estrs = concat $ sort . map ((++ "\n") . empName) $ es

main :: IO ()
main = getArgs >>= (\args -> case args of [] -> putStrLn "Please enter a input name."
                                          [a] -> readFile a >>= (\fcontent -> return (maxFun (read fcontent)))
                                                            >>= (\gl -> putStr . guestListString $ gl)
                                          otherwise -> putStrLn "I only work for one company at a time. You know...")
