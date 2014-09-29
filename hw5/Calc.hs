import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr exp = do_eval (parseExp Lit Add Mul exp)
    where
        do_eval Nothing  = Nothing
        do_eval (Just e) = Just (eval e)
