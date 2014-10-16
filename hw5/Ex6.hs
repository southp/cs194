{-# LANGUAGE FlexibleInstances #-}

import Parser
import qualified Data.Map as M

-- exercise 6
class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

class HasVars a where
    var :: String -> a

data VarExprT
    = Lit Integer
    | Add VarExprT VarExprT
    | Mul VarExprT VarExprT
    | Var String
    deriving (Show, Eq)

instance HasVars VarExprT where
    var = Var

instance Expr VarExprT where
    lit = Lit
    mul = Mul
    add = Add

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x _ = Just x
    mul x y z = case (x z, y z) of
                    (Nothing, _) -> Nothing
                    (_, Nothing) -> Nothing
                    (Just a, Just b) ->  Just (a * b)
    add x y z = case (x z, y z) of
                    (Nothing, _) -> Nothing
                    (_, Nothing) -> Nothing
                    (Just a, Just b) ->  Just (a + b)

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

