import ExprT
import Parser

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
   lit x   = Lit x
   mul x y = Mul x y
   add x y = Add x y

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr exp = do_eval (parseExp Lit Add Mul exp)
    where
        do_eval Nothing  = Nothing
        do_eval (Just e) = Just (eval e)

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
    lit x   = x
    mul x y = x * y
    add x y = x + y

instance Expr Bool where
    lit x   = x <= 0
    mul x y = x && y
    add x y = x || y

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit x                     = MinMax x
    mul (MinMax x) (MinMax y) = MinMax (max x y)
    add (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
    lit x                 = Mod7 (x `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x*y) `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

