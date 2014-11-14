module Party where

import Data.Monoid
import Data.Tree

import Employee

-- ex.1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL emps fun) = GL (e:emps) (fun + empFun e)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = if gl1 > gl2 then gl1 else gl2

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

-- ex.2
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f b (Node a []) = f a [b]
treeFold f b (Node a ts) = f a (map (treeFold f b) ts)

-- ex.3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e [] = (GL [e] (empFun e), GL [] 0)
nextLevel e glps = (boss, noboss)
    where
        boss = glCons e (mconcat . map snd $ glps)
        noboss = mconcat [max gl1 gl2 | (gl1, gl2) <- glps]

-- ex.4
maxFun :: Tree Employee -> GuestList
maxFun t = max gl1 gl2
    where (gl1, gl2) = treeFold nextLevel (GL [] 0, GL [] 0) t
