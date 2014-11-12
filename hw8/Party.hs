module Party where

import Data.Monoid

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL emps fun) = GL (e:emps) (fun + empFun e)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = if gl1 > gl2 then gl1 else gl2

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)
