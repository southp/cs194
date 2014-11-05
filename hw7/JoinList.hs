import Data.Monoid
import Sized

data JoinList m a = Empty
              | Single m a
              | Append m (JoinList m a) (JoinList m a)
              deriving (Eq, Show)

-- exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) j1 j2 = Append (m1 `mappend` m2) j1 j2
    where m1 = tag j1
          m2 = tag j2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- exercise 2
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

sizedAnnToInt :: (Monoid m, Sized m) => JoinList m a -> Int
sizedAnnToInt = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _   Empty = Nothing
indexJ idx j | idx <  0               = Nothing
             | idx >= sizedAnnToInt j = Nothing
indexJ _ (Single m a) = Just a
indexJ idx (Append m j1 j2)
    | idx < s1  = indexJ idx j1
    | otherwise = indexJ (idx - s1) j2
    where s1 = sizedAnnToInt j1
          s2 = sizedAnnToInt j2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ s j
    | s < 0 = j
    | s > sizedAnnToInt j = Empty
dropJ s j@(Single m a)
    | s <= 0 = j
    | s >  0 = Empty
dropJ s (Append m j1 j2)
    | s >= s1 = dropJ (s - s1) j2
    | s <  s1 = dropJ s j1 +++ j2
    where s1 = sizedAnnToInt j1
          s2 = sizedAnnToInt j2

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ s j
    | s < 0 = Empty
    | s > sizedAnnToInt j = j
takeJ s j@(Single m a)
    | s <= 0 = Empty
    | s >  0 = j
takeJ s (Append m j1 j2)
    | s >  s1 = j1 +++ takeJ (s - s1) j2
    | s <= s1 = takeJ s j1
    where s1 = sizedAnnToInt j1
          s2 = sizedAnnToInt j2

-- some test data for ex.2
x1 = Single (Size 1) "a"
x2 = Single (Size 1) "b"
x3 = Single (Size 1) "c"
x4 = Single (Size 1) "d"
x5 = Single (Size 1) "e"

x = (x1 +++ x2) +++ (x3 +++ x4 +++ x5)


