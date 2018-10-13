-- 関数プログラミン実践入門　Page:190
-- 高階関数について
--
import Debug.Trace
mTrace y x xs = trace(" -OOPs: "++show y ++ " " ++ show x ++ " " ++ show xs)
fTrace y = trace("OOPs: " ++ show y) y
xTrace ::(Show a) => a -> a
xTrace x = trace("oop: " ++ show x) x
--
-- 関数を二つ要求しそのタップルに適応する関数　each
each' :: ( a -> b) -> (c -> d) -> ( a , c) -> (b,d)
each' f g (x,y) = (f x, g y)
-- 使い方
-- *Main> each' (+2) (+1) (10,100)
-- (12,101)
-- *Main> each' show length (100,"abcdef")
-- ("100",6)


-- よく使う高階関数を定義します。
-- filter'
-- *Main> filter' (<3) [1..5]
-- [1,2]
-- *Main> filter' (\x -> x <= 3) [1..5]
-- [1,2,3]
filter' :: ( a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs)
    | f x       = x : filter' f xs
    | otherwise = filter' f xs

-- zip'
--
-- *Main> zip' [4,5,6] [10,20]
-- [(4,10),(5,20)]
-- *Main> zip' [4,5,6] [10,20,30,40]
-- [(4,10),(5,20),(6,30)]
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _     = []
zip' _ []     = []
zip' (x:xs) (y:ys) = (x,y) : (zip' xs ys)

-- foldl'
-- foldl (+) 1 [1..3] は : を関数に置き換える　初期値を末尾にプラスする
-- 1 : 2 : 3  のリストはまず初期値を末尾にプラス
-- 1 : 2 : 3 : 4     次に:を引数関数に変換する。
-- 1 + ( 2 + ( 3 + ( 4))) = 10
-- ちなみにmap関数をfoldl'で書き直すと
--map' :: (Show (a -> b)) => ( a -> b ) -> [a] -> [b]
map' f as = foldr ( (:) . (trace f) ) [] as
--length' もfoldr で書き直す
length' :: [a] -> Int
length' as = foldr (\a b -> 1 + b) 0 as


data RGB a = RGB { getR:: a
                ,getG:: a
                ,getB:: a
}

--myRGB :: (Show RGB)
myRGB =  RGB { getR=1.0::Float,getG=0.25,getB=0.5}

myR = getR  myRGB
myG = getG  myRGB
myB = getB  myRGB

-- mTree
data Tree a = Leaf{  element :: a }
            |  Fork{ element :: a ,
               left          :: Tree a,
               right         :: Tree a
            } deriving Show
dTree :: Tree Integer
dTree = dtree' 0 where
      dtree' depth = Fork { element = depth
                          , left = dtree' (depth + 1)
                          , right = dtree' (depth + 2)
      }
