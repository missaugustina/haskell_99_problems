-- return last element of a list
-- problem - needs to enforce a non-empty list
myLast :: [a] -> a
myLast xs = head $ reverse xs

-- return second to last element of a list
-- problem - needs to enforce a list with at least 2 elements
myButLast :: [a] -> a
myButLast xs = head $ tail $ reverse xs

-- return 1-index element in a list
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

-- return length of list
myLength :: [a] -> Int
myLength (x:xs) = 1 + myLength xs
myLength [] = 0

-- check if a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)
-- before i realized i could compare 2 lists
isPalindrome' xs = not $ elem False $ map (\x -> fst x == snd x) $ zip (take ((length xs) `div` 2) xs) (reverse $ drop ((length xs) `div` 2) xs)

-- flatten a nested list or element
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List (a:as)) = (flatten a) ++ (flatten (List as))
flatten (Elem a) = [a]
flatten (List []) = []

flatten' :: NestedList a -> [a]
flatten' (List as) = concatMap flatten as
flatten' (Elem a) = [a]

-- remove consecutive repeats
compress :: Eq a => [a] -> [a]
compress (x:xs@(y:_)) -- x, xs as y:... aka x:(y:xs)
    | x == y = compress xs
    | otherwise = x : compress xs
compress xs = xs -- get the final element

-- break up into sub lists of repeats and non repeating elements
-- pack "aaaabccaadeeee" -> ["aaaa","b","cc","aa","d","eeee"]
-- pack [1,1,2,3,1,2,2,3] -> [[1,1],[2],[3],[1],[2,2],[3]]
pack :: Eq a => [a] -> [[a]]
pack xs@(x:_) = [replicate (snd pair) x] ++ pack (drop (snd pair) xs)
    where
      pair = countGroups xs
pack [] = []

-- make a list of pairs,
-- the number and then the number of times it occurs in the list
countGroups :: Eq a => [a] -> (a, Int)
countGroups xs@(x:_) =
    (x, (loop x xs 0))
    where
      loop :: Eq a => a -> [a] -> Int -> Int
      loop x [] acc = acc
      loop x (y:ys) acc = if y == x
                        then loop x ys (acc + 1)
                        else acc
-- aka takeWhile
foo :: (a -> Bool) -> [a] -> [a]
foo _ [] = []
foo f (x:xs)
    | f x = x : (foo f xs)
    | otherwise = []

pack' :: Eq a => [a] -> [[a]]
pack' xs@(x:_) = keep : pack' (drop (length keep) xs)
                 where keep = takeWhile (\y -> y == x) xs
pack' [] = []

-- aka span
bar :: (a -> Bool) -> [a] -> ([a], [a])
bar f (x:xs)
    | f x =  let (h, t) = bar f xs
             in (x:h, t)
    | otherwise = ([], x:xs)
bar _ [] = ([],[])

pack'' :: Eq a => [a] -> [[a]]
pack'' xs@(x:_) = hs : pack'' ts
    where (hs, ts) = span (\y -> y == x) xs
pack'' [] = []

-- run-length encoding (whoops I already did this haha)
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack''
