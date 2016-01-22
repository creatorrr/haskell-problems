-- 99 Haskell Problems
-- https://wiki.haskell.org/99_questions/1_to_10

-- Imports
import Control.Monad (liftM)
import System.Random (newStdGen, randomR)
import Data.List (intersect)
import Data.Complex (Complex(..), imagPart, realPart)

-- Utils
decc :: (Num a) => a -> a
decc = flip (-) $ 1

-- #1
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- #2
myButLast :: [a] -> a
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

-- #3
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) k = elementAt xs (k-1)

-- #4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- #5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- #6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome l = l == (myReverse l)

-- #7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))

-- #8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:rest@(y:xs))
  | x==y = compress rest
  | otherwise = x : compress rest

-- #9
segment :: (Eq a) => [[a]] -> [a] -> [[a]]
segment [] [] = []
segment [] (x:xs) = segment [[x]] xs
segment l [] = l
segment (g@(y:ys):gs) (x:xs)
  | x==y = segment ((x:g):gs) xs
  | otherwise = segment ([[x]] ++ (g:gs)) xs

pack :: (Eq a) => [a] -> [[a]]
pack = myReverse . segment []

-- #10
head' (x:xs) = x

encode :: (Eq a) => [a] -> [(Int, a)]
encode = countGroups . pack

countGroups :: [[a]] -> [(Int, a)]
countGroups [] = []
countGroups l = zip (map myLength l) (map head' l)

-- #11
data Counted a = Multiple Int a | Single a | None
  deriving Show

encodeModified :: (Eq a) => [a] -> [Counted a]
encodeModified = countGroupsModified . encode

countGroupsModified :: [(Int, a)] -> [Counted a]
countGroupsModified = map toCounted
  where
    toCounted (i, a)
      | i==0 = None
      | i==1 = Single a
      | otherwise = Multiple i a

-- #12
decodeModified :: (Eq a) => [Counted a] -> [a]
decodeModified = merge . map decode

merge :: [[a]] -> [a]
merge [] = []
merge (x:xs) = x ++ merge xs

decode :: Counted a -> [a]
decode None = []
decode (Single a) = [a]
decode (Multiple i a) = replicate i a

-- #13
encodeDirect :: (Eq a) => [a] -> [Counted a]
encodeDirect = countGroupsModified . myReverse . countDirect []

countDirect :: (Eq a) => [(Int, a)] -> [a] -> [(Int, a)]
countDirect [] [] = []
countDirect [] (x:xs) = countDirect [(1, x)] xs
countDirect l [] = l
countDirect (g@(i,y):gs) (x:xs)
  | x==y = countDirect ((i+1,y):gs) xs
  | otherwise = countDirect ([(1, x)] ++ (g:gs)) xs

-- #14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

-- #15
repli :: [a] -> Int -> [a]
repli l i = merge $ map (replicate i) l

-- #16
dropEvery :: [a] -> Int -> [a]
dropEvery l i = dropper len l
  where
    len = myLength l
    dropper 0 l = l
    dropper _ [] = []
    dropper c (x:xs)
      | (len - c + 1) `mod` i == 0 = dropper (c-1) xs
      | otherwise = x : (dropper (c-1) xs)

-- #17
split :: [a] -> Int -> [[a]]
split l = splitter [[], l]
  where
    splitter ls 0 = ls
    splitter [l, (x:xs)] c = splitter [l++[x], xs] (c-1)

-- #18
slice :: [a] -> Int -> Int -> [a]
slice l i j = drop (i-1) $ take j l

-- #19
rotate :: [a] -> Int -> [a]
rotate l i = rest ++ beg
  where
    beg = spl !! 0
    rest = spl !! 1
    len = myLength l
    ind = if i < 0 then len+i else i
    spl = split l ind

-- #20
removeAt :: Int -> [a] -> (a, [a])
removeAt i l = (removed, rest)
  where
    removed = l !! (i-1)
    rest = (slice l 0 (i-1)) ++ (slice l (i+1) len)
    len = myLength l

-- #21
insertAt :: a -> [a] -> Int -> [a]
insertAt c l i = beg ++ [c] ++ rest
  where
    beg = slice l 0 (i-1)
    rest = slice l i len
    len = myLength l

-- #22
range :: (Eq a, Enum a) => a -> a -> [a]
range i j
  | i==j = [i]
  | otherwise = i : range (succ i) j

-- #23
extract :: Int -> [a] -> (a, [a])
extract i l = (el, rest)
  where
    el = l !! i
    rest = (slice l 0 i) ++ (slice l (i+2) len)
    len = myLength l

rndSelect :: [a] -> Int -> IO [a]
rndSelect l 0 = return []
rndSelect l i = do
  gen <- newStdGen
  let (elem, rem) = extractRand gen

  rest <- rndSelect rem $ decc i
  return $ elem : rest

  where
    extractRand = (flip extract) l . index
    index = fst . rand
    rand = randomR (0, decc len)
    len = myLength l

-- #24
diffSelect :: Int -> Int -> IO [Int]
diffSelect n up = rndSelect [1..up] n

-- #25
rndPermu :: [a] -> IO [a]
rndPermu l = rndSelect l $ myLength l

-- #26
permutations :: Int -> [a] -> [[a]]
permutations 0 l = []
permutations 1 l = [[a] | a <- l]
permutations n l = [
    a : p |

    -- Iterate over all indices
    i <- [0..(decc . myLength $ l)],

    -- Get element and the rest of the list
    let (a, rest) = extract i l,

    -- Recursively populate permutations
    p <- permutations (decc n) rest
  ]

combinations :: Int -> [a] -> [[a]]
combinations 0 l = []
combinations 1 l = [[a] | a <- l]
combinations n l = [
    -- Add elem to combination
    (l !! i) : c |

    -- Iterate over all indices
    i <- [0..(decc . myLength $ l)],

    -- Recursively populate combinations
    let rest = drop (succ i) l,
    c <- combinations (decc n) rest
  ]

-- #27
extractAll :: [Int] -> [a] -> ([a], [a])
extractAll _ [] = ([], [])
extractAll [] l = ([], l)

extractAll (i:is) l = (el:selected, rem)
  where
      -- Recursively extract elements
      (selected, rem) = extractAll is' rest
      (el, rest) = extract i l

      -- decrease indices (now that the list is smaller)
      is' = map decc is

group :: [Int] -> [a] -> [[[a]]]

-- Group of all elements
group [n] set
  -- n must be equal to length of set
  | n == myLength set = (:[]) $ combinations n set
  | otherwise = error "Bad configuration"

group (n:ns) set = [
    -- Append group to list of subgroups
    subgroup : subgroups |

    -- Get all possible combinations of n indices
    indices <- iCombs n,

    -- Extract elements from generated indices
    let (subgroup, rest) = extractAll indices set,

    -- Recursively grab other groups in configuration
    subgroups <- group ns rest
  ]
  where
    iCombs = (flip combinations) is
    is = [0..(decc len)]
    len = myLength set

-- #28
sort' :: (Ord a) => [a] -> [a]
sort' = sortFunc id

sortFunc :: (Ord b) => (a -> b) -> [a] -> [a]
sortFunc _ [] = []
sortFunc f (x:xs) = sortFunc f smaller ++ [x] ++ sortFunc f larger
  where
    smaller = [a | a <- xs, (f a) <= (f x)]
    larger =  [a | a <- xs, (f a) > (f x)]

groupBy :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupBy _ [] = []
groupBy f l@(n:ns) = filter firstKind l : groupBy f (filter others l)
  where
    firstKind = (== (f n)) . f
    others = (/= (f n)) . f

lsort :: [[a]] -> [[a]]
lsort = sortFunc myLength

lfsort :: [[a]] -> [[a]]
lfsort = (foldr (++) []) . lsort . groupBy myLength

-- #31
divisors :: Int -> [Int]
divisors n = filter ((==0) . (mod n)) [1..n]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = (<=2) . myLength . divisors $ n

-- #32
max' :: (Ord a) => a -> a -> a
max' a b = if a > b then a else b

greatest :: (Ord a) => [a] -> a
greatest = foldr1 max'

common :: (Eq a) => [a] -> [a] -> [a]
common l m = [
    a |
    a <- l,
    b <- m,
    a==b
  ]

gcd' :: Int -> Int -> Int
gcd' a b = greatest $ common (divisors a) (divisors b)

-- #33
coprime :: Int -> Int -> Bool
coprime a b = 1 == gcd' a b

-- #34
totientPhi :: Int -> Int
totientPhi m = myLength $ filter (coprime m) [1..m]

-- #35
primeFactors :: Int -> [Int]
primeFactors = filter isPrime . divisors

-- #36
rev :: (a, b) -> (b, a)
rev (a, b) = (b, a)

getMult :: [Int] -> Int -> [Int]
getMult _ 1 = []
getMult l@(f:fs) n
  | n `mod` f == 0 = f : getMult l (n `div` f)
  | otherwise = getMult fs n

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = map rev $ encode $ getMult (primeFactors n) n

-- #37
mult :: (Num a) => [a] -> a
mult = foldr (*) 1

totientPhi' :: Int -> Float
totientPhi' = mult . (map reduce) . primeFactorsMult
  where
    reduce (p, m) = (decc (toF p)) * (toF p) ** (toF $ decc m)
    toF v = fromIntegral v :: Float

-- #39
-- TODO: Use prime sieve instead
primesR :: Int -> Int -> [Int]
primesR a b = filter isPrime [a..b]

-- #40
goldbach n
  | odd n = error "Only even numbers allowed"
  | otherwise = pairs !! 0
    where
      pairs = [
          (a, b) |
          let primes = primesR 1 n,
          a <- primes,
          b <- primes,
          a + b == n
        ]

-- #41
goldbachList a b = (map gbs . filter even) $ range a b
  where
    gbs n = (n, goldbach n)

largeGbs :: Int -> Int -> [(Int, Int)]
largeGbs a b = (filter big . map snd) $ goldbachList a b
  where
    checkPair p (a, b) = p a && p b
    big = checkPair (>threshold)
    threshold = 50

-- #46
infixl 3 `and'`
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

infixl 3 `or'`
or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

infixl 3 `not'`
not' :: Bool -> Bool
not' True = False
not' False = True

infixl 3 `nand'`
nand' :: Bool -> Bool -> Bool
nand' a b = not' $ and' a b

infixl 3 `nor'`
nor' :: Bool -> Bool -> Bool
nor' a b = not' $ or' a b

infixl 3 `eq'`
eq' :: Bool -> Bool -> Bool
eq' a b
  | a==b = True
  | otherwise = False

infixl 3 `xor'`
xor' :: Bool -> Bool -> Bool
xor' a b = not' $ eq' a b

table :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table p = [
    (a, b, p a b) |
    a <- [True, False],
    b <- [True, False]
  ]

-- #48
genBools :: (Num a, Eq a) => a -> [[Bool]]
genBools 0 = [[]]
genBools n = [ a:rest | a <- [True, False], rest <- genBools (n-1)]

table'
  :: (Num a, Eq a) =>
    a -> ([Bool] -> Bool) -> [[Bool]]

table' n p = [
    bs ++ [p bs] |
    bs <- genBools n
  ]

-- #90
-- 8-queens problem solution
-- (NOT an N-QUEENS solution, however)
-- Places queens row by row
type Board = [Int]
allPositions = [1..8]

queens :: Int -> [Board]
queens i = complete $ solutions [] i
  where
    -- Make sure all queens have been placed
    complete = filter $ (i==) . myLength

    solutions boards 0 = boards
    solutions boards i = solutions (nextBoards boards) $ decc i

nextBoards :: [Board] -> [Board]
nextBoards [] = [[pos] | pos <- allPositions]
nextBoards boards = [
    board ++ [pos] | board <- boards,
    pos <- nextPositions board
  ]

nextPositions :: Board -> Board
nextPositions [] = allPositions
nextPositions board = foldr intersect allPositions possibleSolutions
  where
    -- Run constraints by each queen on the board
    possibleSolutions = [placeBy i board | i <- [1..(myLength board)]]

placeBy :: Int -> Board -> Board
placeBy qIndex board = [
    col | col <- allPositions,  -- Next queen column

    col /= qCol,                -- Same column
    qCol + qRow /= col + row,   -- Primary diagonal
    qCol + row /= col + qRow    -- Secondary diagonal
  ]

  where
    row = myLength board        -- Row where next quuen is to be placed
    qCol = board !! qRow        -- Col of checking queen
    qRow = decc qIndex          -- Row (0-index) of checking queen

-- Misc: Matrix to spiral
type Point a = (a, a)

turnRight :: (RealFloat a) => Complex a -> Complex a
turnRight vec = vec * (0 :+ (-1))

-- Get next coords for matrix (y-axis is inverted)
next :: (Integral a, RealFloat b) => Point a -> Complex b -> Point a
next (x, y) vec = (x + (floor $ realPart vec), y - (floor $ imagPart vec))

-- Get elem at coord (inverse for matrix system)
get :: (Integral a) => Point a -> [[b]] -> b
get (x, y) mat = mat !! fromIntegral y !! fromIntegral x

-- Eat num elems from a matrix along a certain vector
eat
  :: (Num a, Eq a, RealFloat b, Integral c) =>
      a -> Point c -> Complex b -> [[d]] -> [d]

eat 0 _ _ _ = []
eat n pt vec mat = el : rest
  where
    el = get pt mat
    rest = eat (decc n) (next pt vec) vec mat

-- Spiral generator
spiralGen
  :: (RealFloat b, Integral c) =>
      Int -> Point c -> Complex b -> [[a]] -> [a]

spiralGen 0 _ _ _ = []
spiralGen n pt vec mat = (eat n pt vec mat) ++ spiralGen nextN nextPt nextVec mat
  where
    nextVec = turnRight vec
    nextPt = next pt ((fromIntegral . decc $ n) * vec + nextVec)
    nextN = if horizontal vec then decc n else n
    horizontal = (==0) . imagPart

-- Spiral
spiral :: [[a]] -> [a]
spiral mat = spiralGen (myLength mat) (0, 0) (1 :+ 0) mat

