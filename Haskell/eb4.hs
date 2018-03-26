-- Learning about Higher Order Functions...
-- Recursive vs List comprehension vs (map funciton element)

--Problem 1
uppers :: String -> String
uppers str = map toUpper str

--Remember Map is like THIS
--g xs = [ f x | x <- xs ]
--g xs = map f xs
-- map, function, list (Super easy!)

--Problem 2 (List Comprehension Version)
uppers2 :: String -> String
uppers2 str = [toUpper x | x <- str]

--Problem 3 (Double Value With Map)
doubles :: [Int] -> [Int]
doubles val = map doubleVal val

doubleVal :: Int -> Int
doubleVal val = 2*val

--Problem 3 (List Comprehension)
doubles :: [Int] -> [Int]
doubles val = [2*v | v <- val]

--Problem 4
penceToPounds :: [Int] -> [Float]
penceToPounds val =  map (\x -> fromIntegral x / 100) val

--Problem 5 (List Comprehension)
alphas :: String -> String
alphas str = [x | x <- str, x >= 'A' && x <= 'Z' || x >= 'a' && x <= 'z']

--Problem 5 (Map)
alphas :: String -> String
alphas text = filter isAlpha text

--Filter is even easier... lets pass a function, and a list!
--g xs = [ x | x <- xs, p x ]
--g xs = filter p xs, instead of a function, p is a condition!

--Problem 6 (List Compresension)
rmChar :: Char -> String -> String
rmChar char str = [x | x <- str, x /= char]

--Problem 6 (Map)
rmChar ::  Char -> String -> String
rmChar char text = filter (\x -> x /= (toLower char) && x /= (toUpper char)) text

--Problem 6 (Recursive)
rmChar :: Char -> String -> String
rmChar _ [] = []
rmChar char (x:xs) | x == char = rmChar char xs
				   | otherwise = x : rmChar char xs

--Problem 7 (Filter)
above :: Int -> [Int] -> [Int]
above val list = filter (\x -> x > val) list

--Problem 7 (List Compresention)
above :: Int -> [Int] -> [Int]
above val list = [x | x <- list, x > val]

--Problem 8 (Filter)
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals list = filter (\(x,y) -> x /= y) list

--We can also dynamically assign variabes within the list... i nthis case-
-- we have a list of tuples... this makes it easy

--Problem 8 (List Comprehension)
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals list = filter (\(x,y) -> x /= y) list

--Problem 8 (Recursion)
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals [] = []
unequals ((x,y):xs) | x == y = unequals xs
					| otherwise = (x,y) : unequals xs

--In general, [f x | x <- xs, p x] is equivalent to map f (filter p xs).
--Problem 9
upperChars :: String -> String
upperChars str = map toUpper (filter (\c -> isAlpha c) str)


-- Problem 10 (List Comprehension)
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

-- Problem 10 (Filter and Map)
largeDoubles :: [Int] -> [Int]
largeDoubles list = map (2*) (filter (\x -> x > 3) list)

--Problem 11 (List Comprehension)
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

--Problem 11 (Filter and Map)
reverseEven :: [String] -> [String]
reverseEven str = map reverse (filter (\x -> even (length x)) str)

--Foldr

--Problem 12
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

--Problem 12
productFold :: [Int] -> Int
productFold xs = foldr (*) 1 xs


--Problem 13 (Recursion)
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) | x == True = True && andRec xs
			  | otherwise = andRec xs

--Problem 13 (Foldr)
andFold :: [Bool] -> Bool
andFold list = foldr (&&) True list

--Problem 14
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold list = foldr (++) [] list

--Problem 15
rmChar ::  Char -> String -> String
rmChar char text = filter (\x -> x /= char) text

rmCharsRec :: String -> String -> String
rmCharsRec _ [] = []
rmCharsRec [] str = str
rmCharsRec (x:xs) str = rmCharsRec xs (rmChar x str)

rmCharsFold :: String -> String -> String
rmCharsFold target str = foldr (rmChar) str target

--Problem 16 (Had problems with this one)
uniform :: [Int] -> Bool
uniform list = all (\x -> x == list !! 0) list

--Problem 17
type Matrix = [[Int]]

valid :: Matrix -> Bool
valid mat | length mat > 0 && length (mat !! 0) > 0 = matEquality mat
		  | otherwise = False

matEquality :: Matrix -> Bool
matEquality mat = all(\x -> length x == length (mat !! 0)) mat

--Problem 18
-- zipWith f [] _ = []
-- zipWith f _ [] = []
-- zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- Show how to define zipWith using zip and a list comprehension.

-- Remember, Zip... zip takes two lists and mergers them into an array of pairs!

zipWith2 f [] _ = []
zipWith2 f _ [] = []
zipWith2 f xs ys = [f x y | (x,y) <- zip xs ys]

-- Important thing about this is that zip takes two lists, and we can define
-- when setting variables.. not only after the fact!

--Problem 19
-- Use map and uncurry to define the same functions vs list Comprehension
--Map is: map f list
zipWith2 f [] _ = []
zipWith2 f _ [] = []
zipWith2 f xs ys = map (uncurry f) (zip xs ys)

--Problem 20 (A bit tricky using zipWith two times...)
plusM :: Matrix -> Matrix -> Matrix
plusM mat1 mat2 = zipWith (plusRow) mat1 mat2

plusRow :: [Int] -> [Int] -> [Int]
plusRow row1 [] = row1
plusRow [] row2 = row2
plusRow row1 row2 = zipWith (+) row1 row2

--Problem 21 (More tricky!!!)
timesM :: Matrix -> Matrix -> Matrix
timesM mat1 mat2 = [[dotProd rows cols | cols <- transpose mat2]
									   | rows <- mat1]

dotProd :: [Int] -> [Int] -> Int
dotProd x y = sum (zipWith (*) x y)
