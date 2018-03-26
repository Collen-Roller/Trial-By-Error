import isPrefixOf
import toUpper

main = do
  --print ("maxVal=")
  --val <- getVal
  --print ("Fib=")
  --print ("Hello")
  --print (rotate 5 ['A'..'Z'])
  --print (makeKey 2)
  print (candidates "DGGADBCOOCZYMJHZYVMTOJOCZHVS")

--Problem 1
rotate :: Int -> [Char] -> [Char]
rotate k list | k >= 0 && k <= length list = drop k list ++ take k list
                | otherwise = error "n is negative or too large"

--Problem 2
makeKey :: Int -> [(Char,Char)]
makeKey k = zip ['A'..'Z'] (rotate k ['A'..'Z'])

--Problem 3
lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec asd [] = asd
lookUpRec target ((key,val):xs) | key == target = val
						| otherwise = lookUpRec target xs

--Problem 4
lookUp :: Char -> [(Char, Char)] -> Char
lookUp target list = if length [snd item | item <- list, target == fst item] > 0
				   then [snd item | item <- list, target == fst item] !! 0
                   else target

--Problem 5
encipher :: Int -> Char -> Char
encipher val target = lookUpRec target (makeKey val)

--Problem 6
normalize :: String -> String
normalize val = [toUpper x | x <- val, x >= '0' && x <= '9' ||
				       x >= 'A' && x <= 'Z' ||
                                       x >= 'a' && x <= 'z']

--Problem 7
encipherStr :: Int -> String -> String
encipherStr val msg = [encipher val letter |letter <- normalize msg]

--Problem 8
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey list = [(snd x, fst x) | x <- list]

--Problem 9 -> Skipped because the recursive versin would be annoying to writeBlocks_

--Problem 10
decipher :: Int -> Char -> Char
decipher val char = lookUp char (reverseKey (makeKey val))

decipherStr :: Int -> String -> String
decipherStr val str = [decipher val x | x <- normalize str]

--Problem 11 ->  This I took from the tutorial answers. I had trouble on the
--               previous tutorial so I checked it out. isPrefixOf (Super useful)
--               I used this last time but I needed to check ever part of the String
--               therefore the recursive section!
contains :: String -> String -> Bool
contains _ [] = True
contains [] _ = False
contains str substr = isPrefixOf substr str || contains (tail str) substr

--Problem 12 -> Put it all together!
candidates :: String -> [(Int, String)]
candidates str = [(val, decipherStr val str) | val <- [0..25], isCandidate val str]

isCandidate :: Int -> String -> Bool
isCandidate val str = if contains (decipherStr val str) "THE" || contains (decipherStr val str) "AND"
		      then True
         	      else False


--Online Solution for 12
--candidates :: String -> [(Int, String)]
--candidates str = [(i, decipherStr i str) | i <- [0..25], candidate (decipherStr i str)]
--    where
--        candidate str = str `contains` "AND" || str `contains` "THE"
