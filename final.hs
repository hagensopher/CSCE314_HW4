--final file with all the functions 
-- Hagen Sopher
-- UIN: 426004814
-- //////////////////////////////// START OF CTR QUESTION //////////////////////


--helper functions needed to solve the problem:
--1. generate a list of numbers from the a = x mod n (i.e. 3 = x mod 0 = 3,6,9,12,15,18)
-- the input would be a pair of number a and n so taking 
listOfFactor :: (Integer, Integer) -> [Integer]
listOfFactor (a,n) = [a + x  | x <- [0..105], x `mod` n == 0]
--2. function which takes the numbers to find the highest i.e. 5x7x3 = 105 
sumOfAll :: [(Integer, Integer)] -> Integer
sumOfAll [] = 1
sumOfAll (x:xs) = snd x * sumOfAll xs 
-- 3. a function which compares two list to find the correct a = x mod n pair (On^2)
-- i.e. 2 = x mod 7 and 1 = x mod 5 would be 16 = x mod 35 
-- then take that new mod and generate its list with 3 = x mod 0 to get the final answer
--3.a inner loop of double for loop
smallestSharedNumber:: [Integer] -> Integer -> Bool
smallestSharedNumber myList n = n `elem` myList --if n is in the list 

--3.b outer loop of double for loop --return the [a . n]

listChecker :: [Integer] -> [Integer] -> Integer
listChecker [] _ = -1
listChecker (x:xs) l2 = if smallestSharedNumber l2 x then x else listChecker xs l2

--function take 2 tuples and generate the new one then make the new list tuple pair
equationGenerator:: (Integer, Integer)-> (Integer, Integer) -> (Integer, Integer)
equationGenerator (a1,n1) (a2,n2) =  (listChecker (listOfFactor(a1, n1)) (listOfFactor(a2, n2))  , n1 * n2)

--                 --list of pairs      --current pair       --return cobination
equationLooper :: [(Integer,Integer)] ->(Integer,Integer)-> (Integer,Integer)
equationLooper [] (a,n) = (a,n) 
equationLooper (x:xs) current = equationLooper xs (equationGenerator current (x))


--starter function 
ctr:: [(Integer,Integer)] -> (Integer,Integer)
ctr (x:xs) = equationLooper xs x

-- /////////////////////////////////END OF QUESTOIN 1 CTR/////////////////////////////


-- /////////////////////////////////START OF QUESTION 2 komposites/anagram /////////////

numOfFactors :: Integer -> [Integer]
numOfFactors  n = [x | x <- [2..div n 2 +1], n `mod` x == 0]

kcomposite :: Integer -> [Integer]
kcomposite n = [x | x <- [1..], toInteger (length (numOfFactors x)) == n ]

-- now we have this start of the hard part 
--take the length of a list and a list of k-composite (above) and check if it is two composite
isTwoComp :: Integer -> [Integer]-> Bool
isTwoComp _ [] = False
isTwoComp listLength (x:xs)
    | listLength > x = isTwoComp listLength xs
    | listLength == x = True
    | listLength < x = False

--this function will make the list two composite if it is not
makeTwoComp :: [Char] -> [Char]
makeTwoComp myWord = if x then myWord else makeTwoComp (myWord ++ ['X']) 
    where x = isTwoComp ( toInteger (length myWord)) (kcomposite 2)

-- se we know the string is now two composite number , so we can use the two factors to make a string
--so make a list that holds the large factor of the list lenght
rowCreation :: [Char] -> Integer -> [Char]
rowCreation myWord n = take (fromInteger n) myWord

collumCreation :: [Char] -> [Integer] -> [[Char]]
collumCreation [] _ = []
collumCreation myWord n = [rowCreation (myWord) (sndFactor)] ++ collumCreation (drop (fromInteger sndFactor) myWord) n
    where sndFactor = last n
--use map

--it was this simple OMG well this grabs each element in a collum
combineHeads:: [[Char]] -> [Char]
combineHeads [] = []
combineHeads listOfStrings = if head listOfStrings == [] then [] else map head listOfStrings ++ combineHeads (map tail listOfStrings)
--makes the right message but gives a expetion that the haed is null

--function that glues it all together
anagramEncode :: [Char] -> [Char]
anagramEncode myList = combineHeads (collumCreation (makeTwoComp myList) (numOfFactors  (toInteger (length (makeTwoComp myList)))))

--trim of the x values from the string
trimX:: [Char]->[Char]
trimX myList = if last myList /= 'X' then myList else trimX (init myList)
--this function decodes the message 
anagramDecode :: [Char] -> [Char]
anagramDecode myList = trimX (combineHeads (collumCreation myList (reverse (numOfFactors  (toInteger (length (myList)))))))

--example of mapping with getting the first element of each collum in a 2D array (not used in the solution)
col :: [[Int]] -> [Int]
col n = map head n

-- //////////////////////////////////END OF QUESTION 2 kcomposites/anagram /////////////////////////////

-- ///////////////////////////////// START OF QUESTION 3 JUG PROBLEM //////////////////////////////////

--a list of all actions we can do
{--
1. fil jug 1
2. fill jug 2
3. move jug 1 -> jug 2
4. move jug 2 -> jug 1
5. discard jug 1 
6. discard jug 2

--}
-- 1,2,5,6 are moved into the function actionList for simplicity , but 3 and 4 are recusive so needed
-- 1.
--fillJug1 :: (Integer,Integer) -> Integer-> (Integer,Integer)
--fillJug1 jugs sizeOfJug = if fst jugs == sizeOfJug then jugs else (sizeOfJug, snd jugs)
-- 2.
--fillJug2 :: (Integer,Integer) -> Integer-> (Integer,Integer)
--fillJug2 jugs sizeOfJug = if snd jugs == sizeOfJug then jugs else (fst jugs ,sizeOfJug) 
-- 3.
move1to2 :: (Integer,Integer) ->(Integer,Integer)-> (Integer,Integer)
move1to2 jugs jugSizes = if fst jugs == 0 || snd jugs == snd jugSizes then jugs else move1to2 (fst jugs - 1,snd jugs +1) jugSizes
-- 4.
move2to1 :: (Integer,Integer) ->(Integer,Integer)-> (Integer,Integer)
move2to1 jugs jugSizes = if snd jugs == 0 || fst jugs == fst jugSizes then jugs else move2to1 (fst jugs + 1,snd jugs -1) jugSizes
-- 5.
--discardJug1 :: (Integer,Integer) -> (Integer,Integer)
--discardJug1 jugs = (0 ,snd jugs)
-- 6.
--discardJug2 :: (Integer,Integer) -> (Integer,Integer)
--discardJug2 jugs = (fst jugs ,0)

actionList:: (Integer,Integer) ->(Integer,Integer)->Integer->(Integer,Integer)
actionList jugs sizeOfJugs i --i is the action number we want to do
    | i == 1 = if fst jugs == fst sizeOfJugs then jugs else (fst sizeOfJugs, snd jugs)
    | i == 2 = if snd jugs == snd sizeOfJugs then jugs else (fst jugs ,snd sizeOfJugs)
    | i == 3 = if fst jugs == 0 || snd jugs == snd sizeOfJugs then jugs else move1to2 (fst jugs - 1,snd jugs +1) sizeOfJugs
    | i == 4 = if snd jugs == 0 || fst jugs == fst sizeOfJugs then jugs else move2to1 (fst jugs + 1,snd jugs -1) sizeOfJugs
    | i == 5 = (0 ,snd jugs)
    | i == 6 = (fst jugs ,0)
    | otherwise = (-100,-100) --catch all of bad

--state checker --visted states    --current jugs   --the size of jugs x y  --action list [1..6] --measuring too (z)
stateChecker:: [(Integer,Integer)] -> (Integer,Integer) -> (Integer,Integer)->[Integer] -> Int-> Bool
stateChecker stateList jugs _ [] _ = False
stateChecker stateList jugs sizeOfJugs (x:xs) output  
    |  (fst newState + snd newState) == toInteger output = True
    |  newState `elem` stateList =  stateChecker stateList jugs sizeOfJugs xs output  
    |  otherwise = stateChecker (stateList ++ [newState]) newState sizeOfJugs [1..6] output 
    where newState = actionList jugs sizeOfJugs x --the state we are checking 

measureWater:: Int -> Int -> Int -> Bool
measureWater jug1 jug2 output = stateChecker [(0,0)] (0,0) (toInteger jug1, toInteger jug2) [1..6] output
    -- [(0,0),(3,0),(0,3),(3,3),(1,5)]