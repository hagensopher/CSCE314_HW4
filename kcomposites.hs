numOfFactors :: Integer -> Integer
numOfFactors  n = toInteger (length [x | x <- [2..div n 2 +1], n `mod` x == 0])

kcomposite :: Integer -> [Integer]
kcomposite n = [x | x <- [1..], numOfFactors x == n ]
