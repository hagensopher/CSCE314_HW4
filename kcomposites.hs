--isPrime Functions from class
isPrime:: Integer -> Bool
isPrime 1 = False
isPrime n = ([] == [x | x <- [2..(n-1)], n `mod` x == 0])

klist :: Integer -> [Integer]
klist  k = [x | x <-[1..], mod x k == 0, not (isPrime x)]

--1. find the prime factorization

--2. write in exponent form 2^2 + 3^3 + 5^1....

--3. add 1 to each exponet

--4. multiply all the new exponets

--5. that number is amount of factors

--OR
