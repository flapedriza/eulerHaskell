--Find the sum of all the multiples of 3 or 5 below 1000.
multiplesSum :: Int
multiplesSum = sum [x | x<-[1..999], (mod x 3 == 0) || (mod x 5 == 0)]

--By considering the terms in the Fibonacci sequence whose values do not exceed
-- four million, find the sum of the even-valued terms.
fibSum :: Integer
fibSum = sum (filter (even) $ takeWhile (<4000000) [quickFib x | x<-[1..]])
    where
        quickFib :: Integer -> Integer
        quickFib n = fib n (0,1)
        fib n (n1,n2)
            | n == 0 = n1
            | otherwise = fib (n-1) (n2, n1+n2)

--What is the largest prime factor of the number 600851475143 ?
largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = foldr1 (max) $ primeFactors n
    where
        primeFactors n = addivs n 2
        addivs 1 _ = []
        addivs n n2
            |mod n n2 == 0 = n2:addivs (div n n2) n2
            |otherwise = addivs n (n2+1)

--Find the largest palindrome made from the product of two 3-digit numbers.
largestPalindrome :: Integer
largestPalindrome = maximum [x*y | x<-[100..999], y<-[100..999], (reverse (show (x*y)) == show (x*y))]

--What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
lcmList :: [Integer] -> Integer
lcmList xs = foldr1 (lcm') xs
    where
        lcm' 0 _ = 0
        lcm' _ 0 = 0
        lcm' n n2 = abs ((div n (gcd' n n2)) * n2)
        gcd' n 0 = n
        gcd' n n2 = gcd' n2 (mod n n2)

--Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
squareDiff :: Integer -> Integer
squareDiff n = (squareSum n) - (sumSquares n)
    where
        squareSum n = (sum [1..n])^2
        sumSquares n = sum [x^2 | x<-[1..n]]

--What is the 10 001st prime number?
nThPrime :: Int -> Integer
nThPrime n = primes!!(n-1)
    where
        primes = let siege (x:xs) = x : siege (filter (\y -> mod y x /= 0) xs)
            in siege [2..]

--Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?
bigNumber = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
adjacentProduct :: Int
adjacentProduct = maximum (mult13 bigNumber)
mult13 :: String -> [Int]
mult13 [] = []
mult13 (x:xs)
    |length xs < 12 = []
    |otherwise = (fromEnum x - fromEnum '0')*(product (map (read.(:"")) (take 12 xs))) : mult13 xs

--There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.
tripletProduct :: Integer
tripletProduct = let [(a,b,c)] = [(x,y,z) | z<-[1..500], x<-[1..z], y<-[1..z], x<y, x+y+z==1000, x^2+y^2==z^2] in a*b*c

--Find the sum of all the primes below two million.
sumPrimes :: Int -> Int
sumPrimes n = sum (siege [2..n])
siege [] = []
siege (x:xs) = x : siege (filter (\y -> mod y x /= 0) xs)
