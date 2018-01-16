import Data.List

{-squaresUpTo n = aux 1
    where aux i
            |square <= n = square: aux (i+1)
            |otherwise   = []
            where square = i^2
-}


--1
productTrips xs = 
    let inOrder = sort xs
    in [(x,y,z) | x <- inOrder, y <- inOrder, let z = x * y, x < y, z `elem` inOrder]

--2
myintersect :: (Eq a) => [a] -> [a] -> [a]
myintersect xs ys = [ x | x <- xs, y <- ys, x == y]

--3
findMultiplesOfPrimes pr muls = [ findMultiples x muls | x <- pr]
findMultiples p muls = (p, [ x | x <- muls, x `mod` p == 0])

--4
fib 0 = 1
fib 1 = 1
fib n = fibHelper (fib 0) (fib 1) 2 n
fibHelper bprev prev cur n  
    |cur < n   = fibHelper prev (bprev +prev) (cur + 1) n
    |otherwise = bprev + prev

--5
flatmap fx [] = []
flatmap fx (x:xs) = (fx x)++(flatmap fx xs)

--6
findBest fx (x:xs) = helper (x, fx x) xs
    where
        helper (best,val) [] = best
        helper (best,val) (x:xs) 
            |temp > val = helper (x, temp) xs 
            |otherwise      = helper (best,val) xs
            where temp = fx x
