-- start with a list of module monomials, generate the frame

-- given the frame, generate the Schreyer order, corresponding monomials/tie breakers.

-- monomial --> heft degree
-- monomial --> degree
-- monomial, monomial --> monomial (quotient, lcm)

-- given monomial, find divisor: Just (Int, Monomial)

-- given (degree,level), compute syzygies at that location

module Monomials
    (
     Monomial,
     isWellDefinedMonomial,
     fromExponents,
     mult,
     lcmMonomial,
     quotientMonomial
    ) where

data Monomial = Monom [(Int, Int)] deriving (Show, Eq, Ord)

fromExponents :: [Int] -> Monomial
fromExponents xs = Monom (fromExp1 0 xs)

isWellDefinedMonomial :: Monomial -> Bool
isWellDefinedMonomial (Monom m) = isWellDefinedMonom m

mult :: Monomial -> Monomial -> Monomial
mult (Monom m1) (Monom m2) = Monom (mult1 m1 m2)

lcmMonomial :: Monomial -> Monomial -> Monomial
lcmMonomial (Monom m1) (Monom m2) = Monom (lcm1 m1 m2)

quotientMonomial :: Monomial -> Monomial -> Monomial
quotientMonomial (Monom m1) (Monom m2) = Monom (quotient1 m1 m2)

isWellDefinedMonom :: [(Int,Int)] -> Bool
isWellDefinedMonom [] = True
isWellDefinedMonom [(v,e)] = (v >= 0) && (e > 0)
isWellDefinedMonom ((v1,e1):(m@((v2,e2):xs))) = v1 < v2 && e1 > 0 && isWellDefinedMonom m

fromExp1 v [] = []
fromExp1 v (e:es)
    | e == 0 = fromExp1 (v+1) es
    | e /= 0 = (v,e):(fromExp1 (v+1) es)

mult1 :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
mult1 m [] = m
mult1 [] m = m
mult1 m1@((v1,e1):xs) m2@((v2,e2):ys)
    | v1<v2 = (v1,e1):(mult1 xs m2)
    | v1>v2 = (v2,e2):(mult1 m1 ys)
    | v1==v2 = (v1,e1+e2):(mult1 xs ys)

lcm1 :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
lcm1 m [] = m
lcm1 [] m = m
lcm1 m1@((v1,e1):xs) m2@((v2,e2):ys)
    | v1<v2 = (v1,e1):(lcm1 xs m2)
    | v1>v2 = (v2,e2):(lcm1 m1 ys)
    | v1==v2 = (v1,max e1 e2):(lcm1 xs ys)
               
quotient1 :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
quotient1 m [] = m
quotient1 [] m = []
quotient1 m1@((v1,e1):xs) m2@((v2,e2):ys)
    | v1<v2 = (v1,e1):(quotient1 xs m2)
    | v1>v2 = quotient1 m1 ys
    | (v1==v2) = if e1 >= e2
                 then (v1,e1-e2):(quotient1 xs ys)
                 else quotient1 xs ys
               
monom1 = Monom [(1,1),(3,4)]
monom2 = Monom [(1,2),(2,1)]

exp1 = [0,1,3,0,4,2] :: [Int]         
