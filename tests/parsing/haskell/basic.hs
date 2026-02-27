module Main where

import Data.List (sort)
import qualified Data.Map as Map

-- Function definition
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Data type
data Color = Red | Green | Blue
  deriving (Show, Eq)

-- Record type
data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show)

-- Type class
class Container f where
  empty :: f a
  insert :: a -> f a -> f a

-- Instance
instance Container [] where
  empty = []
  insert x xs = x : xs

-- Type alias
type Name = String

-- Guards
bmi :: Double -> String
bmi x
  | x < 18.5  = "underweight"
  | x < 25.0  = "normal"
  | x < 30.0  = "overweight"
  | otherwise  = "obese"

-- Where clause
circleArea :: Double -> Double
circleArea r = pi * r * r
  where pi = 3.14159

-- Let expression
cylinderVolume :: Double -> Double -> Double
cylinderVolume r h =
  let base = pi * r * r
  in base * h
  where pi = 3.14159

-- Lambda
double :: [Int] -> [Int]
double = map (\x -> x * 2)

-- List comprehension
evens :: [Int] -> [Int]
evens xs = [x | x <- xs, even x]

-- Do notation
main :: IO ()
main = do
  putStrLn "Hello, World!"
  let x = 42
  print x

-- Case expression
describe :: Color -> String
describe c = case c of
  Red   -> "red"
  Green -> "green"
  Blue  -> "blue"

-- Pattern matching with as-pattern
firstTwo :: [a] -> [a]
firstTwo xs@(_:_:_) = take 2 xs
firstTwo xs = xs

-- Newtype
newtype Wrapper a = Wrapper { unwrap :: a }

-- Infix operator
infixl 6 |+|
(|+|) :: Int -> Int -> Int
x |+| y = x + y + 1
