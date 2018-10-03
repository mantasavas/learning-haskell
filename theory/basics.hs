module Lesson1 where

import Data.List
import Data.Char
  

add14 :: Integer -> Integer
add14 = add 14

add :: Integer -> Integer -> Integer
add a b = a + b


calculate :: Integer -> Integer -> Integer
calculate = f 1
  where
     f :: Integer -> Integer -> Integer -> Integer
     f a b c = a + b + c

kita2 :: Integer -> Integer
kita2 a =
  let
    b = 4
    c = 6
  in
    a + b + c

canBuyAlcohol :: Integer -> Bool
canBuyAlcohol a =
  if a <= 20 then True else False

l1 = [4]
l2 = 5 : l1


len :: [a] -> Integer
len [] = 0
len (_:t) = 1 + len t


lenFold :: [a] -> Integer
lenFold = foldl f 0
  where
    f :: Integer -> b -> Integer
    f acc _ = acc + 1


-- anonymous function, sign of it is: \....

-- (1 argumentas) => funkciją, kuri iš sukauptos (akumuliuotos) reikšmės ir iš eilės sekančio
-- sąrašo elemento pagamina naują sukauptą reikšmę

-- (2 argumentas) => ,,nulinį`` elementą, kuris bus panaudotas kaip rezultatas, jei sąrašas tuščias,
-- arba kaip sukauptas elementas apdorojant pirmą elementą 

-- (3 argumentas) => patį sąrašą
sumOfList :: [Integer] -> Integer
sumOfList = foldl (\acc el -> acc + el) 1


-- It can be imagined as if you take each element from basket s and throw to
-- anonymous function \el -> el + 5 
incBy5 :: [Integer] -> [Integer]
incBy5 s = map (\el -> el + 5) s
--incBy5 = map (+ 5)
--incBy5 = map (add 5)
--incBy5 = map (`add` 5)


-- type is reserved word, for defining your types, array of tuples
type Map = [(Integer, String)]

dict :: Map
dict = [(1, "vienas"), (2, "du")]

search :: Integer -> Map -> Maybe String
search _ [] = Nothing
search k ((k',r):t) = if k == k' then Just r else search k t

-- drop (length lenghtAsStr) msg
-- makes from "554:labas42" to ":labas42"
parseStr :: String -> (String, String)
parseStr msg = readStr len $ drop (length lenghtAsStr) msg
  where
    lenghtAsStr = takeWhile isDigit msg
    len :: Int
    len = read lenghtAsStr
    readStr :: Int -> String -> (String, String)
    readStr n (':':m) = (take n m, drop n m)
    readStr _ _ = error "Colon expected"


parseTuple :: String -> ((String, String), String)
parseTuple msg =
  let
    (r1, l1) = parseStr msg
    (r2, l2) = parseStr l1
  in
    ((r1, r2), l2)
    
parseStringList :: String -> ([String], String)
parseStringList ('[':m) = parseElement m []
  where
    -- goes until it finds [5:labas2:sd3:asd4:masa] last ] bracket
    parseElement (']':l) acc = (reverse acc, l)
    parseElement z acc =
      let
        (r, l) = parseStr z 
      in
        -- adding reminder, at first acc is [], and we add and add
        parseElement l (r:acc)
parseStringList _ = error "Square bracket expected"
  
-- searchE 3 [(2, "Mantas"), (3, "Karolis"), (100, "Martynas")]
searchE :: Integer -> [(Integer, String)] -> Either String String
searchE _ [] = Left "Not found"
searchE k ((k',r):t) = if k == k' then Right r else searchE k t


data Tipas = A | B | C
  deriving Show

-- FireExtinguisher :: Tipas -> Int -> FireExtinguisher
data FireExtinguisher = FireExtinguisher Tipas Int
  deriving Show

-- :t Mantas gives error
-- :t Mantitas shows type constructor (methods => returning type values) with definition: Mantitas :: Tipas -> Int -> Mantas
data Mantas = Mantitas Tipas Int
  deriving Show



-- Tuples and Lists allows to store mutliple values into single value

-- Tuples:
-- 1) Can store different type of values
-- 2) Tuple size is fized (immutable), therefore, it before creating you must know in advance the size
-- 3) Examples:
-- (True, 1)
-- ("Hello world", False)
-- (4, 5, "Six", True, 'b')

-- Lists:
-- 1) Can store only one type values in it
-- 2) Size is not fixed
-- 3) Examples:
-- [1,2,3,4]
-- ["here", "are", "some", "strings"]


-- Tuples and Lists can be nested:
-- [(1,2), (3,4), (5,6)]
-- ((2,3), [2,3])


-- ADT can be recursive
-- Msg is considered type, like Integer, Int etc.
-- MsgDict, MsgList and MsgString are all type values like 4, 584
data Msg = MsgDict [(String, Msg)] 
    -- [Msg] tells that is going to be a list of any type values that Msg type has: MsgDict, MsgList, MsgString, its recursive
    | MsgList [Msg]
    | MsgString String
  deriving Show

firstMove :: Msg
firstMove = MsgDict [("coord", MsgList [MsgString "A", MsgString "1"])]


fewMoves :: Msg
fewMoves = MsgDict [
      ("coord", MsgList [MsgString "A", MsgString "1"])
    , ("result", MsgString "MISS")
    , ("prev", firstMove)
  ]


-- data type: Extinguisher
-- data value: Extinguishers
data Extinguisher = Extinguishers {
    tipas :: Tipas
  , talpa :: Int
} deriving Show

defaultE10r :: Extinguisher
defaultE10r = Extinguishers {tipas = C, talpa = 5}

-- It automatically generates getters and setters, so we can simply use to get value:
-- talpa defaultE10r


-- data List = Nil | Cons Integer List
--   deriving Show

-- constructor Cons takes two arguments:
-- a - integer
-- List a - because this constructor returns List a type value, it can be passed as parameter to constructor as if recursion
-- And returns List a
data List a = Nil | Cons a (List a)
  deriving Show


-- emptyList = Nil
-- oneElList = Cons 1 emptyList
-- twoElList = Cons 2 oneElList
-- threeElList = Cons 3 twoElList