module MyModule where

import Data.List
import Data.Char

-- Cordinates Of Shot:

-- {
-- 	"coord": ["D", "7"],
-- 	"result": "HIT",
-- 	"prev": {
-- 		"coord": ["A", "10"],
-- 		"result": null,
-- 		"prev": null
-- 	}
-- }

-- {"coord":["D","7"],"result":"HIT","prev":{"coord":["A","10"],"result":null,"prev":null}}

-- data MyBool a = MyFalse | MyTrue a
--   deriving Show

data MoveMsg = DicMsg [(String, MoveMsg)]
              | CordList [MoveMsg] 
              | CordValue String
              | Exist (Maybe MoveMsg)
  deriving Show


-- Some examples of parsing data
firstMove :: MoveMsg
firstMove = DicMsg [
    ("coord",  CordList[CordValue "A", CordValue "10"]),
    ("result", Exist Nothing),
    ("prev",   Exist Nothing)
  ]

secondMove :: MoveMsg
secondMove = DicMsg [
    ("coord",  CordList[CordValue "D", CordValue "7"]),
    ("result",  CordValue "HIT"),
    ("prev",    firstMove)
  ]

