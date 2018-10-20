module Parser where

import Data.List
import Data.Char

------------------------ Defining data type ----------------------

data MoveMsg = DicMsg [(String, MoveMsg)]
              | CordList [MoveMsg] 
              | CordValue String  
  deriving Show


--------------- Some examples of storing data in MoveMsg data type -----------------
firstMove :: MoveMsg
firstMove = DicMsg [
    ("coord",  CordList[CordValue "A", CordValue "10"]),
    ("result", CordValue "null"),
    ("prev",   CordValue "null")
  ]

secondMove :: MoveMsg
secondMove = DicMsg [
    ("coord",  CordList[CordValue "D", CordValue "7"]),
    ("result",  CordValue "HIT"),
    ("prev",    CordValue "null")
  ]

parseValue :: String -> String
parseValue value = value


parseStr :: String -> (String, String)
parseStr msg = readStr len $ drop (length lenghtAsStr) msg
  where
    lenghtAsStr = takeWhile isDigit msg
    len :: Int
    len = read lenghtAsStr
    readStr :: Int -> String -> (String, String)
    readStr n (':':m) = (take n m, drop n m)
    readStr _ _ = error "Colon expected"


parseStringList :: String -> ([String], String)
parseStringList ('[':m) = parseElement m []
  where
    -- goes until it finds [5:labas2:sd3:asd4:masa] last ] bracket
    -- result (["labas","sd","asd","masa"],"")
    parseElement (']':l) acc = (reverse acc, l)
    parseElement z acc =
      let
        (r, l) = parseStr z 
      in
        -- adding reminder, at first acc is [], and we add and add
        parseElement l (r:acc)
parseStringList _ = error "Square bracket expected"


resolveEither :: Either String (String, String, String) -> (String, String, String)
resolveEither (Right result) = result

resolveFault :: Either String (String, String, String) -> String
resolveFault (Left fault) = fault

resolveShotError :: Either String MoveMsg -> String
resolveShotError (Left msg) = msg

resolveCordinate :: Either String MoveMsg -> MoveMsg
resolveCordinate (Right coord) = coord

resolveMassive :: Either String [String] -> [String]
resolveMassive (Right hashValues) = hashValues

isRight :: Either a b -> String
isRight (Left _) = "False"
isRight (Right _) = "True"

-- Check hash ending, make sure its ether , or }, because later it just drops
checkHashEnding :: (String, String, String) -> Either String (String, String, String)
checkHashEnding (key, value, (',':ending))  | key == "coord" = Right (key, value, ending)
                                            | key == "result" = Right (key, value, ending)
                                            | otherwise = Left ("Missing either coord or result key!")
checkHashEnding (key, value, ('}':ending))  | key == "prev" = Right (key, value, ending)
                                            | otherwise = Left ("Missing preveous move key")
checkHashEnding (_, _, _) = Left ("Missing either comma after coord or result, or closing curly bracket")

-- parseKey "\"coord\":[\"D\",\"7\"]"
splitBlock :: String -> Either String (String, String, String)
splitBlock key  | (take 9 key) == "\"coord\":[" = checkHashEnding ("coord", cordValue, cordEnd)
                | (take 10 key) == "\"result\":\"" = checkHashEnding ("result", resultValue, resEnd)
                | (take 8 key) == "\"prev\":{" = checkHashEnding ("prev", prevValue, prevEnd)
                | (take 13 key) == "\"result\":null" = checkHashEnding ("result", "null", resNullEnd)
                | (take 11 key) == "\"prev\":null" = checkHashEnding ("prev", "null", preNullEnd)
                | otherwise = Left "Wrong message format! Check these words: coord, result, prev. Maybe missing double quotes?"
                  where
                    -- Cordinates allways exists, and we know it ends with ] for sure
                    -- Parsing values of hash
                    cordValue = takeWhile (\x -> x /= ']') ( drop 9 key)
                    resultValue = takeWhile (\x -> x /= '"') ( drop 10 key)
                    prevValue = take ((length key) - 8) (drop 7 key)

                    -- Parsing remaining part (which is goint to be parsed later recursevily)
                    cordEnd = drop ((length cordValue) + 1 + 9) key
                    resEnd = drop ((length resultValue) + 1 + 10) key
                    prevEnd = drop ((length key) - 8 + 7) key
                    resNullEnd = drop (13) key
                    preNullEnd = drop (11) key


prints :: String -> [String]
prints msg = [msg] 

parseMessageBlock :: String -> Either String [String]
parseMessageBlock ('{':remainingBlock) = parseValues [] remainingBlock
  where
    -- returns parsed hash key values: ["["D","7"]", "parseShotHIT", "{"coord":["A","10"],"result":null,"prev":null}"]
    -- which later could passed to my created data types

    parseValues :: [String] -> String -> Either String [String]
    parseValues accValues "" = Right accValues 
    -- if we didn't reached the end of JSON block, continue cutting the string...
    parseValues accValues remainingBlock = 
      let
        val :: Either String (String, String, String)
        val = splitBlock remainingBlock
        -- must return parsed hash value, and cutted string of remaining block
        (hash, value, rem) | isRight (val) == "True" = resolveEither val
      in
        -- add that parsed value to accumulated values accValues, and call recursively again with remaining block
        if isRight (val) == "True"
          then parseValues (value:accValues) rem
        else Left (resolveFault val)
parseMessageBlock _ = Left "Missing opening curly brackets!"


-- It can only have empty coodinates at the begining 
parseCordinates :: String -> Either String (String, MoveMsg)
parseCordinates "" = Right ("coord", CordList[])
parseCordinates coord = parsedCord
  where
    coordx = ["\"A\"", "\"B\"", "\"C\"", "\"D\"", "\"E\"", "\"F\"", "\"G\"", "\"H\"", "\"I\"", "\"J\""]
    coordy = ["\"1\"", "\"2\"", "\"3\"", "\"4\"", "\"5\"", "\"6\"", "\"7\"", "\"8\"", "\"9\"", "\"10\""]
   
    valx = takeWhile (/= ',') (coord)
    valy = drop 4 coord

    parsedX | valx `elem` coordx = Right (CordValue (take 1 (drop 1 valx)))
            | otherwise = Left("Cordx is not in [A..J] list, or missing double quotes!")
    
    parsedY | valy `elem` coordy = Right (CordValue (take ((length valy) - 2) (drop 1 valy)))
            | otherwise = Left("Cordx is not in [1..10] list, or missing double quotes!")
    
    parsedCord | (isRight(parsedX) == "True") && (isRight(parsedY) == "True") = 
                  Right ("coord", CordList[resolveCordinate parsedX, resolveCordinate parsedY]) 
               | isRight(parsedX) == "False" = Left ( resolveShotError parsedX )
               | isRight(parsedY) == "False" = Left ( resolveShotError parsedY )
               | otherwise = Left "Something went wrong in parsing coordinates!" 

parseShot :: String -> Either String (String, MoveMsg)
parseShot shot = parsedShot
  where
    withoutQ  | (shot == "MISS") || (shot == "HIT") || (shot == "null") = Right ( CordValue shot )
              | otherwise = Left ("Wrong shot message specified, either Miss or Hit. Make sure your spelling correct!")  
    
    parsedShot  | isRight(withoutQ) == "True" = Right ("result",  resolveCordinate withoutQ)
                | isRight(withoutQ) == "False" = Left (resolveShotError withoutQ)
                | otherwise = Left "Something went wrong in parsing shot message!"


resolveDict :: Either String (String, MoveMsg) -> String
resolveDict (Right (_, CordValue msg)) = msg

resolveResult :: Either String (String, MoveMsg) -> MoveMsg
resolveResult (Right (_, msg)) = msg

resolvePrevious :: Either String MoveMsg -> MoveMsg
resolvePrevious (Right msg) = msg

resolveBlockError :: Either a b -> a
resolveBlockError (Left a) = a

parseMessage :: String -> Either String MoveMsg
parseMessage msg = parseBlocks msg -- parseBlocks msg accMessage
  where
    parseBlocks :: String -> Either String MoveMsg
    parseBlocks msg =
      let
        parsedBlock = parseMessageBlock msg
        shotVal = parseShot ((resolveMassive parsedBlock)!!1)
        coordVal = parseCordinates ((resolveMassive parsedBlock)!!2)
        prevVal = (resolveMassive parsedBlock)!!0
        previous  | isRight parsedBlock == "False" = Left (resolveBlockError parsedBlock)
                  | isRight shotVal == "False" = Left ( resolveBlockError shotVal )
                  | isRight coordVal == "False" = Left ( resolveBlockError coordVal )
                  | prevVal /= "null" && (resolveDict shotVal) /= "null" = parseBlocks prevVal
                  | (prevVal == "null") && (resolveDict shotVal) == "null" = Right(CordValue "null")
                  | otherwise = (Left "Someting went wrong! Make sure that prev and shot are both null or not null")
      in
        if (isRight previous) == "True"
          then do  
            Right (DicMsg [
                ("coord",  resolveResult coordVal),
                ("result", resolveResult shotVal),
                ("prev",   resolvePrevious previous)
              ])
        else previous