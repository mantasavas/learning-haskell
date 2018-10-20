module BattleShip where

import Parser
import GameData

printSimple :: String -> String
printSimple msg = msg

-- Resolves errors
resolveEithers :: Either String a -> a
resolveEithers (Right msg) = msg

getPlayersShots :: Either String MoveMsg -> [MoveMsg]
getPlayersShots msg = playerMoves "coord" $ (resolveEithers msg)

resolveError :: Either String a -> String
resolveError (Left msg) = msg

-- Checks if we reached the end of data structure recursively
isTheEnd :: MoveMsg -> Bool
isTheEnd (DicMsg msg) = False
isTheEnd (CordValue "null") = True

------- Checking if the game end symbol is the last one ------

checkEmptyCoord :: MoveMsg -> Bool
checkEmptyCoord (CordList []) = True  
checkEmptyCoord (CordList _) = False

checkGameEndSymbol :: [MoveMsg] -> Bool
checkGameEndSymbol [] = False
checkGameEndSymbol (head:tail) | checkGameEndSymbol tail == True = True
                               | checkEmptyCoord head == True = True
                               | otherwise = False

checkIfFirstEndSymb :: [MoveMsg] -> Bool
checkIfFirstEndSymb (head:tail) | checkEmptyCoord head == True && checkGameEndSymbol tail == False = True
                                | checkEmptyCoord head == False && checkGameEndSymbol tail == False = True
                                | otherwise = False

-------------------------------------------------------------

getCoorValue :: MoveMsg -> String
getCoorValue (CordList [CordValue x, CordValue y]) = x ++ y
getCoorValue (CordList []) = ""

firstPlayerCoord :: ([MoveMsg], [MoveMsg]) -> [MoveMsg]
firstPlayerCoord (msg, _) = msg

secondPlayerCoord :: ([MoveMsg], [MoveMsg]) -> [MoveMsg]
secondPlayerCoord (_, msg) = msg

-- Based on key, returns value of MoveMsg data structure
getKeyValue :: String -> MoveMsg -> (String, MoveMsg)
getKeyValue ("coord") (DicMsg msg) = msg!!0
getKeyValue ("result") (DicMsg msg) = msg!!1
getKeyValue ("prev") (DicMsg msg) = msg!!2


-- Can return either all the cordinates or shoot results of both players combined
-- Recursively iterrates through MoveMsg and returns player moves
playerMoves :: String -> MoveMsg -> [MoveMsg]
playerMoves key msg = accCoordinates msg
  where
    accCoordinates :: MoveMsg -> [MoveMsg]
    accCoordinates msg =
      let
        (_, nextMsg) = getKeyValue "prev" msg 
        (_, result) = getKeyValue key msg
      in
        if isTheEnd nextMsg then result:[]
        else result:(accCoordinates nextMsg)

-- Returns shooting coordinates of both players separated
getShootingCoordinates :: [MoveMsg] -> [MoveMsg] -> [MoveMsg] -> ([MoveMsg], [MoveMsg])
getShootingCoordinates [] accFirsPlayer accSecondPlayer = (accFirsPlayer, accSecondPlayer)
getShootingCoordinates (head:tail) accFirsPlayer accSecondPlayer  
  | length tail `mod` 2 == 0 = (getShootingCoordinates tail (head:accFirsPlayer) accSecondPlayer)
  | otherwise = (getShootingCoordinates tail accFirsPlayer (head:accSecondPlayer))


countPlayerCoordinates :: Either String ([MoveMsg], [MoveMsg]) -> Either String (Int, Int)
countPlayerCoordinates shootingCoords =
  let
    (firstPlayer, secondPlayer) | isRight(shootingCoords) == "True" = resolveEithers shootingCoords
  in
    if isRight(shootingCoords) == "True"
      then Right (100 - (length firstPlayer), (100 - (length secondPlayer)))
    else Left (resolveError shootingCoords)

-- Combines coordinate, so that later it could be easy to compare if before A 1 seperated => A1
combineCoordinate :: [MoveMsg] -> [String]
combineCoordinate [] = []
combineCoordinate (head:tail) = (getCoorValue head):(combineCoordinate tail)


-- Every time it comes back checks if True, if true, doesn't check anymore
-- Checks for multiple shots, if true wrong!
checkMultipleShots :: [MoveMsg] -> Bool
checkMultipleShots [] = False
checkMultipleShots (head:tail)  | checkMultipleShots tail == True = True
                                | otherwise = ((getCoorValue head) `elem` (combineCoordinate tail)) 


checkGameLogic :: Either String ([MoveMsg], [MoveMsg]) -> Either String MoveMsg -> (Bool, Bool, Bool)
checkGameLogic shootingCoords parsedMessage =
  let
    -- Checking for duplicate shots for first player
    firstPlayerDup = checkMultipleShots (firstPlayerCoord (resolveEithers shootingCoords))

    -- Checking for duplicate shots for second player
    secondPlayerDup = checkMultipleShots (secondPlayerCoord (resolveEithers shootingCoords))

    -- checking for end symbol, must be at the end!
    endSymbolCorrect = (checkGameEndSymbol (getPlayersShots parsedMessage) == True) && (checkIfFirstEndSymb (getPlayersShots parsedMessage) == False)
  in
    (firstPlayerDup, secondPlayerDup, endSymbolCorrect)


resolveCheckGameLogic :: (Bool, Bool, Bool) -> Either String String
resolveCheckGameLogic (True, _, _) = Left("Game end symbol is in wrong position! Must be at the end")
resolveCheckGameLogic (_, True, _) = Left("First Player duplicate shots!")
resolveCheckGameLogic (_, _, True) = Left("Second Player duplicate shots!")
resolveCheckGameLogic (_, _, _) = Right("All Logic Valid")


-- Finds number of moves available for a players
available :: String -> Either String (Int, Int)
available msg =
  let
    parsedMessage = parseMessage msg
    shootingCoords  | isRight(parsedMessage) == "False" = Left (resolveError parsedMessage)
                    | otherwise = Right (getShootingCoordinates (getPlayersShots parsedMessage) [] [])
    
    (firstPlayerDup, secondPlayerDup, endSymbolCorrect) | isRight(shootingCoords) == "True" = checkGameLogic shootingCoords parsedMessage
                                                        | otherwise = (False, False, False)
    logicError = resolveCheckGameLogic(endSymbolCorrect, firstPlayerDup, secondPlayerDup)
  in
    -- Checks for all the logical error in message
    if isRight(logicError) == "False"
      then Left(resolveError logicError)
      -- Checks if JSON is correct format
      else if isRight(shootingCoords) == "False"
        then Left(resolveError shootingCoords)
        -- If finds out that end symbol exist, end game symbol [], no available moves!
        else if checkGameEndSymbol (getPlayersShots parsedMessage) == True
          then Right(0,0)
          -- If everything is okey, main scenario, return available moves for both players
          else countPlayerCoordinates shootingCoords