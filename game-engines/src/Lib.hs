{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where
import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Lens
import Data.Char
import Data.Conduit
import Data.List(intercalate)
import Data.Maybe
import Data.Monoid
import System.Environment
import System.IO
import Text.Printf.TH
import qualified Data.Aeson.TH as AT
import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

-- Definition of a "Game"
#define deriveAllJSONs ((concat <$>) . mapM (AT.deriveJSON (AT.defaultOptions { AT.allNullaryToStringTag = False })))

newtype PlayerId = PID Int deriving (Eq, Num, Show)

data GameStatus = CurrentTurn PlayerId | Drawn | PlayerWin PlayerId deriving Show
deriveAllJSONs [''PlayerId, ''GameStatus]

data Game state move = Game {
    _blankState :: state,
    _checkStatus :: state -> GameStatus,
    _makeMove :: move -> state -> Maybe state,
    _renderState :: state -> T.Text, -- output is SVG
    _validMoves :: state -> [move]
    }

makeLenses ''Game

-- generic game loop
infixl 1 `bindJust`
f `bindJust` g = f >>= maybe (return ()) g

inspectConduit = await `bindJust` \x -> liftIO (print x) >> yield x

runByteStringConduit :: Handle -> Handle -> ConduitM B.ByteString B.ByteString IO () -> IO ()
runByteStringConduit i o c = runConduit $
    CB.sourceHandle i =$=
    CB.lines =$=
    c =$=
    CB.sinkHandle o

gameLoop :: (ToJSON s, ToJSON m, FromJSON s, FromJSON m) => Game s m -> ConduitM B.ByteString B.ByteString IO ()
gameLoop g = loop (g ^. blankState) where
    emit = yield . L.toStrict . (<>"\n") . encode
    loop state = do
        liftA2 (>>=) await (pure (decode . L.fromStrict)) >>= \case
            Just (jsonLine :: Value) -> do
                pure (jsonLine ^? nth 0 . _String) `bindJust` \case
                    "reset" -> loop (g ^. blankState)
                    "getState" -> emit state >> loop state
                    "renderState" -> emit ((g ^. renderState) state) >> loop state
                    "checkStatus" -> emit ((g ^. checkStatus) state) >> loop state
                    "makeMove" -> pure (jsonLine ^? nth 1 . _JSON) `bindJust` \(move :: m) ->
                        pure ((g ^. makeMove) move state) >>= maybe (loop state) loop
                    "validMoves" -> emit ((g ^. validMoves) state) >> loop state
                    _ -> loop state
            Nothing -> loop state

main = getArgs >>= \case
    ["rps"] -> runGame rockPaperScissors
    ["tron"] -> runGame (tronBikeGame (20,20))
    ["tron", x, y] -> runGame (tronBikeGame (read x, read y))
    _ -> putStrLn "Options: rps, tron [x y]"
    where runGame = runByteStringConduit stdin stdout . gameLoop 

-- 2-player Rock-paper-scissors
data RPSMove = Rock | Paper | Scissors deriving (Eq, Show)
data RPSState = ZeroProvided | OneProvided RPSMove | TwoProvided RPSMove RPSMove deriving Show

rpsCompare Rock Paper = LT
rpsCompare Paper Scissors = LT
rpsCompare Scissors Rock = LT
rpsCompare x y | x == y = EQ
rpsCompare _ _ = GT

rockPaperScissors :: Game RPSState RPSMove
rockPaperScissors = Game {
    _blankState = ZeroProvided,
    _checkStatus = \case
        ZeroProvided -> CurrentTurn 0
        OneProvided _ -> CurrentTurn 1
        TwoProvided l r -> case l `rpsCompare` r of
            LT -> PlayerWin 1
            EQ -> Drawn
            GT -> PlayerWin 0,
    _makeMove = \m -> \case
        ZeroProvided -> Just $ OneProvided m
        OneProvided m1 -> Just $ TwoProvided m1 m
        TwoProvided _ _ -> Nothing,
    _renderState = \x -> "<svg width='100' height='50'><text x='0' y='25' fill='black'>" <> T.pack (show x) <> "</text></svg>",
    _validMoves = \case
        TwoProvided _ _ -> []
        _ -> [Rock, Paper, Scissors]
    }


-- 2-player Tron
data TronMove = KeepGoing | TurnCW | TurnCCW deriving Show
data TronDirection = North | East | South | West deriving (Enum, Show)
newtype TronCoord = TC (Int, Int) deriving (Eq, A.Ix, Ord, Show)
newtype TronPlayer = TP (TronCoord, TronDirection) deriving Show
newtype TronGrid = TG (A.Array TronCoord (Maybe PlayerId)) deriving Show
data TronState = TS TronPlayer TronPlayer TronGrid deriving Show

unTC (TC x) = x

applyTurn :: TronMove -> TronDirection -> TronDirection
applyTurn dir x = toEnum ((fromEnum x + f dir) `mod` 4) where
        f KeepGoing = 0; f TurnCW = 1; f TurnCCW = -1

getDelta North = TC ( 0,-1)
getDelta South = TC ( 0, 1)
getDelta West  = TC (-1, 0)
getDelta East  = TC ( 1, 0)

updatePosition (TC (x,y)) dir = let TC (dx, dy) = getDelta dir in TC (x+dx, y+dy)

tronCheckStatus (TS (TP (p1,d1)) (TP (p2,d2)) (TG board)) = let
    [alive1, alive2] = map (isNothing . (board A.!)) [p1,p2]
    in case (alive1, alive2) of
        (True,True) -> CurrentTurn 0
        (True,False) -> PlayerWin 0
        (False,True) -> PlayerWin 1
        (False,False) -> Drawn

tronBikeGame :: (Int, Int) -> Game TronState (TronMove, TronMove)
tronBikeGame size@(width, height) = Game {
    _blankState = TS (TP (TC (0,0), South)) (TP (TC size, North)) (TG (A.listArray (TC (0,0),TC size) (repeat Nothing))),
    _checkStatus = tronCheckStatus,
    _makeMove = \(m1,m2) state@(TS (TP (p1,d1)) (TP (p2,d2)) (TG board)) -> case tronCheckStatus state of
        CurrentTurn _ -> let
            [d1', d2'] = zipWith applyTurn [m1,m2] [d1,d2]
            [p1', p2'] = zipWith updatePosition [p1, p2] [d1', d2']
            in Just (TS (TP (p1',d1')) (TP (p2', d2')) (TG (board A.// [(p1,Just 0), (p2,Just 1)])))
        _ -> Nothing,
    _renderState = \(TS p1 p2 (TG board)) -> let
{-
(echo '["renderState"]' | ./build_and_run.sh tron) > foo.svg
(python -c 'print """["makeMove", [{"tag":"KeepGoing","contents":[]},{"tag":"KeepGoing","contents":[]}]]\n"""*2 + """["renderState"]"""' | ./build_and_run.sh tron) > foo.svg
cat foo.svg | sed 's#\([^\]\)"#\1#g' | sed 's#^"##g' | sed 's#\\"#"#g' > fooprime.svg
-}
        cellSize = 32
        cellSize' = cellSize `div` 2
        header = [st|<svg width="%d" height="%d">|] (cellSize*(width+1)) (cellSize*(height+1))
        footer = [st|</svg>|]
        playerColor :: PlayerId -> T.Text
        playerColor 0 = "rgb(255,128,0)"
        playerColor 1 = "rgb(0,0,255)"
        playerColor _ = "rgb(128,128,128)" -- shouldn't happen, but define something visible to be safe
        mkStyle :: T.Text -> Float -> T.Text
        mkStyle = [st|style='fill:%s;stroke=black;stroke-width=6;fill-opacity:%f'|]
        renderCell (TC (x,y), Nothing) = [st|<rect x="%d" y="%d" width="%d" height="%d" %s />|]
            (x*cellSize) (y*cellSize) cellSize cellSize (mkStyle "white" 1.0)
        renderCell (TC (x,y), Just i) = [st|<rect x="%d" y="%d" width="%d" height="%d" %s />|]
            (x*cellSize) (y*cellSize) cellSize cellSize (mkStyle (playerColor i) 0.5)
        renderPlayer (TP (TC (x,y), dir)) i = [st|<circle cx="%d" cy="%d" r="%d" %s />|]
            (x*cellSize + cellSize') (y*cellSize + cellSize') cellSize' (mkStyle (playerColor i) 1)
        playerData = renderPlayer p1 0 <> renderPlayer p2 1
        in header <> foldMap renderCell (A.assocs board) <> playerData <> footer
        ,
    _validMoves = \state -> case tronCheckStatus state of
        CurrentTurn _ -> (liftA2 (,) <*> id) [KeepGoing, TurnCW, TurnCCW]
        _ -> []
    }

-- 2-player Tic-Tac-Toe
{-
data TTTMove = O | X deriving (Eq, Show, Enum, Ord)
type Position = (Char, Int)
data BoardMove = BoardMove
                { bMove :: TTTMove, bPos :: Position } 
                deriving (Eq, Show)
type Board = [BoardMove]
type InvalidMove = String

bsize = 3
coord = (['A'..],['1'..])

move :: BoardMove -> Board -> Either InvalidMove Board
move (BoardMove _ (c,r))[]=
    Left $ "Invalid move" ++ [c] ++ (show r)
move bm@(BoardMove nmov npos)(x:xs)
    | findMove x = Right $ bm:xs
    | otherwise =
      case move bm xs of
        Right r -> Right $ x:r
        err     -> err
      where findMove (BoardMove m p) =
                p == npos && isNothing m && nmov /= Nothing

win :: BoardMove -> Board -> Bool
win (BoardMove Nothing _) _ = False
win (BoardMove m (c,r)) b = row || col || diag' cb || diag' (reverse cb)
    where row = length
                (filter (\(BoardMove m2 (_,r2)) ->
                        m2 == m && r2 == r) b) == bsize
          col = length
                (filter (\(BoardMove m2 (c2,_)) ->
                        m2 == m && c2 == c) b) == bsize
          diag' xss = all (\(BoardMove m2 _) ->
                        m2 == m) $ diag xss
          cb = chop bsize b

draw :: BoardMove -> Board -> Bool
draw bm b = not (any (isNothing . bMove) b)

          && not (win bm b)

printBoard :: Board -> String
printBoard b = intercalate "\n" $
                map (\row-> [(fst. bPos) (row !! 0)] ++ ") |" ++
                    (intercalate " | "
                        $ map (\bm-> show $ bMove bm) row)
                    ++ " |")
                (chop bsize b)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

diag :: [[a]] -> [a]
diag xss = [xss !! n !! n | n <- [0 .. length xss -1]]

mainprime = do
    putStrLn "Starting new game. . ."
    putStrLn "Type 'quit' to exit game"
    let newBoard = []
        in do
            (putStrLn .  (\s->"\n"++s++"\n") . printBoard) newBoard
            gameloop Nothing newBoard

gameloop prevMove board = do
    let currPlayer = (\(BoardMove mv _) ->
                                case mv of
                                     X -> O
                                     O -> X) prevMove
    putStr $ "Player '" ++ (show currPlayer) ++ "': "
    hFlush stdout
    playerMove <- getLine
    case (playerMove, (map toUpper playerMove) `elem` allCoord) of
        ("quit", _) ->
            putStrLn "K. BYE."
        (_, False ) -> do
            putStrLn $ "Possible options: " ++ intercalate ", " allCoord
            gameloop prevMove board
        otherwise   -> do
            let pos = (toUpper $ playerMove !! 0,
                       read [(playerMove !! 1)] :: Int)
                currMove = BoardMove (Just currPlayer) pos
                currBoard = move currMove board
            either putStrLn (putStrLn . (\s->"\n"++s++"\n") . printBoard) currBoard
            case currBoard of
                Right r -> if win currMove r then do 
                                    putStrLn $ "Player '" ++ (show currPlayer) ++ "' wins!"
                                    main
                            else if draw currMove r then do 
                                putStrLn $ "No one wins, no one losses!"
                                main
                            else gameloop (Just currMove) r
                Left err -> gameloop prevMove board
    where allCoord = [[x] ++ show y | x <- take bsize (fst coord),
                                      y <- take bsize (snd coord)]
-}


{-data TTTState = A0taken | A1taken | A2taken|
                B0taken | B1taken | B3taken |
                C0taken | C1taken | C3taken deriving Show
-}
-- JSON hello world

{-
jsonBlob :: T.Text
jsonBlob = "{\"foo\": \"hello\", \"bar\": [\"world\"]}"

main :: IO ()
main = do
    print $ jsonBlob ^. key "foo" . _String
    print $ jsonBlob ^. key "bar" . nth 0 . _String
-}

{-
instance (A.Ix i, Show i, ToJSON e) => ToJSON (A.Array i e) where
    toJSON = Object . H.fromList . Prelude.map ((T.pack . show) *** toJSON) . A.assocs
instance (A.Ix i, Read i, FromJSON e) => FromJSON (A.Array i e) where
    fromJSON (Object m) = H
-}
instance (A.Ix a, ToJSON a, ToJSON b) => ToJSON (A.Array a b) where toJSON arr = toJSON (A.bounds arr, A.assocs arr)
instance (A.Ix a, FromJSON a, FromJSON b) => FromJSON (A.Array a b) where parseJSON = fmap (uncurry A.array) . parseJSON

deriveAllJSONs [''RPSMove, ''RPSState]
deriveAllJSONs [''TronDirection, ''TronMove, ''TronState]
deriveAllJSONs [''TronCoord, ''TronGrid, ''TronPlayer]
