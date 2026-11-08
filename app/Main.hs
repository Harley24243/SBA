{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Data.List (delete, find, (\\), union)
import Data.Set ( Set, empty )
import Graphics.Gloss ( play, loadBMP )
import Graphics.Gloss.Data.Color
import Data.Char (digitToInt)
import Control.Lens (element, makeLenses, view)
import Control.Lens.Operators ((.~))
import Data.Maybe (isNothing, isJust)
import Control.Lens.Combinators (set, over)
import Graphics.Gloss.Interface.IO.Interact
import System.Random (StdGen, getStdGen, Random (randomR))

type Cell = Maybe Int

data CellMark = CellMark {
    _center :: Set Int,
    _corner :: Set Int
}
$(makeLenses ''CellMark)

type Grid = (Int, Int) -> Cell
type GridMark = (Int, Int) -> CellMark
type Solution = (Int, Int) -> Int
type Selection = [(Int, Int)]
type Stained = [(Int, Int, Int)]

data Input = Full | Corner | Center

data Screen = Start | Grid | Pause | Shop deriving Eq

data Item = Item {
    _id :: String,
    _cost :: Int,
    _desc :: String
} deriving Eq

data Player = Player {
    _index :: Int,
    _name :: String,
    _score :: Int,
    _health :: Int,
    _mark :: GridMark,
    _input :: Input,
    _items :: [Item],
    _itemSelection :: Int
}
$(makeLenses ''Player)

type SDB = [(Grid, Solution)]

data Model = Model {
    _db :: SDB,
    _gen :: StdGen,
    _screen :: Screen,
    _grid :: Grid,
    _selection :: Selection,
    _solution :: Solution,
    _players :: [Player],
    _turn :: Int,
    _timeLeft :: Int,
    _roundTime :: Int,
    _shopItems :: [Item],
    _shopSelection :: Int,
    _finished :: Bool,
    _end :: Bool,
    _winner :: Maybe Player,
    _stained :: Stained
}
$(makeLenses ''Model)

dTextSize :: Float
dTextSize = 100

cellSize :: Float
cellSize = 60

sTextSize :: Float
sTextSize = cellSize * 0.8;

roundTime' :: Int
roundTime' = 30

drawEmptyGrid :: Float -> Bool -> Picture
drawEmptyGrid n detailed = Pictures $ [1 .. 10] >>= \i ->
    if i `elem` [1, 4, 7, 10]
    then
        [ Translate (n * 5.5) (n * i) $ rectangleSolid (n * 9) 5
        , Translate (n * i) (n * 5.5) $ rectangleSolid 5 (n * 9)]
    else if detailed then [Line [(n, n*i), (n*10, n*i)], Line [(n*i, n), (n*i, n*10)]] else []

outerIndexTranslate :: Float -> Int -> Picture -> Picture
outerIndexTranslate s i
    | i == 0 = Translate (- (s * 0.3)) (- (s * 0.3))
    | i == 1 = Translate (s*0.3) (- (s * 0.3))
    | i == 2 = Translate (- (s * 0.3)) (s*0.3)
    | i == 3 = Translate (s*0.3) (s*0.3)
    | i == 4 = Translate 0 (- (s * 0.3))
    | i == 5 = Translate 0 (s*0.3)
    | i == 6 = Translate (- (s * 0.3)) 0
    | i == 7 = Translate (s*0.3) 0
    | otherwise = id

drawText :: String -> Picture
drawText str = Scale s s $ Text str
    where
        s = (cellSize / dTextSize) * 0.8

drawCell :: Cell -> CellMark -> Picture
drawCell (Just n) _ = Translate s s $ drawText (show n) where s = (cellSize - sTextSize) / 2
drawCell Nothing _ = Blank
-- drawCell Nothing m = Translate (cellSize * 0.4) (cellSize * 0.4) $ Pictures
--     $ Scale 0.25 0.25 (drawText (concatMap show (toAscList (_center m)))) : ts
--         where
--             (_, ts) =
--                 foldl'
--                     (\(i, ps) n -> (i+1, outerIndexTranslate cellSize i (Scale 0.5 0.5 (drawText (show n))) : ps))
--                     (0, [])
--                     (toAscList (_corner m))

drawGrid :: Float -> Grid -> GridMark -> Stained -> Picture
drawGrid s g m st = Pictures $ drawEmptyGrid s True : do
    r <- [0 .. 8]
    c <- [0 .. 8]
    return
        $ Translate ((fromIntegral c + 1) * s) ((9 - fromIntegral r) * s)
        $ Pictures
            [ drawCell (g (r, c)) (m (r, c))
            , if any (\(r', c', _) -> (r, c) == (r', c')) st
                then Translate (cellSize / 2) (cellSize /2) $ circleSolid (cellSize / 2)
                else Blank]

drawSelection :: Float -> Selection -> Picture
drawSelection s sel = Color aquamarine $ Pictures $ map f sel
    where
        f (r, c) = Translate ((fromIntegral c + 1.5) * s) ((9.5 - fromIntegral r) * s) $ rectangleSolid s s

drawIcon :: Player -> Picture
drawIcon player = Pictures [ Translate (l / 2) (sTextSize / 2) $ Color yellow $ rectangleSolid l (sTextSize * 1.5), drawText $ _name player ]
    where l = sTextSize * fromIntegral (length (_name player))

drawTimer :: Int -> Picture
drawTimer time = drawText $ concat [show (time `div` 60), " : ", show (time `mod` 60)]

drawHealth :: Player -> Picture
drawHealth player = drawText $ "Health : " ++ show (_health player)

drawScore :: Player -> Picture
drawScore player = drawText $ "Score : " ++ show (_score player)

drawItem :: Item -> Bool -> Picture
drawItem (Item n c _) showCost = Pictures
    [ Translate (cellSize * 0.1) cellSize $ Scale s s $ Text n
    , if showCost then Translate (cellSize * 0.1) (cellSize * 0.1) $ drawText $ show c else Blank]
    where
        s = cellSize * 3 / (dTextSize * fromIntegral (length n))

drawPlayerItemGrid :: Picture
drawPlayerItemGrid = Pictures
    $ Line [(0, cellSize * 6), (0, - (cellSize * 6))]
    : Line [(cellSize * 4, cellSize * 6), (cellSize * 4, - (cellSize * 6))]
    : map (\i -> Line [(0, cellSize * (6 - i * 2)), (cellSize * 4, cellSize * (6 - i * 2))]) [0..6]

drawPlayerItem :: Player -> Picture
drawPlayerItem player = Pictures
    $ drawPlayerItemGrid
    : (if _itemSelection player >= 0 then Translate (cellSize * 2) (cellSize * (5 - 2 * fromIntegral (_itemSelection player)))
        $ Color aquamarine $ rectangleSolid (cellSize * 4) (cellSize * 2) else Blank)
    : fst (foldr (\it (ps, i) ->
        (Translate 0 (cellSize * (3.5 - 2 * i)) (drawItem it False) : ps, i + 1)) ([], 0) (_items player))

drawItemDesc :: Item -> Picture
drawItemDesc item = let d = _desc item in Pictures [
    Translate (- (sTextSize * fromIntegral (length d) / 3)) (- (cellSize * 5)) (drawText d),
    Translate (- (sTextSize * 4)) (- (cellSize * 6.5)) $ drawText "[B]uy / [U]se?"]

drawPlayer :: Player -> Int -> Picture
drawPlayer player time = Pictures
    [ Translate 0 (sTextSize * 6) $ drawIcon player
    , Translate 0 (sTextSize * 4) $ drawTimer time
    , Translate 0 (sTextSize * 2) $ drawHealth player
    , drawScore player
    , Translate (- (cellSize * 5)) 100 $ drawPlayerItem player
    , let n = _itemSelection player in if n >= 0 && n < length (_items player) then Translate 400 100 $ drawItemDesc (_items player !! n) else Blank]

drawEORText :: Bool -> Maybe Player -> Picture
drawEORText e mp = Pictures
    [ maybe (Translate (- (sTextSize * 5)) 0 $ Color red $ drawText $ if e then "No one Wins" else "Grid Finished!")
        (\player -> Translate (- (sTextSize * 3)) 0 $ Color orange $ drawText $ _name player ++ " Wins!") mp
    , Translate 0 (- sTextSize) $ Color orange $ rectangleSolid (sTextSize * 4.5) (sTextSize * 1.2)
    , Translate (- (sTextSize * 2)) (- (sTextSize * 1.5)) $ drawText $ if e then "Restart" else "Next" ]

drawStartScreen :: [Picture]
drawStartScreen =
    [ Translate (-600) 300 $ Scale 0.8 0.8 $ Text "Sudoku Battle Advanced"
    , Translate (-400) 100 $ drawText "- [P]lay Game -"
    , Translate (-400) (-100) $ drawText "- [S]ettings -"]

drawPauseScreen :: Player -> [Picture]
drawPauseScreen player =
    [ Translate (-500) 300 $ Text $ "Next Fill : " ++ _name player
    , Translate (-300) 0 $ drawText "- [R]eady -"]

drawShopScreen :: [Item] -> Int -> [Picture]
drawShopScreen is n =
    Translate 0 (- (cellSize * 3.5)) (drawEmptyGrid cellSize False)
        : Translate (-600) 350 (drawText "SHOP, [C]ontinue?")
        : (if n >= 0 then Translate (cellSize * (3 * fromIntegral (mod n 3) + 2.5)) (cellSize * (5 - 3 * fromIntegral (div n 3))) (Color aquamarine (rectangleSolid (cellSize * 3) (cellSize * 3))) else Blank)
        : (if n >= 0 && n < length is then drawItemDesc (is !! n) else Blank)
        : fst (foldl (\(ps, i) it ->
            (Translate (cellSize * (3 * fromIntegral (mod i 3) + 1)) (cellSize * (3.5 - 3 * fromIntegral (div i 3))) (drawItem it True) : ps, i + 1)) ([], 0 :: Int) is)


render :: Picture -> Model -> Picture
render bg model = Pictures $ bg : case _screen model of
    Start -> drawStartScreen
    Grid ->
        [ Translate 0 (-300) $ drawSelection cellSize $ _selection model
        , Translate 0 (-300) $ drawGrid cellSize (_grid model) (_mark player) (_stained model)
        , Translate (-400) (-100) $ drawPlayer player (_timeLeft model)
        , if _end model || _finished model then Translate 0 (-300) $ drawEORText (_end model) (_winner model) else Blank]
        where
            player = _players model !! _turn model
    Pause -> drawPauseScreen $ _players model !! _turn model
    Shop -> Translate (-400) (-100) (drawPlayer player 0) : drawShopScreen (_shopItems model) (_shopSelection model)
        where
            player = _players model !! _turn model

getBox :: (Int, Int) -> [(Int, Int)]
getBox (r, c) = [(r', c') | r' <- expand br, c' <- expand bc]
    where
        br = r `div` 3
        bc = c `div` 3
        expand :: Int -> [Int]
        expand n = [n*3 .. n*3+2]

isFinished :: Grid -> Solution -> Bool
isFinished g s = isNothing $ find (\c -> g c /= Just (s c)) [(r, c) | r <- [0..8], c <- [0..8]]

findNextPlayer :: [Player] -> Int -> Maybe Player
findNextPlayer ps t = find (\p' -> _health p' > 0) (sps ++ fps)
    where
        (fps, sps) = splitAt (t+1) ps

atRestartArea :: (Float, Float) -> Bool
atRestartArea (x, y) = x >= - (sTextSize * 4.5 / 2) && x <= sTextSize * 4.5 / 2 && y >= - (300 + sTextSize * 1.6) && y <= - (300 + sTextSize * 0.4)

itemsAreaIndex :: (Float, Float) -> Int
itemsAreaIndex (x, y) =
    if x >= - (400 + cellSize * 5) && x <= - (400 + cellSize)
    then floor (3 - y / (cellSize * 2)) else -1

handleEvent :: Event -> Model -> Model
handleEvent event model = case event of
    EventKey (MouseButton LeftButton) Up _ coor@(x, y) ->
        case _screen model of
            Grid ->
                let
                    c :: Int
                    c = floor (x / cellSize) - 1
                    r :: Int
                    r = 9 - floor ((y + 300) / cellSize)
                    sel = _selection model
                    atR = atRestartArea coor
                    ii = itemsAreaIndex coor
                in
                if c >= 0 && c < 9 && r >= 0 && r < 9
                then set selection (if (r, c) `elem` sel then Data.List.delete (r, c) sel else (r, c) : sel) model
                else if _end model && atR then reset model
                else if _finished model && atR then continue model
                else if ii >= 0 && ii < 6 then
                    over players (\ps -> element (view turn model) .~ over itemSelection (\i ->
                        if i == ii then -1 else ii) (ps !! view turn model) $ ps) model
                else model
            Shop ->
                let
                    c :: Int
                    c = floor $ ((x / cellSize) - 1) / 3
                    r :: Int
                    r = floor $ (6.5 - (y / cellSize)) / 3
                in
                if c >= 0 && c < 3 && r >= 0 && r < 3
                then over shopSelection (\s -> if s == r * 3 + c then -1 else r * 3 + c) model
                else model
            _ -> model
    -- EventKey (Char ch) Up (Modifiers Down _ _) _ ->
    --     if ch `elem` ['1' .. '9']
    --     then
    --         let
    --             t = _turn model
    --             sel = _selection model
    --             n = digitToInt ch
    --         in
    --         over players (\ps ->
    --             element t .~ over mark (\m c ->
    --                 if c `elem` sel
    --                 then over corner (\cs ->
    --                     if member n cs
    --                     then Data.Set.delete n cs
    --                     else insert n cs) (m c)
    --                 else m c) (ps !! t) $ ps) model
    --     else model
    -- EventKey (Char ch) Up (Modifiers _ Down _) _ ->
    --     if ch `elem` ['1' .. '9']
    --     then
    --         let
    --             t = _turn model
    --             sel = _selection model
    --             n = digitToInt ch
    --         in
    --         over players (\ps ->
    --             element t .~ over mark (\m c ->
    --                 if c `elem` sel
    --                 then over center (\cs ->
    --                     if member n cs
    --                     then Data.Set.delete n cs
    --                     else insert n cs) (m c)
    --                 else m c) (ps !! t) $ ps) model
    --     else model
    EventKey (Char 'p') Up _ _ ->
        if _screen model == Start
        then set timeLeft (_roundTime model) $ set screen Grid model
        else model
    EventKey (Char 'r') Up _ _ ->
        if _screen model == Pause
        then
            over stained (filter (\(_, _, t) -> t >= 0) . map (\(r, c, t) -> (r, c, t-1)))
            $ set timeLeft (_roundTime model)
            $ set screen Grid model
        else model
    EventKey (Char 'b') Up _ _ ->
        if _screen model == Shop && _shopSelection model >= 0
        then
            let
                item = _shopItems model !! _shopSelection model
                sc = _score (_players model !! _turn model)
            in
            if sc >= _cost item
            then
                (\m -> over players (\ps ->
                    let p = ps !! view turn m in
                    element (view turn m) .~
                        over items (item :) (over score (\s -> s - _cost item) p) $ ps) m)
                $ set shopSelection (-1)
                $ over shopItems (delete item) model
            else model
        else model
    EventKey (Char 'c') Up _ _ ->
        if _screen model == Shop
        then
            (\m -> if _screen m == Grid then set timeLeft (_roundTime m) m else m)
            $ (\m -> set screen (if view turn m == 0 then Grid else Shop) m)
            $ set shopSelection (-1)
            $ (\m -> set shopItems (allItems \\ _items (_players m !! _turn m)) m)
            $ over turn (\t' -> maybe t' _index (findNextPlayer (_players model) t')) model
        else model
    EventKey (Char 'u') Up _ _ ->
        let
            t = _turn model
            ps = _players model
            p = ps !! t
            its = _items p
            is = _itemSelection p
        in
        if _screen model == Grid && is >= 0 && is < length its
        then
            let it = its !! is in
            over players (\ps' -> element t .~ over items (filter (/= it)) (ps' !! t) $ ps') $
            case _id it of
                "Snack" -> over players (element t .~ over health (+1) p) model
                "Coffee" -> over timeLeft (*2) model
                "Ink Bottle" -> over stained (++ map (\(r, c) -> (r, c, 2)) (_selection model)) model
                "Pencil" -> model
                "Tea" -> model
                "Second Pen" -> model
                _ -> model
        else model
    EventKey (Char ch) Up _ _ ->
        let
            g = _grid model
            sel = _selection model
            st = _stained model
        in
        case sel of
            [coor@(r, c)] ->
                if not (_end model) && ch `elem` ['1' .. '9'] && isNothing (g coor) && all (\(r', c', _) -> coor /= (r', c')) st
                then
                    let
                        n = digitToInt ch
                        correct = _solution model coor == n
                        t = _turn model
                        emptyCnt :: [(Int, Int)] -> Int
                        emptyCnt = length . filter isNothing . map g
                        mult = min (emptyCnt [(r, c') | c' <- [0..8]])
                            (min (emptyCnt [(r', c) | r' <- [0..8]]) (emptyCnt $ getBox coor))
                    in
                    (\m -> if not (view end m) && not (view finished m) then set screen Pause m else m)
                        $ (\m -> set end (isJust (view winner m) || view health (view players m !! view turn m) <= 0) m)
                        $ (\m -> set winner (if view turn m == t && view finished m then Just (view players m !! t) else Nothing) m)
                        $ (\m -> set finished (isFinished (view grid m) (view solution m)) m)
                        $ (\m -> set turn (maybe (view turn m) _index (findNextPlayer (view players m) (view turn m))) m)
                        $ set selection []
                        $ (\m -> over players (\ps ->
                            let p = ps !! view turn m in
                            element (view turn m) .~ over health (\h -> if correct then h else h-1)
                                (over score (if correct then (+ _timeLeft m * mult) else id) p) $ ps) m)
                        $ set grid (if correct then \coor' -> if coor' == coor then Just n else g coor' else g) model
                else model
            _ -> model
    _ -> model


step :: Float -> Model -> Model
step _ model =
    (\m -> if not (view end m) && view timeLeft m <= 0 then set screen Pause m else m)
        $ (\m -> over end (\e -> e || view health (view players m !! view turn m) <= 0) m)
        $ (\m -> if view timeLeft m <= 0 then over turn (\t -> maybe t _index (findNextPlayer (view players m) t)) m else m)
        $ (\m -> if view timeLeft m <= 0 then set selection [] m else m)
        $ (\m -> if view timeLeft m <= 0 then over players (element (view turn m) .~ over health (\h -> if h > 0 then h - 1 else h) (view players m !! view turn m)) m else m)
        $ (\m -> if view screen m /= Grid || view finished m || view end m then m else over timeLeft (\tl -> if tl > 0 then tl - 1 else tl) m) model

parseCell :: Char -> Cell
parseCell '0' = Nothing
parseCell c = Just $ digitToInt c

parseGrid :: String -> Grid
parseGrid s (r, c) = parseCell $ s !! (r * 9 + c)

parseSolution :: String -> Solution
parseSolution s (r, c) = digitToInt $ s !! (r * 9 + c)


emptyGridMark :: GridMark
emptyGridMark _ = CellMark empty empty

allItems :: [Item]
allItems =
    [ Item "Snack" 200 "+1 health"
    , Item "Coffee" 200 "*2 remaining time"
    , Item "Ink Bottle" 300 "cover some cells for next 3 round"
    -- , Item "Pencil" 300 "+1 fill in chance"
    -- , Item "Tea" 500 "no time limit for 2 round"
    -- , Item "Second Pen" 500 "can fill in 2 digit in a round" 
    ]

initModel :: SDB -> StdGen -> Model
initModel sdb g = Model {
    _db = sdb,
    _gen = g',
    _screen = Start,
    _grid = gr,
    _selection = [],
    _solution = s,
    _players =
        [ Player 0 "P1" 0  5 emptyGridMark Full [] (-1)
        , Player 1 "P2" 0  5 emptyGridMark Full [] (-1)],
    _turn = 0,
    _timeLeft = roundTime',
    _roundTime = roundTime',
    _shopItems = allItems,
    _shopSelection = -1,
    _finished = False,
    _end = False,
    _winner = Nothing,
    _stained = []
} where
    (r, g') = randomR (0, length sdb - 1) g
    (gr, s) = sdb !! r

reset :: Model -> Model
reset model = genNext model {
    _screen = Start,
    _selection = [],
    _players =
        [ Player 0 "P1" 0  5 emptyGridMark Full [] (-1)
        , Player 1 "P2" 0  5 emptyGridMark Full [] (-1)],
    _turn = 0,
    _timeLeft = roundTime',
    _roundTime = roundTime',
    _shopItems = allItems,
    _shopSelection = -1,
    _finished = False,
    _end = False,
    _winner = Nothing
}

continue :: Model -> Model
continue model = (\m -> set shopItems (allItems \\ _items (_players m !! 0)) m) $ genNext model {
    _screen = Shop,
    _selection = [],
    _players = fst . foldl (\(ps, i) p -> (p {_index = i} : ps, i+1)) ([], 0 :: Int) $ filter (\p -> _health p > 0) (_players model),
    _turn = 0,
    _timeLeft = roundTime',
    _finished = False
}

genNext :: Model -> Model
genNext model = model {
    _grid = gr,
    _solution = s,
    _gen = g
} where
    (r, g) = randomR (0, length (_db model) - 1) (_gen model)
    (gr, s) = _db model !! r

pairAll :: [a] -> [(a, a)]
pairAll [] = []
pairAll [_] = []
pairAll (x:y:xs) = (x, y) : pairAll xs

main :: IO ()
main = do
    raw <- readFile "sudoku.txt"
    background <- loadBMP "background.bmp"
    -- gridPicture <- loadBMP "blank_sudoku.bmp"
    stdg <- getStdGen
    play
        (InWindow "SBA" (1800, 1000) (10, 10))
        black               -- background color
        1                   -- frame per second
        (initModel
            (map (\(sg, ss) -> (parseGrid sg, parseSolution ss)) $ pairAll $ filter (\s -> length s == 81) $ lines raw)
            stdg)
        (render $ Scale 1.2 1.2 background) -- picture to display
        handleEvent
        step