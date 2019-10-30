import Debug.Trace
import System.Random

type Board = [[Char]]

type Point = (Int, Int)

fillChar = '█'

emptyChar = ' '

generateEmptyBoard :: Int -> Int -> [[Char]]
generateEmptyBoard w h = take w $ repeat $ take h $ repeat emptyChar

board :: Board
board = insertAtStart (generateEmptyBoard 15 15) fillChar

insertAtStart :: [[Char]] -> Char -> [[Char]]
insertAtStart (b:bs) val = (replaceAt 0 fillChar b : bs)

startPoint :: Point
startPoint = (0, 0)

getPoint :: Board -> Point -> Char
getPoint b (row, col)
  | isInsideBounds b (row, col) == False = fillChar
  | otherwise = b !! col !! row

isAvailable :: Board -> Point -> Bool
isAvailable b p = (== ' ') $ getPoint b p

getAvailableTiles :: Board -> Point -> [Point]
getAvailableTiles b (row, col) =
  filter (isAvailable b) $
  filter (isInsideBounds b) $
  [(row - 2, col), (row + 2, col), (row, col - 2), (row, col + 2)]

getPath :: Point -> Point -> [Point]
getPath (x1, y1) (x2, y2)
  | x1 == x2 && y1 < y2 = [(x2, y2 - 1)] ++ [(x2, y2)]
  | x1 == x2 && y1 > y2 = [(x2, y2 + 1)] ++ [(x2, y2)]
  | y1 == y2 && x1 < x2 = [(x2 - 1, y2)] ++ [(x2, y2)]
  | y1 == y2 && x1 > x2 = [(x2 + 1, y2)] ++ [(x2, y2)]

replaceAt :: Int -> a -> [a] -> [a]
replaceAt z y xs = as ++ (y : tail bs)
  where
    (as, bs) = splitAt z xs

insertPoints :: Board -> [Point] -> Board
insertPoints board [] = board
insertPoints board ((x, y):ps) = insertPoints newBoard ps
  where
    newRow = replaceAt x fillChar $ board !! y
    newBoard = replaceAt y newRow board

pickPoint :: RandomGen g => g -> [Point] -> (Point, g)
pickPoint g ps = (ps !! x, newG)
  where
    (x, newG) = randomR (min, max) g
    min = 0
    max = (length ps) - 1

isInsideBounds :: Board -> Point -> Bool
isInsideBounds board (x, y)
  | x < 0 = False
  | x > (length (head board) - 1) = False
  | y < 0 = False
  | y > ((length board) - 1) = False
  | otherwise = True

buildMaze board [] _ = board
buildMaze board (cP:restPoints) gen
  | length (getAvailableTiles board cP) == 0 = buildMaze board restPoints gen
  | otherwise =
    let availTiles = getAvailableTiles board cP
        (nextPoint, newGen) = pickPoint gen $ availTiles
        newBoard = insertPoints board $ getPath cP nextPoint
     in buildMaze newBoard (nextPoint : cP : restPoints) newGen

printMaze :: Board -> IO ()
printMaze = putStr . unlines

main :: IO ()
main = do
  seed <- getStdGen
  printMaze $ buildMaze (generateEmptyBoard 16 16) [startPoint] seed
