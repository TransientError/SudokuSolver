module SudokuBoard where

  import qualified ListUtils as LU
  import qualified Data.Set as S
  import qualified Data.Maybe as M
  type Position = (Int, Int)
  type Rows = [[Maybe Integer]]
  type Columns = [[Maybe Integer]]
  type Boxes = [[Maybe Integer]]

  data Board = Board {rows :: Rows, columns :: Columns, boxes :: Boxes} deriving (Show, Eq)

  makeColumnsFromRows :: Rows -> Columns
  makeColumnsFromRows rows = map (\i -> map (!! i) rows) [0..8]

  partitionByThree :: [a] -> [[a]]
  partitionByThree full = [first, second, third]
    where (first, end) = splitAt 3 full
          (second, third) = splitAt 3 end

  makeBox :: [[a]] -> [[a]]
  makeBox partition =
    [ (partition !! 0) ++ (partition !! 3) ++ (partition !! 6)
    , (partition !! 1) ++ (partition !! 4) ++ (partition !! 7)
    , (partition !! 2) ++ (partition !! 5) ++ (partition !! 8)
    ]

  makeBoxesFromRows :: Rows -> Boxes
  makeBoxesFromRows rows =
    let rowsOfBoxes = partitionByThree rows
    in concatMap (makeBox . concatMap partitionByThree) rowsOfBoxes

  initializeBoardFromRows :: Rows -> Board
  initializeBoardFromRows rows =
     Board { rows = rows
           , columns = makeColumnsFromRows rows
           , boxes = makeBoxesFromRows rows
           }

  boardLookup :: Board -> Position -> Maybe Integer
  boardLookup Board {columns=columns} (x, y) = columns !! xInt !! yInt
    where xInt = fromIntegral x
          yInt = fromIntegral y

  findBox :: Position -> Int
  findBox (x, y)
    | x < 3 && y < 3 = 0 | x < 6 && y < 3 = 1 | y < 3 = 2
    | x < 3 && y < 6 = 3 | x < 6 && y < 6 = 4 | y < 6 = 5
    | x < 3 = 6          | x < 6 = 7          | otherwise = 8

  findBoxPosition :: Position -> Int
  findBoxPosition (x, y) = fromIntegral(3 * xPos + yPos)
    where xPos = x `mod` 3
          yPos = y `mod` 3

  insertRow :: Rows -> Position -> Integer -> Rows
  insertRow rows (x, y) number =
    let (aboveR, row:belowR) = splitAt (fromIntegral y) rows
    in aboveR ++ LU.change row x (Just number):belowR

  insertCol :: Columns -> Position -> Integer -> Columns
  insertCol columns (x, y) number =
    let (leftC, col:rightC) = splitAt (fromIntegral x) columns
    in leftC ++ LU.change col y (Just number):rightC

  insertBox :: Boxes -> Position -> Integer -> Boxes
  insertBox boxes position number =
    let (beforeBoxes, box:afterBoxes) = splitAt (findBox position) boxes
        (beforeNums, _:afterNums) = splitAt (findBoxPosition position) box
    in beforeBoxes ++ (beforeNums ++ Just number:afterNums):afterBoxes

  insert :: Board -> Position -> Integer -> Board
  insert board@Board {rows=rows, columns=columns, boxes=boxes} position@(x, y) number =
    case boardLookup board position of
      Just _ -> error "already filled"
      Nothing -> Board { rows = insertRow rows position number
                       , columns = insertCol columns position number
                       , boxes = insertBox boxes position number
                       }

  hasNoDuplicates :: Eq a => Ord a => [Maybe a] -> Bool
  hasNoDuplicates xs = length xs' == (length . S.toList . S.fromList) xs'
    where xs' = filter M.isJust xs

  validate :: Eq a => Ord a => [[Maybe a]] -> Bool
  validate = all hasNoDuplicates

  validateBoard :: Board -> Bool
  validateBoard Board {rows=rows, columns=columns, boxes=boxes} =
    validate rows && validate columns && validate boxes

  isFull :: Board -> Bool
  isFull Board {rows = rows} = all (all M.isJust) rows

  findNextEmpty :: Board -> Position
  findNextEmpty board = if isFull board then error "board is full" else findNextEmpty' board (0, 0)
    where findNextEmpty' board position =
           case (position, boardLookup board position) of
                (_, Nothing) -> position
                ((8, j), Just _) -> findNextEmpty' board (0, j + 1)
                ((i, j), Just _) -> findNextEmpty' board (i + 1, j)
