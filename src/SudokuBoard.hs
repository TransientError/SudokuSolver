module SudokuBoard where

  import qualified ListUtils as LU
  import qualified SudokuBoard.Internal as I
  import qualified Data.Maybe as M

  type Rows = I.Rows
  type Position = I.Position

  data Board = Board {rows :: Rows, columns :: I.Columns, boxes :: I.Boxes} deriving (Show, Eq)

  initializeBoardFromRows :: Rows -> Board
  initializeBoardFromRows rows =
     Board { rows = rows
           , columns = I.makeColumnsFromRows rows
           , boxes = I.makeBoxesFromRows rows
           }

  boardLookup :: Board -> Position -> Maybe Integer
  boardLookup Board {columns=columns} (x, y) = columns !! xInt !! yInt
    where xInt = fromIntegral x
          yInt = fromIntegral y

  insert :: Board -> Position -> Integer -> Board
  insert board@Board {rows=rows, columns=columns, boxes=boxes} position@(x, y) number =
    case boardLookup board position of
      Just _ -> error "already filled"
      Nothing -> Board { rows = I.insertRow rows position number
                       , columns = I.insertCol columns position number
                       , boxes = I.insertBox boxes position number
                       }

  validateBoard :: Board -> Bool
  validateBoard Board {rows=rows, columns=columns, boxes=boxes} =
    I.validate rows && I.validate columns && I.validate boxes

  isFull :: Board -> Bool
  isFull Board {rows = rows} = all (all M.isJust) rows

  findNextEmpty :: Board -> Position
  findNextEmpty board = if isFull board then error "board is full" else findNextEmpty' board (0, 0)
    where findNextEmpty' board position =
           case (position, boardLookup board position) of
                (_, Nothing) -> position
                ((8, j), Just _) -> findNextEmpty' board (0, j + 1)
                ((i, j), Just _) -> findNextEmpty' board (i + 1, j)
