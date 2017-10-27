module TestSudokuBoard where

  import Test.HUnit
  import SudokuBoard

  sampleRows :: Rows
  sampleRows = [ [Nothing, Just 8, Nothing, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing]
               , [Just 6, Just 5, Just 9, Just 2, Nothing, Nothing, Just 7, Just 3, Just 1]
               , [Nothing, Nothing, Nothing, Just 1, Nothing, Just 9, Just 4, Just 5, Nothing]
               , [Nothing, Nothing, Just 6, Just 7, Nothing, Nothing, Nothing, Nothing, Nothing]
               , [Just 3, Nothing, Just 2, Nothing, Just 1, Nothing, Just 8, Nothing, Just 5]
               , [Nothing, Nothing, Nothing, Nothing, Nothing, Just 4, Just 6, Nothing, Nothing]
               , [Nothing, Just 2, Just 4, Just 6, Nothing, Just 7, Nothing, Nothing, Nothing]
               , [Just 7, Just 9, Just 3, Nothing, Nothing, Just 2, Just 1, Just 8, Just 6]
               , [Nothing, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 7, Nothing]
               ]

  sampleCols :: Columns
  sampleCols = [ [Nothing, Just 6, Nothing, Nothing, Just 3, Nothing, Nothing, Just 7, Nothing]
               , [Just 8, Just 5, Nothing, Nothing, Nothing, Nothing, Just 2, Just 9, Nothing]
               , [Nothing, Just 9, Nothing, Just 6, Just 2, Nothing, Just 4, Just 3, Nothing]
               , [Nothing, Just 2, Just 1, Just 7, Nothing, Nothing, Just 6, Nothing, Just 3]
               , [Nothing, Nothing, Nothing, Nothing, Just 1, Nothing, Nothing, Nothing, Nothing]
               , [Just 3, Nothing, Just 9, Nothing, Nothing, Just 4, Just 7, Just 2, Nothing]
               , [Nothing, Just 7, Just 4, Nothing, Just 8, Just 6, Nothing, Just 1, Nothing]
               , [Nothing, Just 3, Just 5, Nothing, Nothing, Nothing, Nothing, Just 8, Just 7]
               , [Nothing, Just 1, Nothing, Nothing, Just 5, Nothing, Nothing, Just 6, Nothing]
               ]

  sampleBoxes :: Boxes
  sampleBoxes = [ [Nothing, Just 8, Nothing, Just 6, Just 5, Just 9, Nothing, Nothing, Nothing]
                , [Nothing, Nothing, Just 3, Just 2, Nothing, Nothing, Just 1, Nothing, Just 9]
                , [Nothing, Nothing, Nothing, Just 7, Just 3, Just 1, Just 4, Just 5, Nothing]
                , [Nothing, Nothing, Just 6, Just 3, Nothing, Just 2, Nothing, Nothing, Nothing]
                , [Just 7, Nothing, Nothing, Nothing, Just 1, Nothing, Nothing, Nothing, Just 4]
                , [Nothing, Nothing, Nothing, Just 8, Nothing, Just 5, Just 6, Nothing, Nothing]
                , [Nothing, Just 2, Just 4, Just 7, Just 9, Just 3, Nothing, Nothing, Nothing]
                , [Just 6, Nothing, Just 7, Nothing, Nothing, Just 2, Just 3, Nothing, Nothing]
                , [Nothing, Nothing, Nothing, Just 1, Just 8, Just 6, Nothing, Just 7, Nothing]
                ]

  sampleBoard :: Board
  sampleBoard = Board {rows = sampleRows, columns = sampleCols, boxes = sampleBoxes}

  sampleInsertedRows :: Rows
  sampleInsertedRows = [ [Just 1, Just 8, Nothing, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing]
                       , [Just 6, Just 5, Just 9, Just 2, Nothing, Nothing, Just 7, Just 3, Just 1]
                       , [Nothing, Nothing, Nothing, Just 1, Nothing, Just 9, Just 4, Just 5, Nothing]
                       , [Nothing, Nothing, Just 6, Just 7, Nothing, Nothing, Nothing, Nothing, Nothing]
                       , [Just 3, Nothing, Just 2, Nothing, Just 1, Nothing, Just 8, Nothing, Just 5]
                       , [Nothing, Nothing, Nothing, Nothing, Nothing, Just 4, Just 6, Nothing, Nothing]
                       , [Nothing, Just 2, Just 4, Just 6, Nothing, Just 7, Nothing, Nothing, Nothing]
                       , [Just 7, Just 9, Just 3, Nothing, Nothing, Just 2, Just 1, Just 8, Just 6]
                       , [Nothing, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 7, Nothing]
                       ]
  sampleInsertedCols :: Columns
  sampleInsertedCols = [ [Just 1, Just 6, Nothing, Nothing, Just 3, Nothing, Nothing, Just 7, Nothing]
                       , [Just 8, Just 5, Nothing, Nothing, Nothing, Nothing, Just 2, Just 9, Nothing]
                       , [Nothing, Just 9, Nothing, Just 6, Just 2, Nothing, Just 4, Just 3, Nothing]
                       , [Nothing, Just 2, Just 1, Just 7, Nothing, Nothing, Just 6, Nothing, Just 3]
                       , [Nothing, Nothing, Nothing, Nothing, Just 1, Nothing, Nothing, Nothing, Nothing]
                       , [Just 3, Nothing, Just 9, Nothing, Nothing, Just 4, Just 7, Just 2, Nothing]
                       , [Nothing, Just 7, Just 4, Nothing, Just 8, Just 6, Nothing, Just 1, Nothing]
                       , [Nothing, Just 3, Just 5, Nothing, Nothing, Nothing, Nothing, Just 8, Just 7]
                       , [Nothing, Just 1, Nothing, Nothing, Just 5, Nothing, Nothing, Just 6, Nothing]
                       ]

  sampleInsertedBoxes :: Boxes
  sampleInsertedBoxes = [ [Just 1, Just 8, Nothing, Just 6, Just 5, Just 9, Nothing, Nothing, Nothing]
                        , [Nothing, Nothing, Just 3, Just 2, Nothing, Nothing, Just 1, Nothing, Just 9]
                        , [Nothing, Nothing, Nothing, Just 7, Just 3, Just 1, Just 4, Just 5, Nothing]
                        , [Nothing, Nothing, Just 6, Just 3, Nothing, Just 2, Nothing, Nothing, Nothing]
                        , [Just 7, Nothing, Nothing, Nothing, Just 1, Nothing, Nothing, Nothing, Just 4]
                        , [Nothing, Nothing, Nothing, Just 8, Nothing, Just 5, Just 6, Nothing, Nothing]
                        , [Nothing, Just 2, Just 4, Just 7, Just 9, Just 3, Nothing, Nothing, Nothing]
                        , [Just 6, Nothing, Just 7, Nothing, Nothing, Just 2, Just 3, Nothing, Nothing]
                        , [Nothing, Nothing, Nothing, Just 1, Just 8, Just 6, Nothing, Just 7, Nothing]
                        ]

  sampleInsertedBoard :: Board
  sampleInsertedBoard = Board {rows = sampleInsertedRows, columns = sampleInsertedCols, boxes = sampleInsertedBoxes}

  sudokuBoardTests =
    TestList [ "testMakeColumnsFromRows" ~: sampleCols ~=? makeColumnsFromRows sampleRows
             , "testMakeBoxesFromRows" ~: sampleBoxes ~=? makeBoxesFromRows sampleRows
             , "testInitializeBoardFromRows" ~: sampleBoard ~=? initializeBoardFromRows sampleRows
             , "testBoardLookup" ~: Just 8 ~=? boardLookup sampleBoard (1, 0)
             , "testInsert" ~: sampleInsertedBoard ~=? insert sampleBoard (0, 0) 1
             ]
