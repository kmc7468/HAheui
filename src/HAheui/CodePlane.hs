module HAheui.CodePlane(
    CodePlane,
    getHeight, HAheui.CodePlane.getLine, getCodeFromLine, getCode,
    Point, x, y,
    Cursor, location, dx, dy,
    moveCursor
) where

data CodePlane = CodePlane {
    rawData :: [String]
}

getHeightInternal :: CodePlane -> Int -> Int -> Int
getHeightInternal code x (-1) = 0
getHeightInternal code x y =
    if length (HAheui.CodePlane.getLine code y) > x
        then y + 1
        else getHeightInternal code x (y - 1)
getHeight :: CodePlane -> Int -> Int
getHeight code x = getHeightInternal code x (length $ rawData code)

getLine :: CodePlane -> Int -> String
getLine (CodePlane code) 0 = head code
getLine (CodePlane (_:other)) line = HAheui.CodePlane.getLine (CodePlane other) (line - 1)

getCodeFromLine :: String -> Int -> Char
getCodeFromLine line 0 = head line
getCodeFromLine (_:other) loc = getCodeFromLine other (loc - 1)

getCode :: CodePlane -> Cursor -> Char
getCode code (Cursor location _ _) = getCodeFromLine (HAheui.CodePlane.getLine code (y location)) (x location)

data Point = Point {
    x, y :: Int
} deriving (Eq)

data Cursor = Cursor {
    location :: Point,
    dx, dy :: Int
} deriving (Eq)

moveCursorInternal :: Cursor -> Cursor
moveCursorInternal cursor = Cursor (Point ((x $ location cursor) + (dx cursor)) ((y $ location cursor) + (dy cursor))) (dx cursor) (dy cursor)

checkCursorLine :: CodePlane -> Cursor -> Cursor
checkCursorLine code cursor =
    if movedCursorX < 0
        then Cursor (Point (lineLength - 1) movedCursorY) (dx movedCursor) (dy movedCursor)
        else if movedCursorX >= lineLength
            then Cursor (Point 0 movedCursorY) (dx movedCursor) (dy movedCursor)
            else movedCursor

    where
        movedCursor = moveCursorInternal cursor
        movedCursorX = x $ location movedCursor
        movedCursorY = y $ location movedCursor
        lineLength = length $ HAheui.CodePlane.getLine code movedCursorY

checkCursorHeight :: CodePlane -> Cursor -> Cursor
checkCursorHeight code cursor =
    if movedCursorY < 0
        then Cursor (Point movedCursorX (height - 1)) (dx movedCursor) (dy movedCursor)
        else if movedCursorY >= height
            then Cursor (Point movedCursorX 0) (dx movedCursor) (dy movedCursor)
            else movedCursor

    where
        movedCursor = moveCursorInternal cursor
        movedCursorX = x $ location movedCursor
        movedCursorY = y $ location movedCursor
        height = getHeight code movedCursorX

moveCursor :: CodePlane -> Cursor -> Cursor
moveCursor code cursor = checkCursorHeight code (checkCursorLine code (moveCursorInternal cursor))