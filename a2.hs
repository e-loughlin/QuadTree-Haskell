-- CPSC 449: Programming Paradigms
-- Professor: Dr. Ben Stephenson
-- University of Calgary
--
-- Assignment 2
-- Evan Loughlin
-- 00503393
-- June 19, 2017

-- Description:
-- In this assignment, a quad tree data structure is used to store data with 
-- a two-dimensional structure. A quad tree has internal nodes that have up to
-- four children. Each leaf node contains data for the region that it 
-- represents. In particular, each of the four nodes represents the colour 
-- of a single square. Leaf nodes represent data, as follows:
--
--          First Node: Top Left (TL)
--          Second Node: Top Right (TR)
--          Third Node: Bottom Left (BL)
--          Fourth Node: Bottom Right (BR)
--                     ___                      ___________________
--                    /   \                    |         |         |
--                 __|     |_                  |  TL     |  TR     |
--                /   \___/  \                 | (R,G,B) | (R,G,B) |
--               /     | |    \                |         |         |
--              /      | |     \               |---------|---------|
--             /       | |      \              |         |         |
--            /        | |       \             |  BL     |  BR     |
--           /        _| |_       \            | (R,G,B) | (R,G,B) |
--          /        |    |        \           |_________|_________|
--      ___/     ___ |    |___      \___
--     /   \    /   \     /   \     /   \
--    |  TL |  |  TR |   |  BL |   |  BR |
--     \___/    \___/     \___/     \___/
--
--
--  Each leaf node also contains three Int values (Red, Green, Blue), which 
-- represents the colour of that region. If the entire region is of a single
-- colour, then that node has no children, otherwise, it is recursively
-- called until each unique pixel is correctly coloured.
--
-- (For example, a pure black square would contain (R,G,B) = (0,0,0), and have
-- no children nodes.
-- 
-- NOTE: Bonus Rectangle image was attempted, although the result is buggy due to a logic error.

import qualified Data.ByteString.Lazy as BS
import Data.Word
import Data.Bits
import Data.Char
import Codec.Compression.Zlib as Z
import Numeric (showHex)

-- ============================================================================
-- PART 1 ---------------------------------------------------------------------
-- ============================================================================

-- Quad Tree Data Type
-- Notation is as follows:
-- (Int, Int, Int) = (Red, Blue, Green)
-- (QuadTree, Quadtree, QuadTree, QuadTree) = (TL, TR, BL, BR)
data QuadTree = Leaf Int Int Int | 
                Node QuadTree QuadTree QuadTree QuadTree | 
                NilT
--
-- Leaf Constructor for QuadTree
--
newLeaf :: (Int, Int, Int) -> QuadTree
newLeaf (r,g,b)
   | r < 0 || g < 0 || b < 0 || r > 255 || g > 255 || b > 255 =
                 error "Invalid type constructor for Leaf. Integer values (Int, Int, Int) must be between 0 and 255."
   | otherwise = (Leaf r g b)

-- QuadTree Image Representation
-- SquareImage : Width, QuadTree
-- RectImage : Width, Height, QuadTree
data Image = SquareImage Int QuadTree | 
             RectImage Int Int QuadTree
--
instance Show Image where
   show (SquareImage w qt) = "Square QuadTree Image (Width = " ++ show w ++ "): " ++ (show qt)
   show (RectImage w h qt) = "Rectangular QuadTree Image (Width = " ++ show w ++ ") " ++ "(Height = " ++ show h ++ "): " ++ (show qt)

instance Show QuadTree where
   show (Leaf r g b) = "Leaf: (" ++ (show r) ++ ", " ++ (show g) ++ ", " ++ (show b) ++ ") "
   show (Node a b c d) = "Node: [ " ++ (show a) ++ (show b) ++ (show c) ++ (show d) ++ "] "
   show NilT = "NilT "


-- ============================================================================
-- PART 2 ---------------------------------------------------------------------
-- ============================================================================

-- createTree 
-- Function which creates a quad tree image from list of lists of 3-tuples.
createTree :: [[(Int, Int, Int)]] -> Image
createTree list 
   | (isSquareList list) = SquareImage (length list) (listToQuadTree list)
   | otherwise = RectImage (length (head list)) (length list) (listToQuadTree list)
--   | otherwise = error "Quad Tree Image can only be created with a square list. (2x2, 4x4, 100x100, etc.)"


-- Function which takes a list representation of an image, and stores it in a
-- quad tree data structure.
listToQuadTree :: [[(Int, Int, Int)]] -> QuadTree
listToQuadTree [[]] = NilT
listToQuadTree [] = NilT
listToQuadTree arr 
   | (samePixels arr) = newLeaf (head (head arr))
   | otherwise =
                 Node (listToQuadTree (quarterArray arr 1))
                      (listToQuadTree (quarterArray arr 2))
                      (listToQuadTree (quarterArray arr 3))
                      (listToQuadTree (quarterArray arr 4))
--
-- isSquareList
-- Function which returns true if the list of the form [[a]] is square. 
-- That is to say, each inner list has the same length as the entire outer length.
isSquareList :: [[a]] -> Bool
isSquareList [] = True
isSquareList list
   | (length list) == (length (head list)) = True
   | otherwise = False

-- allSameInList:
-- Function that scans through a list, and returns
-- true if they're all the same.
--
allSameInList :: Eq a => [a] -> Bool
allSameInList [] = True
allSameInList [x] = True
allSameInList (x:xs) 
   | x == (head xs) = (allSameInList xs)
   | otherwise = False
--
-- samePixels:
-- Function that scans through a list of type [[(Int, Int, Int)]]
-- and returns true if each tuple is the same as each other tuple.
--
samePixels :: [[(Int, Int, Int)]] -> Bool
samePixes [] = True
samePixels [x] = allSameInList x
samePixels (x:xs)
   | allSameInList x && x == (head xs) = samePixels xs
   | otherwise = False

-- quarterArray
-- Function which takes a list of lists (polymorphic), and returns one quarter of it.
-- The Int input should be either 1, 2, 3, or 4, where 1: Top Left, 2: Top Right, 
-- 3: Bottom Left, and 4: Bottom Right.
quarterArray :: [[a]] -> Int -> [[a]]
quarterArray arr quarter 
   | quarter == 1 = firstHalfList (firstHalfArray arr)
   | quarter == 2 = firstHalfList (secondHalfArray arr)
   | quarter == 3 = secondHalfList (firstHalfArray arr)
   | quarter == 4 = secondHalfList (secondHalfArray arr)
   | otherwise = error "Usage: [[a]] -> Int -> [[a]]. Returns quarter of [[a]]. TL (1), TR (2), BL (3), or BR (4) of [[a]]."

-- firstHalfArray
-- Function which takes a list of lists, and halves each list, returning the front half.
firstHalfArray :: [[a]] -> [[a]]
firstHalfArray [] = []
firstHalfArray (x:xs) = (firstHalfList x):(firstHalfArray xs)

-- secondHalfArray
-- Function which takes a list of lists, and halves each list, returning the second half.
secondHalfArray :: [[a]] -> [[a]]
secondHalfArray [] = []
secondHalfArray (x:xs) = (secondHalfList x):(secondHalfArray xs)

-- firstHalfList: 
-- Function which takes a list, and returns the first half of it.
firstHalfList :: [a] -> [a]
firstHalfList [] = []
firstHalfList xs
   | (length xs) `rem` 2 == 0 = take ((length xs) `div` 2) xs
   | otherwise = take ((length xs)`div` 2 + 1) xs

--secondHalfList:
-- Function which takes a list, and returns the second half of it.
secondHalfList :: [a] -> [a]
secondHalfList [] = []
secondHalfList xs
   | (length xs) `rem` 2 == 0 = drop ((length xs) `div` 2) xs
   | otherwise = drop ((length xs) `div` 2 +1) xs   
   


   
   
-- ============================================================================
-- PART 3 ---------------------------------------------------------------------
-- ============================================================================

-- Write 3 functions for rotating an image in quadTree form. rotate90cw, rotate180,
-- and rotate 90ccw

-- Helper function which returns true if root of current quadTree is a Node.
isNode :: QuadTree -> Bool
isNode qt 
  | (take 4 (show qt)) == "Node" = True
  | otherwise = False

-- Helper function which returns true if root of current quadTree is a Leaf.
isLeaf :: QuadTree -> Bool
isLeaf qt
  | (take 4 (show qt)) == "Leaf" = True
  | otherwise = False

-- rotate90cw
-- Function which rotates an Image 90 degrees clockwise, by modifying the order of
-- nodes in its quad tree representation.
rotate90cw :: Image -> Image
rotate90cw (SquareImage w qt) = (SquareImage w (shiftNodes90cw qt))
rotate90cw (RectImage w h qt) = (RectImage h w (shiftNodes90cw qt))

-- rotate180
-- Function which rotates an Image 180 degrees, by modifying the order of
-- nodes in its quad tree representation.
rotate180 :: Image -> Image
rotate180 (SquareImage w qt) = (SquareImage w (shiftNodes180 qt))
rotate180 (RectImage w h qt) = (RectImage w h (shiftNodes180 qt))

-- rotate90ccw
-- Function which rotates an Image 90 degrees clockwise, by modifying the order of
-- nodes in its quad tree representation.
rotate90ccw :: Image -> Image
rotate90ccw (SquareImage w qt) = (SquareImage w (shiftNodes90ccw qt))
rotate90ccw (RectImage w h qt) = (RectImage h w (shiftNodes90ccw qt))

-- shiftNodes90cw 
-- This function rearranges all nodes in a QuadTree recursively,
-- such that the image representation rotates 90 degrees clockwise.
shiftNodes90cw :: QuadTree -> QuadTree
shiftNodes90cw (Node a b c d) = (Node (shiftNodes90cw c) (shiftNodes90cw a) (shiftNodes90cw d) (shiftNodes90cw b))
shiftNodes90cw x = x

-- shiftNodes180
-- This function rearranges all nodes in a QuadTree recursively,
-- such that the image representation rotates 180 degrees.
shiftNodes180 :: QuadTree -> QuadTree
shiftNodes180 (Node a b c d) = (Node (shiftNodes180 d) (shiftNodes180 c) (shiftNodes180 b) (shiftNodes180 a))
shiftNodes180 x = x

-- shiftNodes90ccw 
-- This function rearranges all nodes in a QuadTree recursively,
-- such that the image representation rotates 90 degrees counter-clockwise.
shiftNodes90ccw :: QuadTree -> QuadTree
shiftNodes90ccw (Node a b c d) = (Node (shiftNodes90ccw b) (shiftNodes90ccw d) (shiftNodes90ccw a) (shiftNodes90ccw c))
shiftNodes90ccw x = x


-- ============================================================================
-- PART 4 ---------------------------------------------------------------------
-- ============================================================================

-- Write a function named toHTML that generates the HTML SVG tags necessary
-- to render a quad tree representation in a browser.


toHTML :: Image -> String
toHTML (SquareImage width qt) = 
                "<html><head></head><body>\n" ++ 
                "<svg width=\"" ++ (show width) ++
                "\" height = \"" ++ (show width) ++ "\">\n" ++                
                (quadTreeHTML 0 0 (SquareImage width qt)) ++
                "</svg>\n</body>\n</html>"
toHTML (RectImage w h qt) =
                "<html><head></head><body>\n" ++ 
                "<svg width=\"" ++ (show w) ++
                "\" height = \"" ++ (show h) ++ "\">\n" ++                
                (quadTreeHTML 0 0 (RectImage w h qt)) ++
                "</svg>\n</body>\n</html>"
-- quadTreeHTML 
-- Helper function, which recursively scans through a quad tree data structure
-- and outputs HTML tags to print squares of leaf values.
-- Type Definition: Int x_coord -> Int y_coord -> Image -> String
quadTreeHTML :: Int -> Int -> Image -> String
quadTreeHTML _ _ (SquareImage w NilT) = ""
quadTreeHTML _ _ (RectImage w h NilT) = ""
quadTreeHTML x y (SquareImage w (Leaf r g b)) = 
                   "<rect x=" ++ (show x) ++ " y=" ++ (show y) ++
                   " width=" ++ (show w) ++ " height=" ++ (show w) ++
                   " style=\"fill:rgb(" ++ (show r) ++ "," ++ (show g) ++
                   "," ++ (show b) ++ ")\" />\n"
quadTreeHTML x y (SquareImage w (Node a b c d)) =
                   -- Recursively call function on:
                   -- Top left quadrant (first node)				   
                   quadTreeHTML x y (SquareImage (w `div` 2) a) ++
                   -- Top right quadrant (second node)
                   quadTreeHTML (x + (w `div` 2)) y (SquareImage (w `div` 2) b) ++
                   -- Bottom left quadrant (third node)
                   quadTreeHTML x (y + (w `div` 2)) (SquareImage (w `div` 2) c) ++
                   -- Bottom right quadrant (fourth node)
                   quadTreeHTML (x + (w `div` 2)) (y + (w `div` 2)) (SquareImage (w `div` 2) d)
-- Same function, but with modifications for a rectangular image.
quadTreeHTML x y (RectImage w h (Leaf r g b)) = 
                   "<rect x=" ++ (show x) ++ " y=" ++ (show y) ++
                   " width=" ++ (show w) ++ " height=" ++ (show h) ++
                   " style=\"fill:rgb(" ++ (show r) ++ "," ++ (show g) ++
                   "," ++ (show b) ++ ")\" />\n"
quadTreeHTML x y (RectImage w h (Node a b c d)) =
                   -- Recursively call function on:
                   -- Top left quadrant (first node)				   
                   quadTreeHTML x y (RectImage (w `div` 2) (h `div` 2) a) ++
                   -- Top right quadrant (second node)
                   quadTreeHTML (x + (w `div` 2)) y (RectImage (w `div` 2) (h `div` 2) b) ++
                   -- Bottom left quadrant (third node)
                   quadTreeHTML x (y + (h `div` 2)) (RectImage (w `div` 2) (h `div` 2) c) ++
                   -- Bottom right quadrant (fourth node)
                   quadTreeHTML (x + (w `div` 2)) (y + (h `div` 2)) (RectImage (w `div` 2) (h `div` 2) d)


-- ============================================================================
-- Code Included in Assignment ------------------------------------------------
-- ============================================================================

--
-- The following functions are a simple PNG file loader.  Note that these
-- functions will not load all PNG files.  They makes some assumptions about
-- the structure of the file that are not required by the PNG standard.
--

--
-- Convert 4 8-bit words to a 32 bit (or larger) integer
--
make32Int :: Word8 -> Word8 -> Word8 -> Word8 -> Int 
make32Int a b c d = ((((fromIntegral a) * 256) + 
                       (fromIntegral b) * 256) + 
                       (fromIntegral c) * 256) + 
                       (fromIntegral d)

--
-- Get a list of all of the PNG blocks out of a list of bytes
--
getBlocks :: [Word8] -> [(String, [Word8])]
getBlocks [] = []
getBlocks (a:b:c:d:e:f:g:h:xs) = (name, take (size+12) (a:b:c:d:e:f:g:h:xs)) : getBlocks (drop (size + 4) xs)
  where
    size = make32Int a b c d
    name = (chr (fromIntegral e)) : (chr (fromIntegral f)) :
           (chr (fromIntegral g)) : (chr (fromIntegral h)) : []

--
-- Extract the information out of the IHDR block
--
getIHDRInfo :: [(String, [Word8])] -> (Int, Int, Int, Int)
getIHDRInfo [] = error "No IHDR block found"
getIHDRInfo (("IHDR", (_:_:_:_:_:_:_:_:w1:w2:w3:w4:h1:h2:h3:h4:bd:ct:_)) : _) = (make32Int w1 w2 w3 w4, make32Int h1 h2 h3 h4, fromIntegral bd, fromIntegral ct)
getIHDRInfo (x : xs) = getIHDRInfo xs

--
-- Extract and decompress the data in the IDAT block. Note that this function
-- only handles a single IDAT block, but the PNG standard permits multiple
-- IDAT blocks.
--
getImageData :: [(String, [Word8])] -> [Word8]
getImageData [] = error "No IDAT block found"
getImageData (("IDAT", (_:_:_:_:_:_:_:_:xs)) : _) = BS.unpack (Z.decompress (BS.pack (take (length xs - 4) xs)))
getImageData (x:xs) = getImageData xs

--
-- Convert a list of bytes to a list of color tuples
--
makeTuples :: [Word8] -> [(Int, Int, Int)]
makeTuples [] = []
makeTuples (x : y : z : vals) = (fromIntegral x, fromIntegral y, fromIntegral z) : makeTuples vals

--
-- Convert a list of bytes that have been decompressed from a PNG file into
-- a two dimensional list representation of the image
--
imageBytesToImageList :: [Word8] -> Int -> [[(Int, Int, Int)]]
imageBytesToImageList [] _ = []
imageBytesToImageList (_:xs) w = makeTuples (take (w * 3) xs) : imageBytesToImageList (drop (w * 3) xs) w

--
-- Determine how many IDAT blocks are in the PNG file
--
numIDAT :: [(String, [Word8])] -> Int
numIDAT vals = length (filter (\(name, dat) -> name == "IDAT") vals)

--
-- Convert the entire contents of a PNG file (as a ByteString) into 
-- a two dimensional list representation of the image
--
decodeImage :: BS.ByteString -> [[(Int, Int, Int)]]
decodeImage bytes
  | header == [137,80,78,71,13,10,26,10] &&
    colorType == 2 &&
    bitDepth == 8 = imageBytesToImageList imageBytes w
  | numIDAT blocks > 1 = error "The image contained too many IDAT blocks"
  | otherwise = error ("Invalid header\ncolorType: " ++ (show colorType) ++ "\nbitDepth: " ++ (show bitDepth) ++ "\n")
  where
    header = take 8 $ BS.unpack bytes
    (w, h, bitDepth, colorType) = getIHDRInfo blocks
    imageBytes = getImageData blocks
    blocks = getBlocks (drop 8 $ BS.unpack bytes)



-- ============================================================================
-- MAIN ----------------------------------------------------------------------
-- ============================================================================


-- Load a PNG file, convert it to a quad tree, rotate it 90, 180 and 270 
-- degrees, and write all four images to an .html file.
--
main :: IO ()
main = do
  -- Change the name inside double quotes to load a different file
  input <- BS.readFile "ThisSideUp.png"
  let image = decodeImage input
  -- image is the list representation of the image stored in the .png file

  -- Convert the list representation of the image into a tree representation
  let qtree = createTree image

  -- Rotate the tree representation of the image
  let rot90 = rotate90cw qtree
  let rot180 = rotate180 qtree
  let rot270 = rotate90ccw qtree
 
  -- Write the original image and the rotated images to quadtree.html

  writeFile "quadtree.html" ((toHTML qtree) ++ "<br><br><br>" ++
                             (toHTML rot90) ++ "<br><br><br>" ++
                             (toHTML rot180) ++ "<br><br><br>" ++
                             (toHTML rot270) ++ "<br><br><br>")

-- ============================================================================
-- TESTS ----------------------------------------------------------------------
-- ============================================================================		

test1 :: IO ()
test1 = do
  -- Convert the list representation of the image into a tree representation
  let qtree = (RectImage 50 50 (listToQuadTree [[(1,1,1),(137,137,137),(1,1,1)],[(199,39,53),(199,39,53)],[(199,39,53),(99,139,53)],[(199,39,53),(199,39,153)]]))

  -- Rotate the tree representation of the image
  let rot90 = rotate90cw qtree
  let rot180 = rotate180 qtree
  let rot270 = rotate90ccw qtree
 
  -- Write the original image and the rotated images to quadtree.html

  writeFile "test.html" ((toHTML qtree) ++ "<br><br><br>" ++
                             (toHTML rot90) ++ "<br><br><br>" ++
                             (toHTML rot180) ++ "<br><br><br>" ++
                             (toHTML rot270) ++ "<br><br><br>")