module Tests (
  Point, Line, Path, Picture, Colour,
  white, black, blue, red, green, orange, magenta, lightgreen, darkblue,
  drawPicture,
) where

import Graphics.Rasterific hiding (Point, Vector, Line, Path, polygon)
import Graphics.Rasterific.Texture
import Codec.Picture

data Point = Point Float Float deriving Show
data Line = Line Point Point deriving Show
data Path = Path [Point] deriving Show
data Colour = Colour {rColour::Int, gColour::Int, bColour::Int, aColour::Int} deriving Show
data Sprite = Sprite Colour Path deriving Show
data Picture = Picture [Sprite] deriving Show

white, black, blue, red, green, orange, magenta, lightgreen, darkblue :: Colour
white      = Colour 255  255 255 255
black      = Colour   0    0   0 255
blue       = Colour   0  110 255 255
red        = Colour 255    0   0 255
green      = Colour  10  255  10 235
orange     = Colour 255  255   0 200
magenta    = Colour 153    0 153 220
lightgreen = Colour  27  230  34 255
darkblue   = Colour  24   50 194 255

rotate :: Float -> Line -> Line
rotate alpha (Line (Point x1 y1) (Point x2 y2))
  = Line (Point x1 y1) (Point (x' + x1) (y' + y1))
  where
    x0 = x2 - x1
    y0 = y2 - y1
    x' = x0 * cos alpha - y0 * sin alpha
    y' = x0 * sin alpha + y0 * cos alpha

scale :: Float -> Line -> Line
scale factor (Line (Point x1 y1) (Point x2 y2))
  = Line (Point x1 y1) (Point (x' + x1) (y' + y1))
  where
    x0 = x2 - x1
    y0 = y2 - y1
    x' = factor * x0 
    y' = factor * y0
  
connectLine :: Line -> Line -> Line
connectLine (Line _ p2) line = startLineFrom p2 line

startLineFrom :: Point -> Line -> Line
startLineFrom startPoint@(Point x0 y0) (Line (Point xS yS) (Point xE yE))
  = Line startPoint (Point (x0 + xE - xS) (y0 + yE - yS))

spiral :: Float -> Float -> Int -> Line -> Path
spiral angle scaleFactor n line = Path (spiral' n line)
  where
    spiral' :: Int -> Line -> [Point]
    spiral' n (line@(Line p _))
      | n <= 0    = []
      | otherwise = p:(spiral' (n - 1) newLine)
      where
        newLine = connectLine line (scale scaleFactor (rotate angle line))
        
polygon :: Int -> Line -> Path
polygon n line | n > 2 = spiral rotationAngle 1 (n + 1) line
  where 
    rotationAngle = (2 * pi) / (fromIntegral n)

bleach :: Colour -> Colour
bleach (Colour r g b a) = Colour (min 255 (r + 18)) (min 255 (g + 18)) (min 255 (b + 18)) a
          
tree :: Int -> Float -> Colour -> Line -> Picture
tree n factor colour line = Picture (tree' n colour line)
  where
    tree' :: Int -> Colour -> Line -> [Sprite]
    tree' 0 colour (Line p1 p2) = [Sprite colour (Path [p1, p2])]
    tree' n colour line         = [Sprite colour (Path [p2, p3])] 
                               ++ [Sprite colour (Path [p4, p1])]
                               ++ tree' (n - 1) colour' (Line p5 p3)
                               ++ tree' (n - 1) colour' (Line p4 p5)
      where 
        colour'             = bleach colour
        Path[p1,p2,p3,p4,_] = polygon 4 line
        Line _ p5           = rotate (factor * pi)
                              $ (\(Line x y) -> (Line y x)) 
                              $ scale 0.5 (Line p3 p4)
        
drawPicture :: Float -> Picture -> Image PixelRGBA8
drawPicture linewidth picture
  = renderDrawing  800 800 (toColour black) $
      mapM_ (\(Sprite colour (Path points)) -> 
        withTexture (uniformTexture $ toColour colour) (drawPath points)) (toSprites picture)
  where
    drawPath points    
         = stroke linewidth  JoinRound (CapRound, CapStraight 0) 
           $ polyline (map (\(Point x y) -> V2 x y) points)
    toColour (Colour r g b a) 
         = PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
    toSprites (Picture sprites) = sprites





















