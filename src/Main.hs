{-# LANGUAGE ScopedTypeVariables #-}

import System.Random
import Data.Traversable (sequenceA)
import Control.Arrow (second)
import Graphics.Gloss hiding (Point, rotate)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Reactive.Banana
import Reactive.Banana.Frameworks
import Graphics.Gloss.Interface.FRP.ReactiveBanana


main :: IO ()
main = playBanana (InWindow "Tetris" (360, 660) (400, 50))
        black 60 0.05 tetris

tetris :: forall t. Frameworks t =>
    Event t Float -> Event t InputEvent -> Moment t (Behavior t Picture)
tetris eTick eEvent = do
    brandom <- fromPoll (randomRIO (0, length blocks - 1) :: IO Int)
    let
       eMoveSpeed = throttle 4 eTick
      
       bPosition = accumB (4, 20) $
        (moveLeft <$> bBase <*> bBlock <@ eLeft) `union`
        (moveRight <$> bBase <*> bBlock <@ eRight) `union`
        (moveDown <$> bBase <*> bBlock <@ eDown `union'` eMoveSpeed) `union`
        (pure (4, 20) <$ eNewBlock)
               
       bCanDown = not <$> (collision <$> bBase <*> bBlock <*> (second (subtract 1) <$> bPosition))
       
       eNewBlock = ePut `union'` whenEDropN (not <$> bCanDown) eMoveSpeed 4
       
       bBlock = accumB oBlock $
        (rotate <$> bBase <*> bPosition <@ eRotate) `union`
        (pure . (blocks !!) <$> brandom <@ eNewBlock)
       
       bBase = accumB [] $
        (mergeBlock <$> bBlock <*> bPosition <@ eNewBlock) `union`
        (pure [] <$ eRestart)
       
       eRestart =  whenEDropN ((>= 20) . length <$> bBase) (() <$ eMoveSpeed) 4
       
       bBasePic = drawBase <$> bBase
       
       keyDown key = ekeyDown key eEvent `union'`
        whenEDropN (bKeyDown key eEvent) eTick 4    
       eRotate = ekeyDown (G.SpecialKey G.KeyUp) eEvent
       eLeft = keyDown $ G.SpecialKey G.KeyLeft
       eRight = keyDown $ G.SpecialKey G.KeyRight
       eDown = keyDown $ G.SpecialKey G.KeyDown
       ePut = keyDown $ G.SpecialKey G.KeySpace
       
       bBlockPic = translate <$> (fromIntegral . fst <$> bPosition) <*>
        (fromIntegral . snd <$> bPosition) <*> (drawBlock <$> bBlock)
    
    return $ translate (-135) (-285) <$> scale 30 30 <$>
        pictures <$> sequenceA [bBlockPic, bBasePic, pure frame]
        
type Point = (Int, Int)

moveLeft :: Base -> Block -> Point -> Point
moveLeft base block p@(x, y)
    | collision base block (x - 1, y) = p
    | otherwise = (x - 1, y)
    
moveRight :: Base -> Block -> Point -> Point
moveRight base block p@(x, y)
    | collision base block (x + 1, y) = p
    | otherwise = (x + 1, y)
    
moveDown :: Base -> Block -> Point -> Point
moveDown base block p@(x, y)
    | collision base block (x, y - 1) = p
    | otherwise = (x, y - 1)

rotate :: Base -> Point -> Block -> Block
rotate base p block
    | collision base block' p = block
    | otherwise = block'
    where block' = rotate' block
    
rotate' :: Block -> Block
rotate' [] = repeat []
rotate' (x:xs) = zipWith (:) (reverse x) $ rotate' xs

union' :: Event t a -> Event t b -> Event t ()
union' a b = (() <$ a) `union` (() <$ b)

whenEDropN :: Behavior t Bool -> Event t a -> Int -> Event t ()
whenEDropN b e n = () <$ filterE (> n)
    (accumE 0 $ (\ t -> if t then (+ 1) else pure 0) <$> b <@ e)
    
mergeBlock :: Block -> Point -> Base -> Base
mergeBlock b point base = filter (not . and) $ go pos block (zip base [0..])
    where
        go y blk [] = [fillLine b |b <- blk]
        go y [] base = fst <$> base
        go y blk@(b:bs) ((l, n):xs)
            | y == n = zipWith (||) (fillLine b) l : go (y + 1) bs xs
            | otherwise = l : go y blk xs
        fillLine b = replicate x False ++ b ++ replicate t False
        pos = touchPos base block p
        t = 10 - x - width block
        (block, p@(x, y)) = trimBlock b point

touchPos :: Base -> Block -> Point -> Int
touchPos base block (x, y)  = (+ 1) $ head $
    dropWhile (\p -> not $ collision base block (x, p) ) [y, y-1..(-1)]
    
trimBlock :: Block -> Point -> (Block, Point)
trimBlock b (x, y) = (b2, (x1, y1))
    where
        (b1, x1) = trimX b x
        (b2, y1) = trimY b1 y
        trimX b x = if or $ map head b then (b, x) else ((map tail b), x + 1)
        trimY b y = if or $ head b then (b, y) else ((tail b), y + 1)

collision :: Base -> Block -> Point -> Bool
collision base b p
    | x == -1 = True
    | y == -1 = True
    | x + width block > 10 = True
    | otherwise = or [or $ zipWith (&&) b l |
        (b, l) <- zip block $ drop y [drop x l|l <- base]]
    where (block, (x, y)) = trimBlock b p

throttle :: Int -> Event t a -> Event t ()
throttle n e = () <$ (\ x -> (x `mod` n) == 0) `filterE` accumE 0 ((+ 1) <$ e)

ekeyUp :: G.Key -> Event t InputEvent -> Event t InputEvent
ekeyUp key = filterE (keyUp key)
    where keyUp k1 (G.EventKey k2 G.Up _ _) = k1 == k2
          keyUp _ _ = False

ekeyDown :: G.Key -> Event t InputEvent -> Event t InputEvent
ekeyDown key = filterE (keyDown key)
    where keyDown k1 (G.EventKey k2 G.Down _ _) = k1 == k2
          keyDown _ _ = False

bKeyDown :: G.Key -> Event t InputEvent -> Behavior t Bool
bKeyDown key eEvent = stepper False $
    (True <$ ekeyDown key eEvent) `union`
    (False <$ ekeyUp key eEvent)
    
type Block = [[Bool]]
type Base = [[Bool]]


blocks :: [Block]
blocks = [oBlock, jBlock, lBlock, sBlock, zBlock, tBlock, strip]

oBlock, jBlock, lBlock, sBlock, zBlock, tBlock, strip :: Block
oBlock = [
          [True, True],
          [True, True]
          ]
          
jBlock = [
    [False, False, True],
    [True, True, True],
    [False, False, False]
    ]
    
lBlock = [
    [True, False, False],
    [True, True, True],
    [False, False, False]
    ]
    
tBlock = [
    [False, False, False],
    [True, True, True],
    [False, True, False]
    ]
    
sBlock = [
    [True, True, False],
    [False, True, True]
    ]
    
strip = [
    [False, False, False, False],
    [True, True, True, True],
    [False, False, False, False]
    ]
    
zBlock = [
    [False, True, True],
    [True, True, False]
    ]
          
width :: Block -> Int
width = maximum . fmap (length . dropWhile not . reverse)

drawLine :: Color -> [Bool] -> Picture
drawLine c l = pictures [translate i 0 $ color c square | (b, i) <- zip l [0..], b]

drawBlock :: Block -> Picture
drawBlock block = pictures $ 
    zipWith (\l i -> translate 0 i $ drawLine green l) block [0..]
            
drawBase :: Base -> Picture
drawBase base = pictures $
    zipWith (\l i -> translate 0 i $ drawLine yellow l) base [0..]
            
frame :: Picture
frame = pictures [translate 0 (-1) hbar, translate 0 20 hbar,
                  translate (-1) 0 vbar, translate 10 0 vbar,
                  bg]
    where hbar = translate (-1) 0 $ pictures 
                    [translate i 0 $ color (greyN 0.6) square | i <- [0..11]]
          vbar = pictures 
                    [translate 0 i $ color (greyN 0.6) square | i <- [0..19]]
          bg = pictures [translate x y $ color (greyN 0.2) $ rectangleWire 1 1 | x <- [0..9], y <- [0..19]]

square :: Picture
square = scale 0.9 0.9 $
    pictures [rectangleWire 1 1, scale 0.4 0.4 $ translate (-0.5) (-0.5) $
        pictures [translate x y $ scale 0.9 0.9 $ rectangleSolid 1 1 | x <- [0,1], y <- [0,1]]]
