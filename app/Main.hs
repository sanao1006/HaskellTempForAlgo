{-# LANGUAGE BangPatterns , BlockArguments ,FlexibleContexts ,FlexibleInstances ,OverloadedStrings ,TypeApplications ,MultiParamTypeClasses ,TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

import Control.Monad (replicateM,forM_,when,(<=<))
import Data.Maybe (fromJust)
import Data.Array.IO(IOUArray,IOArray)
import Data.Array.Unboxed(UArray)
import qualified Data.Array.MArray     as MA
import qualified Data.Array.IArray     as A
import qualified Data.ByteString.Char8 as BS
main = do
    putStrLn "Hello, World"


type Bs = BS.ByteString
type Height = Int
type Width = Int
type MutableArray2D any = IOUArray (Height,Width) any
type Array2D any = UArray (Height, Width) any

-- 初期化DP配列用意(絶対もっと短く描けるはず...)===========================
class DPArray a where
    initDP :: Height -> Width -> a -> IO(MutableArray2D a)

instance DPArray Int where
  initDP height width element = do
    MA.newArray((0,0),(height-1,width-1)) element

instance DPArray Double where
  initDP height width element = do
    MA.newArray((0,0),(height-1,width-1)) element

instance DPArray Bool where
  initDP height width element = do
    MA.newArray((0,0),(height-1,width-1)) element
-- =================================================================


-- 入力 =====================================================================================================================================================
class (Read a) => Input a where
  read' :: BS.ByteString -> a
  readArray2D :: Height -> Width -> BS.ByteString -> a

input :: Input a => IO a
input = read' <$> BS.getLine

inputs :: Input a => IO a
inputs = read' <$> BS.getContents

inputArray :: Input a => Int -> Int -> IO a
inputArray = flip flip BS.getContents . ((<$>) .) . readArray2D

instance Input Bs where
  read' = id

instance Input Int where
  read' = fst . fromJust . BS.readInt

instance Input Double where
  read' = read . BS.unpack

instance Input [Bs] where
  read' = BS.words

instance Input [Int] where
  read' = map ((read @Int) . BS.unpack) . BS.words

instance Input [Double] where
  read' = map ((read @Double) . BS.unpack) . BS.words

instance Input [(Int, Bs)] where
  read' = map ((\[a, b] -> (fst (fromJust (BS.readInt a)), b)) . BS.words) . BS.lines

instance Input [(Bs, Int)] where
  read' = map ((\[a, b] -> (a, fst (fromJust (BS.readInt b)))) . BS.words) . BS.lines

instance Input [[Bs]] where
  read' = map BS.words . BS.lines

instance Input [[Int]] where
  read' = map (map (fst . fromJust . BS.readInt) . BS.words) . BS.lines

instance Input [[Double]] where
  read' = map (map ( (read @Double) . BS.unpack) . BS.words) . BS.lines

instance Input (Array2D Int) where
  readArray2D = flip flip ((map (fst . fromJust . BS.readInt) . BS.words) <=< BS.lines) . (((.) . A.listArray . ((0, 0) ,)) .) . (. subtract 1) . (,) . subtract 1

instance Input (Array2D Double) where
  readArray2D height width = A.listArray ((0, 0), (height - 1, width - 1)) . concatMap (map ((read @Double) . BS.unpack) . BS.words) . BS.lines

instance Input (Array2D Char) where
 readArray2D = flip flip (BS.unpack <=< BS.lines) . (((.) . A.listArray . ((0, 0) ,)) .) . (. subtract 1) . (,) . subtract 1
-- ============================================================================================================================================================