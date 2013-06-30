{-# LANGUAGE EmptyDataDecls #-}

import Prelude
import FFI
import FayRef

import DOM
import JQuery

import MLib

-- Image

data JSImage
type Image = Ptr JSImage

newImage :: Image
newImage = ffi "new Image()"

jsLoadImage :: String -> (Image -> Fay()) -> (String -> Fay()) -> Fay ()
jsLoadImage = ffi "glib.loadImage(%1, %2, %3)"

loadImage :: String -> (Either String Image -> Fay ()) -> Fay ()
loadImage src cb = jsLoadImage src (cb . Right) (cb . Left)

-- DOM

setId :: String -> DOM.Element -> Fay()
setId = ffi "%2['id'] = %1"

-- JQuery

appendHtml :: String -> JQuery -> Fay JQuery
appendHtml = ffi "%2['append'](%1)"

-- Canvas

type Canvas = DOM.Element

setW :: Int -> Canvas -> Fay ()
setW = ffi "%2['width'] = %1"

setH :: Int -> Canvas -> Fay ()
setH = ffi "%2['height'] = %1"

newCanvas :: String -> Int -> Int -> Fay Canvas
newCanvas cid w h = do
  c <- createElement "canvas"
  setW w c
  setH h c
  setId cid c
  return c

data Ctx2D

data Rect = Rect {
  rx :: Double, ry :: Double ,
  rw :: Double, rh :: Double }

ctx2d :: Canvas -> Fay Ctx2D
ctx2d = ffi "%1['getContext']('2d')"

ctxFillStyle :: String -> Ctx2D -> Fay ()
ctxFillStyle = ffi "%2['fillStyle'] = %1"

ctxFillRect :: Rect -> Ctx2D -> Fay ()
ctxFillRect r = jsCtxFillRect (rx r) (ry r) (rw r) (rh r)

jsCtxFillRect :: Double -> Double -> Double -> Double -> Ctx2D -> Fay ()
jsCtxFillRect = ffi "%5['fillRect'](%1, %2, %3, %4)"

drawImageAt :: Double -> Double -> Image -> Ctx2D -> Fay ()
drawImageAt = ffi "%4['drawImage'](%3, %1, %2)"

-- Example

onReady = do
  loadImage "http://upload.wikimedia.org/wikipedia/commons/thumb/5/57/Tokay_Gecko.jpg/174px-Tokay_Gecko.jpg" (either (const $ putStrLn "img load failed") withImg)

withImg img = do
  body <- select "body"
  appendHtml "<i>alma</i>" body
  select "body i" >>= setProp "id" "gyik"
  --
  canvas <- newCanvas "c1" 300 200 
  append canvas body
  ctx <- ctx2d canvas
  ctxFillStyle "#ff0000" ctx
  ctxFillRect (Rect 10 10 50 30) ctx
  drawImageAt 100 100 img ctx
main = ready onReady
