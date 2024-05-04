module Intray.API.Image where

import Codec.Picture
import Codec.Picture.Extra
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB

downsizeImage :: ByteString -> Either String ByteString
downsizeImage contents = do
  di <- decodeImage contents
  let image = convertRGB8 di
      w = imageWidth image :: Int
      h = imageHeight image :: Int
      maxDim = 512
  pure $
    if w <= maxDim && h <= maxDim
      then contents
      else
        let d = maxDim :: Int -- desired
            (w', h') =
              case compare w h of
                EQ -> (d, d)
                -- If width is smaller than height, it's a portrait image.
                -- In that case we want the height to be equal to the desired height
                -- and the width to be adjusted while keeping the image ratio.
                -- we want:
                --
                --  w / h == w' / h'
                --
                -- h' = d
                -- w' = d * w / h
                --
                -- This works:
                --
                --  w / h == (d * w / h) /  d
                --
                -- The same reasoning works for the GT case.
                LT -> (round (fromIntegral d * fromIntegral w / fromIntegral h :: Double), d)
                GT -> (d, round (fromIntegral d * fromIntegral h / fromIntegral w :: Double))
            image' = scaleBilinear w' h' image
         in LB.toStrict $ encodePng image'
