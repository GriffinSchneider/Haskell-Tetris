
module Constants (
  V2(V2),
  V4(V4),
  translateV2,
  kWorldBounds
) where

import Graphics.Rendering.OpenGL

kWorldBounds = V4 (-4) (-100) 5 9


data V2 = V2 Float Float

instance Show V2 where show v = showsV2 v ""

showsV2 :: V2 -> ShowS
showsV2 (V2 x y) = ('[':) . shows x . (", "++) . shows y . (']':)

translateV2 :: V2 -> V2 -> V2
translateV2 (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)


data V4 = V4 Float Float Float Float


