module Cube (
  cube, cubeLines
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

-- Points on the cube
frontTopLeft  w = vertex $ Vertex3 (-w) (-w)   w
frontTopRight w = vertex $ Vertex3   w  (-w)   w
frontBotLeft  w = vertex $ Vertex3 (-w) (-w) (-w)
frontBotRight w = vertex $ Vertex3   w  (-w) (-w)
backTopLeft   w = vertex $ Vertex3 (-w)   w    w
backTopRight  w = vertex $ Vertex3   w    w    w
backBotLeft   w = vertex $ Vertex3 (-w)   w  (-w)
backBotRight  w = vertex $ Vertex3   w    w  (-w)

cube :: GLfloat -> IO()
cube w = renderPrimitive Quads $ do
  -- front face
  frontTopRight w
  frontBotRight w
  frontBotLeft  w
  frontTopLeft  w

  -- back face
  backTopRight  w
  backBotRight  w
  backBotLeft   w
  backTopLeft   w

  -- left face
  backTopLeft   w
  backBotLeft   w
  frontBotLeft  w
  frontTopLeft  w

  -- right face
  backTopRight  w
  backBotRight  w
  frontBotRight w
  frontTopRight w

  -- top face
  backTopRight  w
  frontTopRight w
  frontTopLeft  w
  backTopLeft   w

  -- bottom face
  backBotRight  w
  frontBotRight w
  frontBotLeft  w
  backBotLeft   w

cubeLines :: GLfloat -> IO()
cubeLines w = renderPrimitive Lines $ do
  -- around front face
  frontTopLeft  w >> frontTopRight w
  frontTopRight w >> frontBotRight w
  frontBotRight w >> frontBotLeft  w
  frontBotLeft  w >> frontTopLeft  w

  -- around back face
  backTopLeft   w >> backTopRight  w
  backTopRight  w >> backBotRight  w
  backBotRight  w >> backBotLeft   w
  backBotLeft   w >> backTopLeft   w

  -- connecting edges
  frontTopLeft  w >> backTopLeft   w
  frontTopRight w >> backTopRight  w
  frontBotRight w >> backBotRight  w
  frontBotLeft  w >> backBotLeft   w
