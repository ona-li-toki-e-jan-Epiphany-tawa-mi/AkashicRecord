module Main (main) where

import Data.Fixed hiding (resolution)
import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

type Seconds    = Float
-- |Frames rendered inside a window.
type Frames     = Int
-- |The resolution of a window.
type Resolution = (Int, Int)
-- |A line between two points on a circle's circumference.
type Chord = (Float, Float)



{- MIT License

  Copyright (c) 2023 ona-li-toki-e-jan-Epiphany-tawa-mi

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE. -}

{- This is the Haskell implementation of multiply-by-n, using Gloss to generate
   graphics. -}



{- Config START -}
-- |The frames per second of the animation.
fps :: Frames
fps = 30

-- |The default size of the window. Must be in a separate variable for
-- initializing the animation state.
initialWindowSize :: Resolution
initialWindowSize = (400, 400)

-- |The configuration for the window to create.
window :: Display
window = InWindow "multply-by-n" initialWindowSize (200, 200)


-- |A multiplier that is applied to the speed of the progression of the
-- animation.
animationSpeedMultiplier :: Float
animationSpeedMultiplier = 0.25

-- |A multiplier that is applied to the spped of the change of the opacity.
opacityChangeMultiplier :: Float
opacityChangeMultiplier = 0.03 / fromIntegral fps

-- |How many points to generate along the cicumference of the circle, from which
-- comes a line.
circumferenceDivisions :: Float
circumferenceDivisions = fromIntegral (100 :: Int)

-- |A multiplier that is applied to the size of the circle, which is the size of
-- the smallest dimension of the window.
circleScaleMultiplier :: Float
circleScaleMultiplier = 0.95
{- Config END -}



-- |Takes a 3 argument function and reverses the order of it's arguments.
flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f a b c = f c b a

-- |Applies a function to both of the elements of a tuple.
both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

-- |Applies two functions to a value individually and places the results in a
-- tuple.
forkTuple :: (a -> b) -> (a -> c) -> a -> (b, c)
forkTuple f g a = (f a, g a)

-- |Turns a tuple into a two-element list.
untuple :: (a, a) -> [a]
untuple (a, b) = [a, b]



-- |"Stored" state information for the animation.
data AnimationState = AnimationState { windowSize :: Resolution
                                       -- ^The size of the application window.
                                     , multiplier :: Float
                                       -- ^The multiplier applied to each point
                                       -- to get the one to draw a line to.
                                     , opacities  :: [Float]
                                       -- ^An infinite list of opacity values from 0.0 ()
                                     }

-- | Generates the initial state for the animation, given the initial window size.
initialAnimationState :: Resolution -> AnimationState
initialAnimationState = (flip3 AnimationState) opacityCycle 0.0
  where
    {- An infinite list is much more convienient than storing an extra datapoint
       in the animation state that keeps track of whether to increase or
       decrease the opacity along with a bunch of supporting if statements. -}
    opacityCycle :: [Float]
    opacityCycle = cycle $ liftM2 (++) id reverse [0.0, opacityChangeMultiplier .. 1.0]

-- |Computes the next state of the animation after the given time has passed.
-- Make sure to call around (fps :: Int) times per second or the opacity cycling
-- will be messed up.
advanceAnimation :: Seconds -> AnimationState -> AnimationState
advanceAnimation deltaTime state = state { multiplier = multiplier state + deltaTime * animationSpeedMultiplier
                                         , opacities = tail $ opacities state
                                         }



-- |Finds a scalar from the radius of the unit circle to the full-scale image.
computeCircleRadius :: Resolution -> Float
computeCircleRadius resolution = fromIntegral (minimum resolution) / 2.0 * circleScaleMultiplier

-- |Computes a point along the circumference of a circle by taking the given
-- point and scaling it with the scalar, returning a chord formed from the two
-- points.
generateChord :: Float -> Float -> Float -> Chord
generateChord circumference scalar = forkTuple id ((flip mod') circumference . (* scalar))

-- |Takes a chord along the circumference of a circle and computes the
-- corresponding (x,y) points along the unit circle that represent that coord.
computeChordCoordinates :: Float -> Chord -> Path
computeChordCoordinates circumference = map (forkTuple cos sin . (*) circumferenceToRadians) . untuple
  where
    circumferenceToRadians :: Float
    circumferenceToRadians = 2 * pi / circumference

-- |Scales the points of a path and then converts it to a renderable line.
scaleToLine :: Float -> Path -> Picture
scaleToLine scalar = line . map (both $ (*) scalar)

-- |Converts the given animation state to a renderable image.
render :: AnimationState -> Picture
render state = color (greyN $ head $ opacities state)
             $ pictures [ circle circleRadius
                        , pictures $ map ( scaleToLine circleRadius
                                         . computeChordCoordinates circumferenceDivisions
                                         . generateChord circumferenceDivisions (multiplier state)
                                         ) [1.0 .. circumferenceDivisions]
                        ]
  where
    circleRadius :: Float
    circleRadius = computeCircleRadius $ windowSize state



-- |Update the animation state with any changes to the size of the window to
-- properly size the animation.
handleResize :: Event -> AnimationState -> AnimationState
handleResize (EventResize size) state = state {windowSize = size}
handleResize _                  state = state

main :: IO ()
main = play window black fps (initialAnimationState initialWindowSize) render handleResize advanceAnimation
