{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Wires.Game where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Managed
import Control.Wire
import Control.Wire.Controller
import Control.Wires.Extra
import Data.Align
import Data.Bits
import Data.Foldable
import qualified Data.IntMap.Strict as IntMap
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.These
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import Foreign
import GLObjects
import Graphics.GL.ARB.SeparateShaderObjects
import Graphics.GL.Core33
import Linear
import Parser
import Prelude hiding ((.), id)
import Render
import RenderGraph
import qualified SDL
import qualified SDL.Event
import SDL.Lens
import System.Clock
import Wires.Camera

sinkM44
  :: MonadIO m
  => UniformLocation -> Wire m (Event (M44 Float)) (Event ())
sinkM44 (UniformLocation (Program p) l) = proc update ->
  onEvent -< update <&> \mat ->
    liftIO $
    Foreign.with mat $
    glProgramUniformMatrix4fv p l 1 (fromIntegral GL_TRUE) . castPtr

ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

--------------------------------------------------------------------------------
game
  :: (Monoid (f (GLIO a)), Functor f)
  => BSPFile
  -> [GLuint]
  -> Map.Map Parser.Texture (Double -> Maybe GLObjects.Texture -> f a)
  -> UniformLocation
  -> Wire IO t (Event (Pass (f (GLIO a))))
game bspFile lightMaps compiledShaders u_view = proc _ -> do
  maybeSdlEvent <- newEvent -< Just <$> SDL.pollEvent
  let sdlEvent = filterJust maybeSdlEvent
      eventQueueEmpty = filterE isNothing maybeSdlEvent

  -- An event telling us the current clock time every step
  t <- newEvent -< Just <$> getTime Monotonic

  -- Calculate the delta per step. First step has dt=0, as we initialize with
  -- getTime Monotonic (which is what t contains).
  dt
    <- withM_ unfoldE (getTime Monotonic)
    -< fmap (\t t' -> (secs (t - t'), t)) t

  tGame <- scan 0 -< fmap (+) dt

  fps <- hold 0 . (fmap recip <$> average 25) -< dt

  -- Whenever we have a delta, increment the accumulator. If we are able to
  -- step the simulation (accumulator > stepPhysics), then do so, and take
  -- that time out of the accumulator.
  stepPhysics
    <- fmap filterJust (unfoldE 0)
    -< dt <&> \delta accumulator ->
          let accumulator' = accumulator + delta
          in if accumulator' > physicsStep
              then (Just physicsStep, accumulator' - physicsStep)
              else (Nothing, accumulator')

  V2 windowWidth windowHeight
    <- hold (V2 800 600 :: V2 Int32)
    -< fmap SDL.Event.windowResizedEventSize
            (filterJust (fmap (preview (payload . _WindowResizedEvent))
                              sdlEvent))

  let projection = perspective 1.047 (fromIntegral windowWidth / fromIntegral windowHeight) 0.1 5000
  (cameraLoc, view) <- camera -< CameraEvents {..}
  let rendered = view <$ eventQueueEmpty `unlessE` stepPhysics

  initial -< putStrLn "Game running!"
  frameTime
    <- fmap filterJust (unfoldE 0)
    -< align dt rendered <&> \e frameTime ->
         case e of
           This delta -> (Nothing, frameTime + delta)
           That _ -> undefined
           These delta _ -> (Just (frameTime + delta), 0)

  -- onEvent -< putStrLn . ("Step: " ++) . show <$> dt
  -- onEvent -< print <$ align dt rendered
  onEvent -< print . recip <$> frameTime

  let currentLeaf = findLeaf (realToFrac <$> cameraLoc) bspFile
      currentCluster = leafCluster currentLeaf
      scene =
        IntMap.foldlWithKey'
          (\s clusterNumber clusterScene ->
             if clusterVisible (bspVisData bspFile)
                (fromIntegral currentCluster)
                (fromIntegral clusterNumber)
             then clusterScene tGame <> s
             else s)
          mempty
          clusters

  -- Whenever we aren't stepping physics, render what we have.
  sinkM44 u_view -<
    projection !*! view <$ rendered
  returnA -< Pass tGame (Framebuffer 0) (0,0,windowWidth,windowHeight) scene <$ rendered

  where

  secs = (/ 1000000000) . fromInteger . toNanoSecs
  physicsStep = 1 / 60
  clusters = doDraw lightMaps bspFile compiledShaders

average :: (Fractional a, Monad m) => Int -> Wire m (Event a) (Event a)
average n = proc e -> unfoldE Seq.empty -< fmap go e
    where
    go x xs' =
        let xs = Seq.take n (x Seq.<| xs')
        in (foldl' (+) 0 xs / fromIntegral (Seq.length xs),
            xs)

findLeaf :: V3 FloatLE -> BSPFile -> Leaf
findLeaf loc bspFile = go (bspNodes bspFile SV.! 0)
  where
    go node =
      let plane = bspPlanes bspFile SV.! fromIntegral (nodePlane node)
          distance = planeNormal plane `dot` loc - planeDist plane
          nextIndex
            | distance >= 0 = nodeFront node
            | otherwise = nodeBack node
      in if nextIndex >= 0
           then go (bspNodes bspFile SV.! fromIntegral nextIndex)
           else bspLeafs bspFile V.! (negate (fromIntegral nextIndex) - 1)

clusterVisible :: VisData -> Int32 -> Int32 -> Bool
clusterVisible VisData{..} a b
    | a >= 0 = 0 /= (visSet .&. (shiftL 1 (fromIntegral (b .&. 7))))
    | otherwise = True
  where
    i = a * fromIntegral visDataSzVecs + shiftR b 3
    visSet = visDataVecs SV.! fromIntegral i

-- doDraw
--   :: Double
--   -> t2
--   -> [GLuint]
--   -> BSPFile
--   -> t1
--   -> t
--   -> Map.Map Parser.Texture Material
--   -> IntMap.IntMap (Cull (MultiplePasses (BindTexture (SetUniform Bool (SetUniform (M33 Float) (BlendMode (DepthFunc (GLIO ()))))))))
doDraw lightMaps bspFile compiledShaders =
  snd
    (V.foldl'
       (\(seen, clusters) (cluster, leafFaceIndex) ->
          let LeafFace i = bspLeafFaces bspFile V.! leafFaceIndex
              Face {..} = bspFaces bspFile V.! fromIntegral i
              usesLightMap =
                faceLMIndex >= 0 && fromIntegral faceLMIndex < length lightMaps
              shader t =
                (compiledShaders Map.!
                 (bspTextures bspFile V.! fromIntegral faceTexture))
                  t
                  (if usesLightMap
                     then Just
                            (GLObjects.Texture
                               (lightMaps !! fromIntegral faceLMIndex))
                     else Nothing)
              face t =
                glIO
                  (glDrawElementsBaseVertex
                     GL_TRIANGLES
                     (fromIntegral faceNMeshVerts)
                     GL_UNSIGNED_INT
                     (intPtrToPtr
                        (fromIntegral $
                         fromIntegral faceMeshVert *
                         fromIntegral (sizeOf (undefined :: Int32))))
                     (fromIntegral faceVertex)) <$>
                shader t
          in if i `Set.member` seen
               then (seen, clusters)
               else ( Set.insert i seen
                    , IntMap.insertWith
                        mappend
                        (fromIntegral cluster)
                        (\t -> face t)
                        clusters))
       (mempty, mempty)
       (V.concatMap
          (\leaf ->
             V.map
               (\i -> (leafCluster leaf, i))
               (V.enumFromN
                  (fromIntegral (leafFirstLeafFace leaf))
                  (fromIntegral (leafNLeafFaces leaf))))
          (bspLeafs bspFile)))
