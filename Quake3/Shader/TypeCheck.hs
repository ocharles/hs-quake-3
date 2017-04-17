{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module transforms the result of "Quake3.Shader.Parser" into
-- type-checked shaders.
module Quake3.Shader.TypeCheck where

import Prelude hiding (map)
import Control.Applicative
import Control.Lens (ASetter', makeLenses, (<>~))
import Data.Either.Validation
import Data.Sequence (Seq)
import GHC.Generics
import Generics.Deriving.Monoid
import qualified Quake3.Shader.Parser as Parser
import Text.Read

modifying
  :: (Monoid s, Monoid e)
  => ASetter' a s -> Validation e s -> (Endo a, e)
modifying l (Success a) = (Endo (l <>~ a), mempty)
modifying _ (Failure es) = (mempty, es)

--------------------------------------------------------------------------------

data SortLayer
  = Portal
  | Environment
  | Opaque
  | Decal
  | SeeThrough
  | Banner
  | Fog
  | Underwater
  | Blend0
  | Blend1
  | Blend2
  | Blend3
  | Blend6
  | StencilShadow
  | AlmostNearest
  | Nearest
  deriving (Bounded, Enum, Eq, Ord, Show)

tcSortLayer :: [String] -> Validation [String] SortLayer
tcSortLayer ["portal"] = pure Portal
tcSortLayer ["sky"] = pure Environment
tcSortLayer ["opaque"] = pure Opaque
tcSortLayer ["decal"] = pure Decal
tcSortLayer ["seethrough"] = pure SeeThrough
tcSortLayer ["banner"] = pure Banner
tcSortLayer ["additive"] = pure Blend1
tcSortLayer ["nearest"] = pure Nearest
tcSortLayer ["underwater"] = pure Underwater
tcSortLayer [n] =
  case readMaybe n of
    Just i ->
      let sorts = [minBound .. maxBound]
      in if i < length sorts
           then pure (sorts !! i)
           else Failure ["Unknown sort index: " ++ show i]
    Nothing -> Failure ["Unknown sort: " ++ n]
tcSortLayer args = Failure ["Unknown sort: " ++ show args]

--------------------------------------------------------------------------------

data Function
  = Sin
  | Triangle
  | InverseSawtooth
  | Square
  | Sawtooth
  deriving (Show)

tcFunction :: String -> Validation [String] Function
tcFunction "sin" = pure Sin
tcFunction "triangle" = pure Triangle
tcFunction "inversesawtooth" = pure InverseSawtooth
tcFunction "square" = pure Square
tcFunction "sawtooth" = pure Sawtooth
tcFunction s = Failure ["Unknown function: " ++ s]

--------------------------------------------------------------------------------

tcDouble :: String -> Validation [String] Double
tcDouble ('-':s) = negate <$> tcDouble s
tcDouble ('.':s) = tcDouble ('0':'.':s)
tcDouble s =
  maybe (Failure ["\"" ++ s ++ "\" is not an Double"]) pure (readMaybe s)


--------------------------------------------------------------------------------

data WaveFunction = WaveFunction
  { wfFunction :: Function
  , wfBase :: Double
  , wfAmp :: Double
  , wfPhase :: Double
  , wfFreq :: Double
  } deriving (Show)

tcWaveFunction :: [String] -> Validation [String] WaveFunction
tcWaveFunction [waveFunction, base, amp, phase, freq] =
  WaveFunction <$> tcFunction waveFunction <*> tcDouble base <*> tcDouble amp <*>
  tcDouble phase <*>
  tcDouble freq
tcWaveFunction args = Failure ["Unknown wave function arguments: " ++ show args]

--------------------------------------------------------------------------------

data VertexDeformation
  = DeformWave Double WaveFunction
  | DeformAutosprite
  | DeformAutosprite2
  deriving (Show)

tcVertexDeformation :: [String] -> Validation [String] VertexDeformation
tcVertexDeformation ("wave":div_:args) =
  DeformWave <$> tcDouble div_ <*> tcWaveFunction args
tcVertexDeformation ["autosprite"] = pure DeformAutosprite
tcVertexDeformation ["autosprite2"] = pure DeformAutosprite2
tcVertexDeformation args = Failure ["Unknown vertex deformation: " ++ show args]


--------------------------------------------------------------------------------

data RGBGen
  = RGBGenWave WaveFunction
  | RGBIdentity
  | RGBIdentityLighting
  | RGBVertex
  | RGBLightingDiffuse
  | RGBEntity
  | RGBExactVertex
  deriving (Show)

tcRgbGen :: [String] -> Validation [String] RGBGen
tcRgbGen ("wave":args) = RGBGenWave <$> tcWaveFunction args
tcRgbGen ["identity"] = pure RGBIdentity
tcRgbGen ["identitylighting"] = pure RGBIdentityLighting
tcRgbGen ["vertex"] = pure RGBVertex
tcRgbGen ["lightingdiffuse"] = pure RGBLightingDiffuse
tcRgbGen ["entity"] = pure RGBEntity
tcRgbGen ["exactvertex"] = pure RGBExactVertex
tcRgbGen args = Failure ["Unknown rgbgen: " ++ show args]


--------------------------------------------------------------------------------

data Factor
  = One
  | DstColor
  | Zero
  | SrcAlpha
  | OneMinusSrcAlpha
  | OneMinusDstAlpha
  | OneMinusSrcColor
  | OneMinusDstColor
  | SrcColor
  deriving (Show)

tcFactor :: String -> Validation [String] Factor
tcFactor "gl_one" = pure One
tcFactor "gl_dst_color" = pure DstColor
tcFactor "gl_src_color" = pure SrcColor
tcFactor "gl_zero" = pure Zero
tcFactor "gl_src_alpha" = pure SrcAlpha
tcFactor "gl_one_minus_src_alpha" = pure OneMinusSrcAlpha
tcFactor "gl_one_minus_dst_alpha" = pure OneMinusDstAlpha
tcFactor "gl_one_minus_src_color" = pure OneMinusSrcColor
tcFactor "gl_one_minus_dst_color" = pure OneMinusDstColor
tcFactor s = Failure ["Unknown blending factor: " ++ s]


--------------------------------------------------------------------------------

data Map = MapTexture FilePath | MapLightMap | MapWhite
  deriving (Show)

tcMap :: String -> Validation [String] Map
tcMap "$lightmap" = pure MapLightMap
tcMap "$whiteimage" = pure MapWhite
tcMap s = pure (MapTexture s)


--------------------------------------------------------------------------------

data TCMod
  = TCModTurb TCModTurb
  | TCModScroll TCModScroll
  | TCModScale TCModScale
  | TCModStretch WaveFunction
  | TCModRotate Double
  deriving (Show)

data TCModTurb = Turb
  { turbBase :: Double
  , turbAmp :: Double
  , turbPhase :: Double
  , turbFreq :: Double
  } deriving (Show)

tcTurb :: [String] -> Validation [String] TCModTurb
tcTurb [base, amp, phase, freq] =
  Turb <$> tcDouble base <*> tcDouble amp <*> tcDouble phase <*>
  tcDouble freq
tcTurb args = Failure ["Unknown tcmod turb arguments: " ++ show args]

data TCModScroll = TCScroll
  { tcScrollSSpeed :: Double
  , tcScrollTSpeed :: Double
  } deriving (Show)

tcScroll :: [String] -> Validation [String] TCModScroll
tcScroll (sspeed:tspeed:_) = TCScroll <$> tcDouble sspeed <*> tcDouble tspeed
tcScroll args = Failure ["Unknown tcmod scroll arguments: " ++ show args]

data TCModScale = TCScale
  { tcScrollSFactor :: Double
  , tcScrollTFactor :: Double
  } deriving (Show)

tcScale :: [String] -> Validation [String] TCModScale
tcScale (sfactor:tfactor:_) = TCScale <$> tcDouble sfactor <*> tcDouble tfactor
tcScale args = Failure ["Unknown tcmod scale arguments: " ++ show args]

tcTCMod :: [String] -> Validation [String] TCMod
tcTCMod ("turb":args) = TCModTurb <$> tcTurb args
tcTCMod ("scroll":args) = TCModScroll <$> tcScroll args
tcTCMod ("scale":args) = TCModScale <$> tcScale args
tcTCMod ("stretch":args) = TCModStretch <$> tcWaveFunction args
tcTCMod ("rotate":[theta]) = TCModRotate <$> tcDouble theta
tcTCMod args = Failure ["Unknown tcmod: " ++ show args]


--------------------------------------------------------------------------------

data TCGen
  = TCGenBase
  | TCGenLightmap
  | TCGenEnvironment
  deriving (Show)

tcTCGen :: [String] -> Validation [String] TCGen
tcTCGen ["base"] = pure TCGenBase
tcTCGen ["lightmap"] = pure TCGenLightmap
tcTCGen ["environment"] = pure TCGenEnvironment
tcTCGen other = Failure ["Unknown tcgen: " ++ show other]


--------------------------------------------------------------------------------

data DepthFunc =
  Equal
  deriving (Show)

tcDepthFunc :: [String] -> Validation [String] DepthFunc
tcDepthFunc ["equal"] = pure Equal
tcDepthFunc args = Failure ["Unknown depthfunc args: " ++ show args]


--------------------------------------------------------------------------------

data AlphaFunc =
  Ge128 | Lt128 | Gt0
  deriving (Show)

tcAlphaFunc :: [String] -> Validation [String] AlphaFunc
tcAlphaFunc ["ge128"] = pure Ge128
tcAlphaFunc ["lt128"] = pure Lt128
tcAlphaFunc ["gt0"] = pure Gt0
tcAlphaFunc args = Failure ["Unknown alphafunc args: " ++ show args]


--------------------------------------------------------------------------------

data AlphaGen
  = AlphaGenLightingSpecular
  | AlphaGenWave WaveFunction
  | AlphaGenVertex
  deriving (Show)

tcAlphaGen :: [String] -> Validation [String] AlphaGen
tcAlphaGen ["lightingspecular"] = pure AlphaGenLightingSpecular
tcAlphaGen ("wave":args) = AlphaGenWave <$> tcWaveFunction args
tcAlphaGen ["vertex"] = pure AlphaGenVertex
tcAlphaGen args = Failure ["Unknown alphagen: " ++ show args]


--------------------------------------------------------------------------------

data AnimMap = AnimMap
  { animMapFrequency :: Double
  , animMapFrames :: [FilePath]
  } deriving (Show)

tcAnimMap :: [String] -> Validation [String] AnimMap
tcAnimMap (freq:frames) = AnimMap <$> tcDouble freq <*> pure frames
tcAnimMap args = Failure ["Unknown animmap: " ++ show args]


--------------------------------------------------------------------------------
data Pass = Pass
  { _rgbGen :: !(Last RGBGen)
  , _blendFunc :: !(Last (Factor, Factor))
  , _map :: !(Last Map)
  , _tCMod :: [TCMod]
  , _tCGen :: Last TCGen
  , _depthFunc :: Last DepthFunc
  , _depthWrite :: Any
  , _alphaFunc :: Last AlphaFunc
  , _clampMap :: Last FilePath
  , _alphaGen :: Last AlphaGen
  , _detail :: Any
  , _animMap :: Last AnimMap
  } deriving (Show)

initialPass :: Pass
initialPass =
  Pass
  { _rgbGen = mempty
  , _blendFunc = mempty
  , _map = mempty
  , _tCMod = mempty
  , _tCGen = mempty
  , _depthFunc = mempty
  , _depthWrite = mempty
  , _alphaFunc = mempty
  , _clampMap = mempty
  , _alphaGen = mempty
  , _detail = mempty
  , _animMap = mempty
  }

makeLenses ''Pass

tcPassOp :: Parser.Op -> (Endo Pass, [String])
tcPassOp (Parser.Op "rgbgen" args) =
  modifying rgbGen (pure <$> tcRgbGen args)
tcPassOp (Parser.Op "blendfunc" ["blend"]) =
  tcPassOp (Parser.Op "blendfunc" ["gl_src_alpha", "gl_one_minus_src_alpha"])
tcPassOp (Parser.Op "blendfunc" ["add"]) =
  tcPassOp (Parser.Op "blendfunc" ["gl_one", "gl_one"])
tcPassOp (Parser.Op "blendfunc" ["filter"]) =
  tcPassOp
    (Parser.Op "blendfunc" ["gl_dst_color", "gl_zero"])
tcPassOp (Parser.Op "blendfunc" [src, dst]) =
  modifying blendFunc (pure <$> liftA2 (,) (tcFactor src) (tcFactor dst))
tcPassOp (Parser.Op "map" [m]) = modifying map (pure <$> (tcMap m))
tcPassOp (Parser.Op "tcmod" args) = modifying tCMod (pure <$> tcTCMod args)
tcPassOp (Parser.Op "tcgen" args) = modifying tCGen (pure <$> tcTCGen args)
tcPassOp (Parser.Op "depthfunc" args) =
  modifying depthFunc (pure <$> tcDepthFunc args)
tcPassOp (Parser.Op "depthwrite" []) =
  modifying depthWrite (pure (Any True))
tcPassOp (Parser.Op "detail" []) =
  modifying detail (pure (Any True))
tcPassOp (Parser.Op "alphafunc" args) =
  modifying alphaFunc (pure <$> tcAlphaFunc args)
tcPassOp (Parser.Op "clampmap" [path]) =
  modifying clampMap (pure (pure path))
tcPassOp (Parser.Op "alphagen" args) =
  modifying alphaGen (pure <$> tcAlphaGen args)
tcPassOp (Parser.Op "animmap" args) =
  modifying animMap (pure <$> tcAnimMap args)
tcPassOp (Parser.Op op args) = (mempty, [show op ++ show args])


--------------------------------------------------------------------------------

tcInt :: String -> Validation [String] Int
tcInt s = maybe (Failure ["\"" ++ s ++ "\" is not an Int"]) pure (readMaybe s)




--------------------------------------------------------------------------------

data Cull
  = BackSided
  | TwoSided
  deriving (Show)

tcCull :: [String] -> Validation [String] Cull
tcCull ["none"] = pure TwoSided
tcCull ["twosided"] = pure TwoSided
tcCull ["disable"] = pure TwoSided

tcCull ["back"] = pure BackSided
tcCull ["backside"] = pure BackSided
tcCull ["backsided"] = pure BackSided

tcCull args = Failure ["Unknown cull args: " ++ show args]


--------------------------------------------------------------------------------
-- | A type-checked shader.
data Shader = Shader
  { _passes :: !(Seq Pass)
  , _cull :: !(Last Cull)
  , _deformVertexes :: !(Last VertexDeformation)
  , _sort :: Last SortLayer
  } deriving (Show, Generic)

makeLenses ''Shader

tcShader :: Parser.Shader -> (Shader, [String])
tcShader s =
  foldl
    (\(shader, es) instruction ->
       let (Endo f, es') = tcInstruction instruction
       in (f shader, es <> es'))
    ( Shader
      { _passes = mempty
      , _cull = mempty
      , _deformVertexes = mempty
      , _sort = mempty
      }
    , [])
    (Parser.shaderInstructions s)


--------------------------------------------------------------------------------
-- | Type check shader instructions to a transformation of a shader.
tcInstruction :: Parser.Instruction -> (Endo Shader, [String])
tcInstruction (Parser.ShaderOp (Parser.Op ('q':'3':'m':'a':'p':'_':_) _)) =
  mempty
tcInstruction (Parser.ShaderOp (Parser.Op ('q':'e':'r':'_':_) _)) =
  mempty
tcInstruction (Parser.ShaderOp (Parser.Op "nopicmip" _)) =
  mempty
tcInstruction (Parser.ShaderOp (Parser.Op "surfaceparm" _)) =
  mempty
tcInstruction (Parser.ShaderOp (Parser.Op "nomipmaps" _)) =
  mempty
tcInstruction (Parser.ShaderOp (Parser.Op "tesssize" _)) =
  mempty
tcInstruction (Parser.ShaderOp (Parser.Op "skyparms" _)) =
  mempty
tcInstruction (Parser.ShaderOp (Parser.Op "fogparms" _)) =
  mempty
tcInstruction (Parser.ShaderOp (Parser.Op "fogonly" _)) =
  mempty
tcInstruction (Parser.ShaderOp (Parser.Op "light" [_])) =
  -- Quake 3 source says:
  --   // light <value> determines flaring in q3map, not needed here
  mempty
tcInstruction (Parser.ShaderOp (Parser.Op "cull" args)) =
  modifying cull (pure <$> tcCull args)
tcInstruction (Parser.ShaderOp (Parser.Op "deformvertexes" args)) =
  modifying deformVertexes (pure <$> tcVertexDeformation args)
tcInstruction (Parser.ShaderOp (Parser.Op "sort" args)) =
  modifying sort (pure <$> tcSortLayer args)
tcInstruction (Parser.ShaderOp (Parser.Op op _)) =
  (mempty, ["Unknown shader operation: " ++ op])
tcInstruction (Parser.ShaderPass ops) =
  case foldl
         (\(pass, es) passOp ->
            let (Endo f, es') = tcPassOp passOp
            in (f pass, es <> es'))
         (initialPass, [])
         ops of
    (ps, es) -> (Endo (passes <>~ pure ps), es)
