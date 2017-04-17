{-# LANGUAGE BangPatterns, DataKinds,
  DeriveGeneric, FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving, KindSignatures, RecordWildCards,
  ScopedTypeVariables, TypeApplications, UndecidableInstances #-}

-- | A Quake 3 BSP parser with minimal marshalling.
module Parser where

import Control.Monad
import Data.Bits
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Unsafe as BS
import Data.Int
import Data.Proxy
import qualified Data.Vector as V
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Data.Word
import Foreign.CStorable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import GHC.TypeLits
import Linear
import System.IO
import Unsafe.Coerce

type IntLE = LittleEndian Int32
type FloatLE = LittleEndian Float

newtype LittleEndian a = LittleEndian { getLittleEndian :: a }
  deriving (Show, Eq, Ord, Read, Num, Fractional, Real, RealFrac, Bits, Integral, Enum)

instance Functor LittleEndian where
  fmap f (LittleEndian a) = LittleEndian (f a)

instance Storable (LittleEndian Word32) where
  sizeOf ~(LittleEndian le) = sizeOf le
  alignment ~(LittleEndian le) = alignment le
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 1
    c <- peekByteOff ptr 2
    d <- peekByteOff ptr 3
    return $ LittleEndian $
      (a `shiftL` 24) .|.
      (b `shiftL` 16) .|.
      (c `shiftL` 8) .|.
      d

instance Storable (LittleEndian Int32) where
  sizeOf ~(LittleEndian le) = sizeOf le
  alignment ~(LittleEndian le) = alignment le
  peek ptr = fmap (fromIntegral @Word32) (peek (castPtr ptr))

instance Storable (LittleEndian Float) where
  sizeOf ~(LittleEndian le) = sizeOf le
  alignment ~(LittleEndian le) = alignment le
  peek ptr = fmap (unsafeCoerce @Word32) (peek (castPtr ptr))

instance Storable (LittleEndian a) => CStorable (LittleEndian a) where
  cPoke ptr a = cPoke (castPtr ptr) (Storable a)
  cPeek ptr = fmap (\(Storable a) -> a) (cPeek (castPtr ptr))
  cAlignment a = cAlignment (Storable a)
  cSizeOf a = cSizeOf (Storable a)

newtype ASCII (n :: Nat) = ASCII { getASCII :: C8.ByteString }
  deriving (Eq, Ord, Show)

instance KnownNat n => CStorable (ASCII n) where
  cPoke todo = undefined
  cPeek ptr = ASCII <$> C8.packCString (castPtr ptr)
  cAlignment _ = 1
  cSizeOf _ = fromInteger (natVal (Proxy @n))

instance (Storable a, CStorable a) => CStorable (V2 a) where
  cPoke ptr a = cPoke (castPtr ptr) (Storable a)
  cPeek ptr = fmap (\(Storable a) -> a) (cPeek (castPtr ptr))
  cAlignment a = cAlignment (Storable a)
  cSizeOf a = cSizeOf (Storable a)

instance (Storable a, CStorable a) => CStorable (V3 a) where
  cPoke ptr a = cPoke (castPtr ptr) (Storable a)
  cPeek ptr = fmap (\(Storable a) -> a) (cPeek (castPtr ptr))
  cAlignment a = cAlignment (Storable a)
  cSizeOf a = cSizeOf (Storable a)

instance (Storable a, CStorable a) => CStorable (V4 a) where
  cPoke ptr a = cPoke (castPtr ptr) (Storable a)
  cPeek ptr = fmap (\(Storable a) -> a) (cPeek (castPtr ptr))
  cAlignment a = cAlignment (Storable a)
  cSizeOf a = cSizeOf (Storable a)


-- | 'DirEntry' indicates the length and offset of lumps in a BSP file.

data DirEntry = DirEntry
  { deOffset :: {-# UNPACK #-} !IntLE
  , deLength :: {-# UNPACK #-} !IntLE
  } deriving (Generic, Show)

instance CStorable DirEntry

instance Storable DirEntry where
  peek = cPeek
  poke = cPoke
  alignment = cAlignment
  sizeOf = cSizeOf


-- | Texture lumps

data Texture = Texture
  { textureName :: {-# UNPACK #-} !(ASCII 64)
  , textureFlags :: {-# UNPACK #-}!IntLE
  , textureContents :: {-# UNPACK #-}!IntLE
  } deriving (Eq, Generic, Ord, Show)

instance CStorable Texture

instance Storable Texture where
  peek = cPeek
  poke = cPoke
  alignment = cAlignment
  sizeOf = cSizeOf


-- | Planes lump

data Plane = Plane
  { planeNormal :: V3 FloatLE
  , planeDist :: FloatLE
  } deriving (Generic, Show)

instance CStorable Plane

instance Storable Plane where
  peek = cPeek
  poke = cPoke
  alignment = cAlignment
  sizeOf = cSizeOf


-- | Vertices

data Vertex = Vertex
  { vPosition :: {-# UNPACK #-}!(V3 FloatLE)
  , vSurfaceUV :: {-# UNPACK #-}!(V2 FloatLE)
  , vLightMapUV :: {-# UNPACK #-}!(V2 FloatLE)
  , vNormal :: {-# UNPACK #-}!(V3 FloatLE)
  , vColor :: {-# UNPACK #-}!(V4 Word8)
  } deriving (Generic, Show)

instance CStorable Vertex

instance Storable Vertex where
  peek = cPeek
  poke = cPoke
  alignment = cAlignment
  sizeOf = cSizeOf


-- | Mesh vertices

newtype MeshVert = MeshVert IntLE deriving (Show, Storable)


-- | Leafs

data Leaf = Leaf
  { leafCluster :: {-# UNPACK #-}!(LittleEndian Int32)
  , leafArea :: {-# UNPACK #-}!IntLE
  , leafMins :: {-# UNPACK #-}!(V3 IntLE)
  , leafMaxs :: {-# UNPACK #-}!(V3 IntLE)
  , leafFirstLeafFace :: {-# UNPACK #-}!IntLE
  , leafNLeafFaces :: {-# UNPACK #-}!IntLE
  , leafFirstLeafBrush :: {-# UNPACK #-}!IntLE
  , leafNLeafBrushes :: {-# UNPACK #-}!IntLE
  } deriving (Generic, Show)

instance CStorable Leaf

instance Storable Leaf where
  peek = cPeek
  poke = cPoke
  alignment = cAlignment
  sizeOf = cSizeOf


-- | Leaf Faces

newtype LeafFace = LeafFace IntLE deriving (Show, Storable)


-- | Faces

data Face = Face
  { faceTexture :: {-# UNPACK #-}!IntLE
  , faceEffect :: {-# UNPACK #-}!IntLE
  , faceType :: {-# UNPACK #-}!IntLE
  , faceVertex :: {-# UNPACK #-}!IntLE
  , faceNVertexes :: {-# UNPACK #-}!IntLE
  , faceMeshVert :: {-# UNPACK #-}!IntLE
  , faceNMeshVerts :: {-# UNPACK #-}!IntLE
  , faceLMIndex :: {-# UNPACK #-}!IntLE
  , faceLMStart :: {-# UNPACK #-}!(V2 IntLE)
  , faceLMSize :: {-# UNPACK #-}!(V2 IntLE)
  , faceLMOrigin :: {-# UNPACK #-}!(V3 FloatLE)
  , faceLMS :: {-# UNPACK #-}!(V3 Float)
  , faceLMT :: {-# UNPACK #-}!(V3 FloatLE)
  , faceNormal :: {-# UNPACK #-}!(V3 FloatLE)
  , faceSize :: {-# UNPACK #-}!(V2 IntLE)
  } deriving (Generic, Show)

instance CStorable Face

instance Storable Face where
  peek = cPeek
  poke = cPoke
  alignment = cAlignment
  sizeOf = cSizeOf

-- | Light maps

newtype LightMap = LightMap { lmData :: Vector Word8 }
  deriving (Show)

instance Storable LightMap where
  sizeOf _ = 128 * 128 * 3
  peek ptr = do
    fp <- mallocForeignPtrArray (128 * 128 * 3)
    withForeignPtr fp $ \newPtr ->
      copyArray newPtr (castPtr ptr) (128 * 128 * 3)
    pure (LightMap (Vector.unsafeFromForeignPtr0 fp (128 * 128 * 3)))


-- | Models

data Model = Model
  { modelMins :: {-# UNPACK #-}!(V3 FloatLE)
  , modelMaxs :: {-# UNPACK #-}!(V3 Float)
  , modelFirstFace :: {-# UNPACK #-}!IntLE
  , modelNFaces :: {-# UNPACK #-}!IntLE
  , modelFirstBrush :: {-# UNPACK #-}!IntLE
  , modelNBrushes :: {-# UNPACK #-}!IntLE
  } deriving (Generic, Show)

instance CStorable Model

instance Storable Model where
  peek = cPeek
  poke = cPoke
  alignment = cAlignment
  sizeOf = cSizeOf


-- | Nodes

data Node = Node
  { nodePlane :: {-# UNPACK #-} !IntLE
  , nodeFront :: {-# UNPACK #-} !IntLE
  , nodeBack :: {-# UNPACK #-} !IntLE
  , nodeMin :: {-# UNPACK #-} !(V3 IntLE)
  , nodeMax :: {-# UNPACK #-} !(V3 IntLE)
  } deriving (Generic, Show)

instance CStorable Node

instance Storable Node where
  peek = cPeek
  poke = cPoke
  alignment = cAlignment
  sizeOf = cSizeOf


-- | Vis data

data VisData = VisData
  { visDataNVecs :: {-# UNPACK #-}!IntLE
  , visDataSzVecs :: {-# UNPACK #-}!IntLE
  , visDataVecs :: {-# UNPACK #-}!(Vector Word8)
  } deriving (Show)


-- | A parsed BSP file.

data BSPFile = BSPFile
  { bspEntities :: {-# UNPACK #-}!C8.ByteString
  , bspTextures :: {-# UNPACK #-}!(V.Vector Texture)
  , bspPlanes :: {-# UNPACK #-} !(Vector Plane)
  , bspVertexes :: {-# UNPACK #-} !(Vector Vertex)
  , bspMeshVerts :: {-# UNPACK #-} !(Vector MeshVert)
  , bspLeafs :: {-# UNPACK #-} !(V.Vector Leaf)
  , bspLeafFaces :: {-# UNPACK #-} !(Vector LeafFace)
  , bspFaces :: {-# UNPACK #-} !(V.Vector Face)
  , bspLightMaps :: {-# UNPACK #-} !(Vector LightMap)
  , bspModels :: {-# UNPACK #-} !(Vector Model)
  , bspNodes :: {-# UNPACK #-} !(Vector Node)
  , bspVisData :: {-# UNPACK #-} !VisData
  } deriving (Show)


loadBSP :: FilePath -> IO BSPFile
loadBSP bspPath =
  withBinaryFile bspPath ReadMode $ \bspHandle -> do
    dirEntries <-
      do allocaBytes 8 $ \magicBytes -> do
           _ <- hGetBuf bspHandle magicBytes 8
           headerMagic <- BS.unsafePackCStringLen (castPtr magicBytes, 4)
           when
             (headerMagic /= C8.pack "IBSP")
             (error $ "Unexpected magic: " ++ C8.unpack headerMagic)
         do deBytes <- mallocForeignPtrArray 17
            _ <-
              withForeignPtr deBytes $ \buffer ->
                hGetBuf bspHandle buffer (sizeOf (undefined :: DirEntry) * 17)
            pure (Vector.unsafeFromForeignPtr0 deBytes 17)
    bspEntities <-
      do let DirEntry {..} = dirEntries Vector.! 0
         entitiesData <- mallocBytes (fromIntegral deLength)
         hSeek bspHandle AbsoluteSeek (fromIntegral deOffset)
         hGetBuf bspHandle entitiesData (fromIntegral deLength)
         BS.unsafePackMallocCString entitiesData
    bspTextures <-
      V.convert <$> parseLumpArray bspHandle (dirEntries Vector.! 1)
    bspPlanes <- parseLumpArray bspHandle (dirEntries Vector.! 2)
    bspNodes <- parseLumpArray bspHandle (dirEntries Vector.! 3)
    bspLeafs <- V.convert <$> parseLumpArray bspHandle (dirEntries Vector.! 4)
    bspLeafFaces <- parseLumpArray bspHandle (dirEntries Vector.! 5)
    -- bspLeafBrushes <- parseLumpArray bspHandle (dirEntries Vector.! 6)
    bspModels <- parseLumpArray bspHandle (dirEntries Vector.! 7)
    -- bspBrushes <- parseLumpArray bspHandle (dirEntries Vector.! 8)
    -- bspBrushSides <- parseLumpArray bspHandle (dirEntries Vector.! 9)
    bspVertexes <- parseLumpArray bspHandle (dirEntries Vector.! 10)
    bspMeshVerts <- parseLumpArray bspHandle (dirEntries Vector.! 11)
    -- bspEffects <- parseLumpArray bspHandle (dirEntries Vector.! 12)
    bspFaces <- V.convert <$> parseLumpArray bspHandle (dirEntries Vector.! 13)
    bspLightMaps <- parseLumpArray bspHandle (dirEntries Vector.! 14)
    bspVisData <-
      do let DirEntry {..} = dirEntries Vector.! 16
         hSeek bspHandle AbsoluteSeek (fromIntegral deOffset)
         visDataNVecs <-
           allocaBytes 4 $ \ptr -> do
             hGetBuf bspHandle ptr 4
             peek ptr
         visDataSzVecs <-
           allocaBytes 4 $ \ptr -> do
             hGetBuf bspHandle ptr 4
             peek ptr
         foreignVisData <-
           mallocForeignPtrBytes (fromIntegral (visDataNVecs * visDataSzVecs))
         withForeignPtr foreignVisData $ \ptr ->
           hGetBuf bspHandle ptr (fromIntegral deLength - 4 - 4)
         let visDataVecs =
               Vector.unsafeFromForeignPtr0
                 foreignVisData
                 (fromIntegral (visDataNVecs * visDataSzVecs))
         return VisData {..}
    return BSPFile {..}

parseLumpArray :: forall a. Storable a => Handle -> DirEntry -> IO (Vector a)
parseLumpArray bspHandle DirEntry {..} = do
  let entries = fromIntegral deLength `div` sizeOf (undefined :: a)
  foreignBuffer <- mallocForeignPtrBytes (fromIntegral deLength)
  _ <- withForeignPtr foreignBuffer $ \buffer -> do
    hSeek bspHandle AbsoluteSeek (fromIntegral deOffset)
    hGetBuf bspHandle buffer (fromIntegral deLength)
  pure (Vector.unsafeFromForeignPtr0 foreignBuffer entries)
