{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables, AllowAmbiguousTypes, TypeOperators, DataKinds, TypeInType, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Graphics.Monad.Render 
  ( vertex
  , vertexv
  , color
  , colorv
  , secondaryColor
  , secondaryColorv
  , index
  , indexv 
  , normal
  , normalv
  , texCoord
  , texCoordv
  , multiTexCoord
  , multiTexCoordv
  , fogCoord
  , fogCoordv
  , evalPoint1 
  , evalPoint2
  , evalCoord1
  , evalCoord1v
  , evalCoord2
  , evalCoord2v
  , materialAmbient
  , materialEmission
  , materialShininess
  , callList
  , callLists
  ) where 

import Data.StateVar
import Graphics.Rendering.OpenGL 
  ( materialAmbient
  , materialDiffuse
  , materialAmbientAndDiffuse 
  , materialSpecular 
  , materialEmission
  , materialShininess 
  , edgeFlag )
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Monad
import Foreign.Ptr
import Control.Monad
import Control.Monad.Base
import Control.Monad.Fail
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Fix

newtype Primitive a = Primitive (IO a)
  deriving 
    ( Monad, Functor, MonadFix
    , MonadFail, Applicative, MonadPlus
    , Alternative, MonadIO, Semigroup
    , Monoid, MonadBase IO)

renderPrimitive :: MonadBase IO m => GL.PrimitiveMode -> Primitive a -> GraphicsT c m a 
renderPrimitive mode (Primitive io) = do
  liftBase $ GL.renderPrimitive mode io

callList :: GL.DisplayList -> Primitive ()
callList = Primitive . GL.callList

callLists :: GL.GLsizei -> GL.DataType -> Ptr a -> Primitive ()
callLists s d = Primitive . GL.callLists s d

vertex :: GL.Vertex a => a -> Primitive ()
vertex = Primitive . GL.vertex

vertexv :: GL.Vertex a => Ptr a -> Primitive ()
vertexv = Primitive . GL.vertexv

color :: GL.Color a => a -> Primitive ()
color = Primitive . GL.color

colorv :: GL.Color a => Ptr a -> Primitive ()
colorv = Primitive . GL.colorv

secondaryColor :: GL.SecondaryColor a => a -> Primitive ()
secondaryColor = Primitive . GL.secondaryColor

secondaryColorv :: GL.SecondaryColor a => Ptr a -> Primitive ()
secondaryColorv = Primitive . GL.secondaryColorv

texCoord :: GL.TexCoord a => a -> Primitive ()
texCoord = Primitive . GL.texCoord

texCoordv :: GL.TexCoord a => Ptr a -> Primitive ()
texCoordv = Primitive . GL.texCoordv

multiTexCoord :: GL.TexCoord a => GL.TextureUnit -> a -> Primitive ()
multiTexCoord u = Primitive . GL.multiTexCoord u

multiTexCoordv :: GL.TexCoord a => GL.TextureUnit -> Ptr a -> Primitive () 
multiTexCoordv u = Primitive . GL.multiTexCoordv u

index :: GL.Index a => a -> Primitive ()
index = Primitive . GL.index

indexv :: GL.Index a => Ptr a -> Primitive ()
indexv = Primitive . GL.indexv

normal :: GL.Normal a => a -> Primitive ()
normal = Primitive . GL.normal

normalv :: GL.Normal a => Ptr a -> Primitive ()
normalv = Primitive . GL.normalv

fogCoord :: GL.FogCoord a => a -> Primitive ()
fogCoord = Primitive . GL.fogCoord 

fogCoordv :: GL.FogCoord a => Ptr a -> Primitive ()
fogCoordv = Primitive . GL.fogCoordv

evalPoint1 :: GL.GLint -> Primitive ()
evalPoint1 = Primitive . GL.evalPoint1

evalPoint2 :: (GL.GLint, GL.GLint) -> Primitive ()
evalPoint2 = Primitive . GL.evalPoint2

evalCoord1 :: GL.Domain d => d -> Primitive ()
evalCoord1 = Primitive . GL.evalCoord1 

evalCoord1v :: GL.Domain d => Ptr d -> Primitive ()
evalCoord1v = Primitive . GL.evalCoord1v

evalCoord2 :: GL.Domain d => (d, d) -> Primitive ()
evalCoord2 = Primitive . GL.evalCoord2

evalCoord2v :: GL.Domain d => Ptr d -> Primitive ()
evalCoord2v = Primitive . GL.evalCoord2v