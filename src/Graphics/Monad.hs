{-# LANGUAGE 
    TypeFamilies
  , RecordWildCards
  , FlexibleContexts
  , ScopedTypeVariables
  , AllowAmbiguousTypes
  , TypeOperators
  , DataKinds
  , TypeInType
  , FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving #-}

module Graphics.Monad 
  ( window
  , context
  , WindowSize(..)
  , Title
  , GraphicsT
  , execGraphicsT
  , Context(..)
  ) where

import Data.Kind
import Data.Proxy
import Graphics.UI.GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad.IO.Class
import Control.Monad.Base
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Exception.Lifted
import Control.Monad hiding (fail)
import Prelude hiding (fail)
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Applicative
import Foreign.Ptr

data WindowSize = Fullscreen | Windowed Int Int

type Title = String

newtype GraphicsT c m x
  = GraphicsT { runGraphicsT :: c -> Window -> m x }

window :: Applicative m => GraphicsT c m Window
window = GraphicsT $ \_ -> pure

context :: Applicative m => GraphicsT c m c
context = GraphicsT $ const . pure

pollEvents :: MonadBase IO m => GraphicsT c m ()
pollEvents = liftBase Graphics.UI.GLFW.pollEvents

instance Functor f => Functor (GraphicsT c f) where
  fmap f = GraphicsT . ((.) . (.)) (fmap f) . runGraphicsT

instance Applicative m => Applicative (GraphicsT c m) where
  GraphicsT f <*> GraphicsT x = GraphicsT $ \c w -> do
    f c w <*> x c w
  pure x = GraphicsT $ \_ _ -> pure x

instance Monad m => Monad (GraphicsT c m) where
  return = pure
  GraphicsT x >>= f = GraphicsT $ \c w -> do
    x <- x c w
    runGraphicsT (f x) c w

instance MonadPlus m => MonadPlus (GraphicsT c m) where
  mzero = lift mzero
  mplus x y = GraphicsT $ \c w ->
    mplus (runGraphicsT x c w) (runGraphicsT y c w)

instance Alternative m => Alternative (GraphicsT c m) where
  empty = GraphicsT $ \_ _ -> empty
  x <|> y = GraphicsT $ \c w -> 
    runGraphicsT x c w <|> runGraphicsT y c w

instance MonadFail m => MonadFail (GraphicsT c m) where
  fail = lift . fail

instance MonadIO m => MonadIO (GraphicsT c m) where
  liftIO io = GraphicsT $ \_ _ -> liftIO io

instance MonadBase IO m => MonadBase IO (GraphicsT c m) where
  liftBase io = GraphicsT $ \_ _ -> liftBase io 

instance MonadBaseControl IO m => MonadBaseControl IO (GraphicsT c m) where
  type StM (GraphicsT c m) a = ComposeSt (GraphicsT c) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadTransControl (GraphicsT c) where
   type StT (GraphicsT c) a = a
   liftWith f = GraphicsT $ \c w -> f $ \t -> runGraphicsT t c w 
   restoreT = GraphicsT . \x _ _ -> x

instance MonadTrans (GraphicsT c) where
  lift mx = GraphicsT $ \_ _ -> mx

instance MonadState s m => MonadState s (GraphicsT c m) where
  state f = lift (state f)
  
instance MonadReader r m => MonadReader r (GraphicsT c m) where
  ask = lift ask
  reader = lift . reader
  local f g = GraphicsT $ \c w -> local f $ runGraphicsT g c w

instance MonadFix m => MonadFix (GraphicsT c m) where
  mfix f = GraphicsT $ \c w -> do
    mfix $ \a -> runGraphicsT (f a) c w

instance MonadWriter w m => MonadWriter w (GraphicsT c m) where
  writer = lift . writer
  tell = lift . tell
  listen g = GraphicsT $ \c w -> do
    listen $ runGraphicsT g c w 
  pass g = GraphicsT $ \c w -> do
    pass $ runGraphicsT g c w

execGraphicsT :: (MonadBaseControl IO m, Context c) => WindowSize -> Title -> [WindowHint] -> c -> GraphicsT c m () -> m ()
execGraphicsT ws title hints c g = do
  initialized <- liftBase $ Graphics.UI.GLFW.init
  forM_ hints (liftBase . windowHint)
  if initialized
    then do
      mwin :: Maybe Window <- case ws of
        Fullscreen -> do 
          mmonitor <- liftBase $ getPrimaryMonitor
          case mmonitor of
            Just monitor -> do 
              mvideoMode <- liftBase $ getVideoMode monitor
              case mvideoMode of
                Just VideoMode{..} ->
                  liftBase $ createWindow videoModeWidth videoModeHeight title mmonitor Nothing
                Nothing -> do
                  liftBase $ terminate
                  error "Could not get video mode of primary monitor."
            Nothing -> do
              liftBase $ terminate
              error "Could not get primary monitor."
        Windowed w h -> liftBase $ createWindow w h title Nothing Nothing
      case mwin of
        Just win -> do
          flip finally (liftBase (destroyWindow win) >> liftBase terminate) $ do
            mapM_ liftBase 
              [ setErrorCallback $ errorCallback c,
                setMonitorCallback $ monitorCallback c,
                setWindowPosCallback win $ windowPosCallback c,
                setWindowSizeCallback win $ windowSizeCallback c,
                setWindowCloseCallback win $ windowCloseCallback c,
                setWindowRefreshCallback win $ windowRefreshCallback c,
                setWindowFocusCallback win $ windowFocusCallback c,
                setWindowIconifyCallback win $ windowIconifyCallback c,
                setFramebufferSizeCallback win $ framebufferSizeCallback c,
                setKeyCallback win $ keyCallback c,
                setCharCallback win $ charCallback c,
                setCharModsCallback win $ charModsCallback c,
                setMouseButtonCallback win $ mouseButtonCallback c,
                setCursorPosCallback win $ cursorPosCallback c,
                setCursorEnterCallback win $ cursorEnterCallback c,
                setScrollCallback win $ scrollCallback c,
                setDropCallback win $ dropCallback c,
                setJoystickCallback $ joystickCallback c,
                makeContextCurrent mwin ]  
            runGraphicsT g c win
        Nothing -> do
          liftBase $ terminate
          error "Could not create window."
  else error "Could not initialize GLFW."

class Context c where
  errorCallback :: c -> Maybe ErrorCallback
  errorCallback _ = Nothing
  monitorCallback :: c -> Maybe MonitorCallback
  monitorCallback _ = Nothing
  windowPosCallback :: c -> Maybe WindowPosCallback
  windowPosCallback _ = Nothing
  windowSizeCallback :: c -> Maybe WindowSizeCallback
  windowSizeCallback _ = Nothing
  windowCloseCallback :: c -> Maybe WindowCloseCallback
  windowCloseCallback _ = Nothing
  windowRefreshCallback :: c -> Maybe WindowRefreshCallback
  windowRefreshCallback _ = Nothing
  windowFocusCallback :: c -> Maybe WindowFocusCallback
  windowFocusCallback _ = Nothing
  windowIconifyCallback :: c -> Maybe WindowIconifyCallback
  windowIconifyCallback _ = Nothing
  framebufferSizeCallback :: c -> Maybe FramebufferSizeCallback
  framebufferSizeCallback _ = Nothing
  keyCallback :: c -> Maybe KeyCallback
  keyCallback _ = Nothing
  charCallback :: c -> Maybe CharCallback
  charCallback _ = Nothing
  charModsCallback :: c -> Maybe CharModsCallback
  charModsCallback _ = Nothing
  mouseButtonCallback :: c -> Maybe MouseButtonCallback
  mouseButtonCallback _ = Nothing
  cursorPosCallback :: c -> Maybe CursorPosCallback
  cursorPosCallback _ = Nothing
  cursorEnterCallback :: c -> Maybe CursorEnterCallback
  cursorEnterCallback _ = Nothing
  scrollCallback :: c -> Maybe ScrollCallback
  scrollCallback _ = Nothing
  dropCallback :: c -> Maybe DropCallback
  dropCallback _ = Nothing
  joystickCallback :: c -> Maybe JoystickCallback
  joystickCallback _ = Nothing