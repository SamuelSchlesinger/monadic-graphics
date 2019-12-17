module Main where

import Graphics.Monad

data Temp = Temp 

instance Context Temp

main :: IO ()
main = execGraphicsT Fullscreen "Example" [] Temp (pure ()) where