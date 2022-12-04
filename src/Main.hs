{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Conduit (
  sinkNull,
  yieldMany,
  (.|),
 )
import Data.Conduit.Process (sourceProcessWithStreams)
import Data.Conduit.Process.Typed (ExitCode)
import Data.Int (Int16)
import System.Process (proc)
import System.Process.Internals

import Data.Conduit (ConduitT)
import Data.Conduit.Serialization.Binary (conduitEncode)

main :: IO ()
main = do
  let note' = Note C 4
  (exitCode, _, _) <- stream note'
  print exitCode

stream :: Note -> IO (ExitCode, (), ())
stream note =
  sourceProcessWithStreams
    playProcess
    (signal note .| conduitEncode)
    sinkNull
    sinkNull

playProcess :: CreateProcess
playProcess = command{std_in = CreatePipe}
 where
  command = proc "play" ["-t", "raw", "-B", "-e", "signed-integer", "-b", "16", "-c", "1", "-r", "44100", "-"]

signal :: Monad m => Note -> ConduitT i Int16 m ()
signal note = yieldMany (oscillator (toFreq note))

oscillator :: Int -> [Int16]
oscillator freq =
  cycle . take (sampleRate `div` freq) $ -- Grab one period of the wave
    sine freq sampleRate <$> [0 ..]
 where
  sampleRate = 44100

sine :: Int -> Int -> Int -> Int16
sine freq sampleRate time = round $ amplitude * sin (fromIntegral freq * (2 * pi) * fromIntegral time / fromIntegral sampleRate)
 where
  amplitude :: Double
  amplitude = 32767

toFreq :: Note -> Int
toFreq note' = round $ 440 * (a ** fromIntegral (halfStepsFromA4 note'))
 where
  a :: Double
  a = 2 ** (1 / 12)

halfStepsFromA4 :: Note -> Int
halfStepsFromA4 (Note n o) =
  ((o - a4Octave) * 12) + (fromEnum n - a4Enum)
 where
  a4Enum = 9
  a4Octave = 4

data Note = Note
  { pitchClass :: PitchClass
  , octave :: Int
  }
  deriving (Show, Eq)

data PitchClass
  = C
  | Cx
  | D
  | Dx
  | E
  | F
  | Fx
  | G
  | Gx
  | A
  | Ax
  | B
  deriving (Show, Eq, Enum, Read)
