module FSS where
import ForSyDe.Shallow
import Data.List.Split
import System.IO
-- Sine wave parameters example
sampleFreq = 8000
sineFreq = 440
sinePhase = 0
-- Spectrum process parameter
order = 8 :: Int
-- Discrete Fourrier Transform Calculation (answer in dB)
spec :: (RealFloat a, Enum a) => [a] -> [a]
spec x = zipWith (\re im -> 20 * logBase 10 (sqrt $ re^2 + im^2) - 10 * logBase 10 m)
          [sum $ zipWith (*) x2 [cos (2*pi*k*n/m) | n <- [0 .. (m-1)]] | k <- [0 .. (m-1)]]
          [sum $ zipWith (*) x2 [sin (2*pi*k*n/m) | n <- [0 .. (m-1)]] | k <- [0 .. (m-1)]]
  where m = fromIntegral $ length x
        x2 = zipWith (*) (cycle [1,-1]) x
-- Spectrum actor
spectrum :: (RealFloat a, Enum a) => Signal a -> Signal a
spectrum = actor11SDF (2^order) (2^order) spec
-- Adder actor
adder :: Num a => Signal a -> Signal a -> Signal a
adder = actor21SDF (1,1) 1 (\[x1] [x2] -> [x1+x2])
-- Adder in series with spectrum
freqSpec :: (RealFloat a, Enum a) => Signal a -> Signal a -> Signal a
freqSpec s1 s2 = spectrum $ adder s1 s2
-- Sine wave generator
sineGenerator :: RealFloat a => (a, a, a) -> [a] -> Signal a
sineGenerator (amp, freq, phase) time = signal $ map (\t -> amp * sin (2*pi*freq*t + phase)) time
-- Sine wave source actor
s_sine :: Signal Double
s_sine = sineGenerator (1, sineFreq, sinePhase) [0, 1/sampleFreq .. (2^order - 1)/sampleFreq]
-- Noise source actor
s_noise :: String -> Signal Double
s_noise noise_inp = signal $ map (\x -> read x :: Double) (splitOn "\n" noise_inp)

-- Format the output to save in a file
outputFormat :: (RealFloat a, Show a) => Signal a -> String
outputFormat NullS = []
outputFormat (x:-xs) = show x ++ "\n" ++ outputFormat xs

main = do
  noise <- readFile "NoiseSignal.txt"
  let s_out = freqSpec s_sine (s_noise noise)
  writeFile "OutputSignal.txt" $ outputFormat s_out
