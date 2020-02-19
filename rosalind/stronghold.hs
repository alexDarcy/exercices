import Data.List
-- exercises for the "Bioinformations stronghold"

countDNA :: String -> [Int]
countDNA =  map length . group . sort

transcribeDNA = map transcribe
  where
    transcribe 'T' = 'U'
    transcribe x = x
