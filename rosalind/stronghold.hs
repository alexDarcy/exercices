import Data.List
-- exercises for the "Bioinformations stronghold"

countDNA :: String -> [Int]
countDNA =  map length . group . sort

transcribeDNA = map transcribe
  where
    transcribe 'T' = 'U'
    transcribe x = x

complementDNA = map complement . reverse
  where
    complement 'T' = 'A'
    complement 'A' = 'T'
    complement 'C' = 'G'
    complement 'G' = 'C'
