import Control.Arrow ((&&&))
import Data.List

errorCorrection = snd . last . sort . map (length &&& head) . group . sort

errorCorrection2 = snd . head . sort . map (length &&& head) . group . sort

main = do input <- getContents
          print $ map errorCorrection $ transpose $ lines input
          print $ map errorCorrection2 $ transpose $ lines input