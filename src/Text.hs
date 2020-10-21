{-# LANGUAGE OverloadedStrings #-}
module Text where


import qualified Data.FuzzySet as F
import qualified Data.Text as T
import qualified Data.Set as S
import Data.List

-- filterWords :: T.Text -> Set T.Text 
-- louzy way of finding important words
filterWords :: T.Text -> Int -> S.Set T.Text
filterWords xs n = S.fromList $ map (head.snd) $ take n $ wordRelativeFrequency 
    where words = filter (\x -> T.length x > 3) $ T.words xs 
          wordCount = fromIntegral $ length words
          wordRelativeFrequency = reverse . sortOn (fst) $ map (\x -> (fromIntegral (length x)/wordCount , x)) . group . sort $ words 


buildFuzzySet :: [T.Text] -> F.FuzzySet
buildFuzzySet xs = foldr step F.defaultSet xs 
    where step x z = z `F.add` x