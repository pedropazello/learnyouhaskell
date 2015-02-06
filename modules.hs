import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

encode :: Int -> String -> String
encode shift msg = 
    let ords = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

phoneBook = 
    [("betty","555-2938")
    ,("bonnie", "452-2928")
    ,("patty", "666-6666")]

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"
