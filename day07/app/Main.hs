module Main (main) where

import qualified Data.Map as M

-- Example Map Type: Map String (Map String Int)
type NestedMap = M.Map String (M.Map String Int)

-- Updates an existing key in the nested map

-- updateNestedValue :: String -> String -> Int -> NestedMap -> NestedMap
-- updateNestedValue outerKey innerKey newValue myMap =
--   M.adjust
--     (M.insert innerKey newValue)
--     outerKey
--     myMap

-- eta reduced
updateNestedValue :: String -> String -> Int -> NestedMap -> NestedMap
updateNestedValue outerKey innerKey newValue =
  M.adjust
    (M.insert innerKey newValue)
    outerKey

main :: IO ()
main = do
  let innerMap = M.fromList [("jan", 1), ("feb", 2)] :: M.Map String Int
  let outerMap = M.fromList [("mine", innerMap), ("yours", innerMap)]
  print outerMap
  let newmap = updateNestedValue "yours" "feb" 9 outerMap
  print newmap

-- adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
-- Update a value at a specific key with the result of the provided function.
--   When the key is not a member of the map, the original map is returned.

-- adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- adjust ("new " ++) 7 empty                         == empty
