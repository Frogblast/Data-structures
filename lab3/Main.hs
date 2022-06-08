{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}

import AATree

--------------------------------------------------------------------------------

--main :: IO ()
main = do
  contents <- getContents

  let splittedWords = splitWords contents
  let theTree = foldl (flip insert) emptyTree splittedWords

  let optimalHeight = calculateOptimalHeight (size theTree)
  let actualHeight = height theTree

  let optimalHeightRatio = fromIntegral actualHeight / fromIntegral optimalHeight


  -- calculate and print statistics
  putStrLn ("Size: " ++ show (size theTree))
  putStrLn ("Height: " ++ show (height theTree))
  putStrLn ("Optimal height: " ++ show optimalHeight)
  putStrLn ("Height / Optimal height: " ++ show optimalHeightRatio)
  putStrLn ("checkTree: " ++ show (checkTree theTree))
  putStrLn ("First 20 words: " ++ show (take 20 (inorder theTree)))

-- O(n)
splitWords :: String -> [String]
splitWords = words

-- O(n), because of size function
calculateOptimalHeight :: Int -> Int
calculateOptimalHeight size = ceiling (logBase 2 (fromIntegral (size+1)) - 1)
--------------------------------------------------------------------------------