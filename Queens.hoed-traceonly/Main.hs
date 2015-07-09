-- The queens problem made famous by Wirth.
import Debug.Hoed

type Board = [Int]

main :: IO ()
main = traceOnly $ observe "main" $
  if null solutions then putStrLn "no solution!"
  else putStr (showBoard (head solutions))
  where
  solutions = queens 8

queens :: Int -> [Board]
queens = observe "queens" $ \n -> valid n n 

valid :: Int -> Int -> [Board]
valid = observe "valid" $ \m n -> case m of
 0 -> [[]]
 m -> filter safe (extend n (valid (m-1) n)) 

extend :: Int -> [Board] -> [Board]
extend = observe "extend" $ \n bs -> consEach [1..n] bs 

consEach :: Observable a => [a] -> [[a]] -> [[a]]
consEach = observe "consEach" $ \xs y -> case xs of
 []    -> []
 (a:x) -> map (a:) y ++ consEach x y 

safe :: Board -> Bool
safe = observe "safe" $ \(a:b) -> no_threat a b 1

no_threat :: Int -> Board -> Int -> Bool
no_threat = observe "no_threat" $ \a bs m -> case bs of
 [] -> True
 (b:y) -> a /= b && a+m /= b && a-m /= b && no_threat a y (m+1) 

showBoard :: Board -> String 
showBoard = observe "showBoard" $ \b -> 
  let rank r qcol =
        map line ["o o o", " \\|/ ", " === "]
        where
        line crown_slice =
          concat (zipWith square [1..] b)
          where
          square scol _ =
            if scol == qcol then crown_slice
            else if scol `rem` (2::Int) == r `rem` (2::Int) then "....."
            else "     "
  in unlines (concat (zipWith rank [1..] b))
