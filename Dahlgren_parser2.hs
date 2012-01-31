import Text.Regex.Posix
import System.Environment
                          
-- | 
-- 
-- Another rocking module to parse basic arithmatic (binary) operations   
--   string syntax : Lisp                                           
--   method : Recursive descent        
-- 
-- Erin Dahlgren :: January 15 2011
--  
-- |





-- | I have given each function a straightforward type signature and name. 
-- | I will let the type signatures speak for argument and return types.
-- | Function 'f' is the recursive descent procedure.

split :: [a] -> Int -> ([a], [a])
-- |
split xs 0 = ([], xs)
split (x:xs) n = let (f,l) = split xs (n-1) in (x:f,l)


splitpoint :: [Char] -> Int
-- |
splitpoint xs = findsplit 0 0 0 0 xs where
  findsplit 0 1 sp _ [] = sp
  findsplit l r sp n (x:xs)
    | l == r && l > 0 = n
    | x == ' ' = findsplit l r (n) (n+1) xs
    | x == '(' = findsplit (l+1) r sp (n+1) xs
    | x == ')' = findsplit l (r+1) sp (n+1) xs
    | otherwise = findsplit l r sp (n+1) xs


splitstream :: Int -> [Char] -> [[Char]]
-- |
splitstream 6 _ = []
splitstream n (x:xs)
  | elem n [0,1] = [x] : splitstream (n+1) xs
  | elem n [2,4] = splitstream (n+1) xs
  | n == 3 = let (s,r) = (splitpoint (x:xs), split (x:xs) s) in (fst r) : splitstream (n+1) (snd r)
  | n == 5 = let (s,r) = (length (x:xs)-1, split (x:xs) s) in (fst r) : (snd r) : splitstream (n+1) xs
                                                        
                                                              
verify :: String -> (String -> Bool)
-- |
verify regex = \x -> x =~ regex :: Bool

 
checkint = verify "^[1-9]*[0-9]|0"
plus = verify "^\\+"
minus = verify "^\\-"
mul = verify "^\\*"
space = verify " "


opera :: (Num a) => String -> a -> a -> a
-- |
opera xs 
  | plus xs = (+) 
  | minus xs = (-)
  | mul xs = (*)


leaf :: String -> Int
-- | 
leaf x = read x


f :: String -> Int
-- | 
f xs 
  | e1 && e2 = (op (leaf $ s!!2) (leaf $ s!!3))
  | e1 /= e2 && e1 = (op (leaf $ s!!2) (f $ s!!3))  
  | e1 /= e2 && e2 = (op (f $ s!!2) (leaf $ s!!3))
  | otherwise = (op (f $ s!!2) (f $ s!!3)) 
    where
      (s,op,e1,e2) = (splitstream 0 xs, opera (s !! 1), checkint (s !! 2), checkint (s !! 3))
                

run :: String -> Int
-- |  
run xs
  | space xs = f xs
  | otherwise = leaf xs





-- | Testing
main :: IO ()
-- |
main = do 
  let x = run "45" == 45
  let y = run "(+ 45 50)" == 95
  let z = run "(+ (- 8 3) 50)" == 55
  let a = run "(+ (- 8 3) (+ (- 7 33) 24))" == 3
  let b = run "(* 3 6)" == 18
  print $ (x && y && z && a && b)


