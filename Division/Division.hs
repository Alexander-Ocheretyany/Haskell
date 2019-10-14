type Poly = [Double]
divide::Poly -> Poly -> (Poly, Poly)
divide a b = divide' a b []

divide' a b res | length a >= length b,
                let r = step a b
                = divide' (snd r) b (res ++ (fst r))
                | otherwise = (res, a)  
          
step [] [] = ([], [])
step a b | length a >= length b,
         let dif = ((length a) - (length b)),
         let b' = augment b dif,
         let m = head a,
         let n = head b',
         let multiplier = m / n,
         let b'' = map (multiplier *) b',
         let a'= dif' a b'' [],
         let s = shorten a'
         = ([multiplier], s)

augment a n | n == 0 = a
            | otherwise = augment (a ++ [0.0]) (n - 1) 

dif' [] [] r = r
dif' a b r | length a == length b = dif' (tail a) (tail b) (r ++ [((head a) - (head b))])
           | otherwise = []

shorten a | a == [] = a
          | head a == 0 = shorten (tail a)
          | otherwise = a