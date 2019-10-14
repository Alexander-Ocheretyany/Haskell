Haskell function

divide :: Poly -> Poly -> (Poly, Poly)

Sample query:

> divide [1, -12, 0, -42] [1, -2, 1]
([1.0,-10.0],[-21.0,-32.0])

Another sample query:

> divide [2, 4, 2] [1, 2, 1]
([2.0],[])