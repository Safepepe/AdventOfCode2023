module NaturalNumberz where


data Natural = Minf | Natural Int | Pinf deriving(Show,Eq,Ord)


instance Num Natural where
 (+) :: Natural -> Natural -> Natural
 (Natural x) + (Natural y) = Natural (x+y)
 (Natural x) + Minf        = Minf
 (Natural x) + Pinf        = Pinf
 Minf + (Natural x)        = Minf
 Pinf + (Natural x)        = Pinf
 Minf + Minf               = Minf
 Pinf + Pinf               = Pinf
 Minf + Pinf               = error "can´t substract infinities"
 Pinf + Minf               = error "can´t substract infinities"
 (*) :: Natural -> Natural -> Natural
 (Natural x) * (Natural y) = Natural (x*y)
 (Natural x) * Minf        = if signum x == -1 then Pinf else if signum x == 1 then Minf else error "can´t multiply 0 times infinity"
 (Natural x) * Pinf        = if signum x == -1 then Minf else if signum x == 1 then Pinf else error "can´t multiply 0 times infinity"
 Minf * (Natural x)        = if signum x == -1 then Pinf else if signum x == 1 then Minf else error "can´t multiply 0 times infinity"
 Pinf * (Natural x)        = if signum x == -1 then Minf else if signum x == 1 then Pinf else error "can´t multiply 0 times infinity"
 Minf * Minf               = Pinf
 Pinf * Pinf               = Minf
 Minf * Pinf               = Minf
 Pinf * Minf               = Minf
 abs :: Natural -> Natural 
 abs Pinf = Pinf
 abs Minf = Pinf
 abs (Natural x) = Natural $ abs x
 signum :: Natural -> Natural
 signum Pinf = Natural 1
 signum Minf = Natural (-1)
 signum (Natural x) = Natural (signum x)
 fromInteger :: Integer -> Natural
 fromInteger x = Natural $ fromInteger x
 negate :: Natural -> Natural
 negate Pinf = Minf
 negate Minf = Pinf
 negate (Natural x) = Natural (negate x)

