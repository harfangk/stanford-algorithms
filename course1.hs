module Course1 (karatsuba) where

karatsuba :: Integer -> Integer -> Integer
karatsuba i j =
  let iDigits = length . show $ i
      jDigits = length . show $ j
  in
      if iDigits < 4 || jDigits < 4 then
        i * j
      else
        let
          m = minimum [quot iDigits 2, quot jDigits 2]
          (a, b) = quotRem i (10 ^ m)
          (c, d) = quotRem j (10 ^ m)
          z2 = karatsuba a c
          z0 = karatsuba b d
          z1 = karatsuba (a + b) (c + d) - z0 - z2
        in
          z2 * (10 ^ (2 * m)) + z1 * (10 ^ m) + z0
