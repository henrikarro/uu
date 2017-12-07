import Data.Numbers.Primes (primes)

import Test.HUnit ((@?=))
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.HUnit (testCase)

#include "my_lazy.hs"

test_lazy :: (Integer, Integer, [Integer], Integer) -> Test
test_lazy (from, to, list, expected) =
  testCase (show from) $
    lazy from to list @?= expected

tests = [
    (1, 4, [1..], 7),
    (5, 26, [1..], 42),
    (1000, 2000, primes, 3681),
    (1, 10000000, primes, 36746404)
  ]

main = defaultMain $ map test_lazy tests
