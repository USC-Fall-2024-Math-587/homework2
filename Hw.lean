import USCMath587.Int.Basic

-- This is a helper structure to store the data of the extended GCD algorithm
structure GCDData (m n : Int) where
  gcd : Int
  coeff₁ : Int
  coeff₂ : Int

def extendedGCD (m n : Int) : GCDData m n :=
  -- We need to use the keyword `let` to define local variables and
  -- `rec` to let Lean know that we are defining a recursive function
  let rec go (u g x y: Int) : GCDData m n :=
    -- You can branch off propositions and name them for later use
    if h : y = 0 then
      sorry  -- fill in the solution when y = 0, see the initial values below
    else
      have : (g % y).natAbs < y.natAbs := Int.natAbs_emod_lt_natAbs g h
      -- See exercise 1.12 from the textbook for what to fill in `sorry`s with
      go x y sorry sorry
  -- By default, Lean wants total functions, so we need to prove that
  -- the recursive function will always terminate. We can do this by
  -- showing that the fourth argument decreases in each recursive call.
  -- Lean will use the `have` statement above to prove this with the correct
  -- solution.
  termination_by y.natAbs
  if m = 0 then
    sorry -- fill in the solution when m = 0
  else
    go 1 m 0 n -- we call the recursive function with these initial values

abbrev a (m n : Int) := extendedGCD m n |>.coeff₁

abbrev b (m n : Int) := extendedGCD m n |>.coeff₂

abbrev gcd (m n : Int) := extendedGCD m n |>.gcd

-- Also known as Bezout's theorem, this states we can write the GCD as a linear
-- combination of m and n. Fill in the `sorry` for a legit Lean cred!
theorem extendedGCD_def (m n : Int) : gcd m n = a m n * m + b m n * n := sorry
