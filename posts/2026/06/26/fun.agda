-- Made up a weird relation, where each number is related to its double successor,
-- and also any even number is related to any odd number.
--
-- Claim: this relation is well-founded.

data Even : ℕ → Set where
  ZE : Even 0
  SSE : {n : ℕ} → Even n → Even (suc (suc n))

data Odd : ℕ → Set where
  OO : Odd 1
  SSO : {n : ℕ} → Odd n → Odd (suc (suc n))

suc-Odd : {n : ℕ} → Odd n → Even (suc n)
suc-Even : {n : ℕ} → Even n → Odd (suc n)

suc-Odd OO = SSE ZE
suc-Odd (SSO n) = SSE (suc-Odd n)

suc-Even ZE = OO
suc-Even (SSE n) = SSO (suc-Even n)

even? : (n : ℕ) → (Odd n ⊎ Even n)
even? zero = inj₂ ZE
even? (suc n) with even? n
... | inj₁ o = inj₂ (suc-Odd o)
... | inj₂ e = inj₁ (suc-Even e)

EO-disjoint : (n : ℕ) → Even n → Odd n → ⊥
EO-disjoint (suc zero) () _
EO-disjoint (suc (suc n)) (SSE e) (SSO o) = EO-disjoint n e o

data _ω2_ : Rel ℕ where
  SSω2 : {x : ℕ} → x ω2 (suc (suc x))
  Eω2O : {x y : ℕ} → Even x → Odd y → x ω2 y

ω2-acc : (m : ℕ) → Acc _ω2_ m
ω2-acc m with even? m
... | inj₁ o = acc {!!}
... | inj₂ e = acc (λ { zero SSω2 → acc {!!} ; (suc y) SSω2 → ω2-acc {!!} ; y (Eω2O x x₁) → absurd (EO-disjoint m e x₁) })

