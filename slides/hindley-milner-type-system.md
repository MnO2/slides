class: center, middle

# Hindley Milner Type System

---

# Why do we need Type system ?

* To check if the program run as we expected
* How far could it go?
* Halting problem
* A meta language
* Unit testing
* a type system, namely that: in it all terms are typed. 

---

* Could the meta language describe a structure, but not a set?
* Define structure?  Algebra?
* How about Subtyping? As some of OOPL do?
* It is so hard for such a design problem
* UX is dealing with the general user, PL is serving the need of programmers

---

# What's a Machine?

* Algorithm complexity: Abstract machine 
* Turing machine
* Real CPU by instruction

---

# Lambda Calculus

* Machine for substitution
* Assembly of functional language
* Various extensions of lambda calculus serves as the intermediate form of functional languages
* A single substitution step is called "beta reduction", or simply reduction

---

# They come with different power 

* Could it be possible I could roll my own type system?
* What if I add/remove rule to type system?
* What's normal life programming language's type system?
* The goal of type system

---
# Haskell's type system

* Hindley Milner
* System F
* System F`$\omega$`

---

# Hindley Milner Type Rules


`$$
    \begin{aligned}
    e :=\ & x            & \trule{Var} \\
         & \lambda x. e  & \trule{Lam} \\
         & e\ e          & \trule{App} \\
    \end{aligned}
$$`

* Often a construct called "let binding" is added
`$$ 𝚕𝚎𝚝 a=e 𝚒𝚗 b:=(λa.b)e $$`


---

* The complete set of type functions D is arbitrary in HM, except that it must contain at least `$$\rightarrow$$`
* Polytypes (or type schemes) are types containing variables bound by one or more for-all quantifiers
* The quantifiers can only appear top level. No `$$\forall \alpha -> \forall \alpha$$`

---

# Recursion

* Point to self
* Let rec


---

# Type System

``` haskell
data Exp = EVar EVar
         | ELit ELit
         | EApp Exp Exp
         | EAbs EVar Exp
         | ELet EVar Exp Exp
         deriving (Eq, Ord, Show)
```

---

# Substitutable

``` haskell
class Substitutable a where
  apply ∷ Subst → a → a
  freeTvars :: a -> Set.Set TVar
```

---

``` haskell
instance Substitutable Type where
  apply _ TInt = TInt
  apply _ TBool = TBool
  apply su t@(TVar a) = Map.findWithDefault t a su
  apply su (t1 `TArrow` t2) = apply su t1 `TArrow` apply su t2

  freeTvars TInt = Set.empty
  freeTvars TBool = Set.empty
  freeTvars (TVar a) = Set.singleton a
  freeTvars (t1 `TArrow` t2) = freeTvars t1 `Set.union` freeTvars t2
```


``` haskell
instance Substitutable a ⇒ Substitutable [a] where
  apply = map ∘ apply
  freeTvars = (foldr Set.union Set.empty) ∘ (map freeTvars)
```


``` haskell
instance Substitutable Scheme where
  apply su (Forall as t) = Forall as $ apply s' t
                            where s' = foldr Map.delete su as
  freeTvars (Forall as t) = (freeTvars t) `Set.difference` (Set.fromList as)
```


``` haskell
instance Substitutable TypeEnv where
  apply su (TypeEnv env) = TypeEnv $ Map.map (apply su) env
  freeTvars (TypeEnv env) = freeTvars $ Map.elems env
```

---

# Unification

``` haskell
mgu ∷ MonadError String m ⇒ Type → Type → m Subst
mgu (l `TArrow` r) (l' `TArrow` r') = do s1 ← mgu l l'
                                         s2 ← mgu (apply s1 r) (apply s1 r')
                                         return (s1 `after` s2)
mgu (TVar a) t = varAssign a t
mgu t (TVar a) = varAssign a t
mgu TInt TInt = return emptySubst
mgu TBool TBool = return emptySubst
mgu t1 t2 = throwError $ "types do no unify: " ⧺ (show t1) ⧺ " vs. " ⧺ (show t2)
```


---

# Inference (1)

``` haskell
ti ∷ (MonadState TIState m, MonadError String m) ⇒ TypeEnv → Exp → m (Subst, Type)
ti _ (ELit (LInt _)) = return (emptySubst, TInt)
ti _ (ELit (LBool _)) = return (emptySubst, TBool)
ti (TypeEnv env) (EVar x) =•
  case Map.lookup x env of
    Nothing → throwError $ "Unbound Variable: " ⧺ show x
    Just s → do•
      v ← instantiate s
      return (emptySubst, v)
ti env (EAbs x e) =•
  do tv ← freshTVar "a"
     let env' = env ∖ (x, Forall [] tv)
     (s1, t1) ← ti env' e
     return (s1, (apply s1 tv) `TArrow` t1)
``` 

---

# Inference (2)

``` haskell
ti env (EApp e1 e2) =
  do tv ← freshTVar "a"
     (s1, t1) ← ti env e1
     (s2, t2) ← ti (apply s1 env) e2
     s3 ← mgu (apply s2 t1) (TArrow t2 tv)
     return (s3 `after` s2 `after` s1, apply s3 tv)
```

* Name shadowing 
`$$λxy.(λxz.x+y)$$`

---

# Inference (3)

``` haskell
ti env (ELet x e1 e2) =
  do (s1, t1) ← ti env e1
     let env' = apply s1 env
         t' = generalize env' t1
     (s2, t2) ← ti (env' ∖ (x, t')) e2
     return (s1 `after` s2, t2)
```


---

# The peril of Let-polymorphism

``` haskell
lambda f : (forall A. A -> A). (f Int 1, f String "hello")
```

---

# Does there exist a type system for a non-let-polymorphic lambda calculus?

