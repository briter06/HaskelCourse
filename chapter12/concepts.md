# Equational reasoning

Proof that two functions have the same semantics

## Examples

## Scanl

### Definitiom

```haskell
inits :: [a] -> [[a]]
inits [] [[]]
inits (x:xs) = []:map (x:) (inits xs)
-- inits [x1, x2, x3] = [[], [x1], [x1,x2], [x1, x2, x3]]

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f e = map (foldl f e) . inits
```

### Theorem

```haskell
scanl f e [] = [e]
scanl f e (x:xs) = e:scanl f (f e x) xs
```

### Lemma

```haskell
foldl f e . (x:) = foldl f (f e x)
```

### Proof

#### Clause 1:

```haskell
scanl f e []
= map (foldl f e) (inits [])                            {definition scanl}
= map (foldl f e) [[]]                                  {definition inits}
= [foldl f e []]                                        {definition map}
= [e]                                                   {definition fold}
```

#### Clause 2:

```haskell
scanl f e (x:xs)
= map (foldl f e) (inits (x:xs))                        {definition scanl}
= map (foldl f e) ([]:map (x:) (inits xs))              {definition inits}
= foldl f e [] : map (foldl f e) (map (x:) (inits xs))  {definition map}
= e : map (foldl f e) (map (x:) (inits xs))             {definition fold}
= e : map (foldl f e) . (map (x:)) $ (inits xs)         {definition (.)}
= e : map (foldl f e . (x:)) (inits xs)                 {law map}
= e : map (foldl f (f e x)) (inits xs)                  {lemma}
= e : scanl f (f e x) xs                                {definition scanl}
```

## Scanr

### Definitiom

```haskell
tails [] = [[]]
tails (x:xs) = (x:xs):tails xs
-- tails [x1, x2, x3] = [[x1, x2, x3], [x2, x3], [x3], []]

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f e = map (foldr f e) . tails
```

### Theorem

```haskell
scanr f e [] = [e]
scanr f e (x:xs) = f x (head ys): ys
    where ys = scanr f e xs
```

### Lemma

```haskell
foldr f e . (x:) = f x . foldr f e
```

### Proof

#### Clause 1:

```haskell
scanr f e []
= map (foldr f e) (tails [])                            {definition scanr}
= map (foldr f e) [[]]                                  {definition tails}
= [foldr f e []]                                        {definition map}
= [e]                                                   {definition fold}
```

#### Clause 2:

```haskell
scanr f e (x:xs)
= map (foldr f e) (tails (x:xs))                                            {definition scanr}
= map (foldr f e) ((x:xs) : tails xs)                                       {definition tails}
= foldr f e (x:xs) : map (foldr f e) (tails xs)                             {definition map}
= (foldr f e . (x:)) xs : map (foldr f e) (tails xs)                        {definition (.)}
= (f x . foldr f e) xs : map (foldr f e) (tails xs)                         {lemma}
= f x (foldr f e xs) : map (foldr f e) (tails xs)                           {definition (.)}
= f x (foldr f e xs) : ys
    where ys = map (foldr f e) (tails xs)                                   {definition where}
= f x (foldr f e xs) : ys
    where ys = map (foldr f e) (xs : tails (tail xs))                       {definition tails}
= f x (foldr f e xs) : ys
    where ys = foldr f e xs : map (foldr f e) (tails (tail xs))             {definition map}
= f x (head ys) : ys
    where ys = foldr f e xs : map (foldr f e) (tails (tail xs))             {definition head}
= f x (head ys) : ys
    where ys = map (foldr f e) (xs : tails (tail xs))                       {definition map}
= f x (head ys) : ys
    where ys = map (foldr f e) (tails xs)                                   {definition tails}
= f x (head ys) : ys
    where ys = scanr f e xs                                                 {definition scanr}
```

# Full induction on Lists

To prove a property P(n), it should hold for:

- P(⊥) => For partial lists (infinite)
- P([]) => For finite lists
- ∀x,xs: P(xs) -> P(x:xs) => For any list

## Examples

## Concatenation

### Property

```haskell
(xs ++ ys) ++ zs = xs ++ (ys ++ zs)
xs ++ [] = [] ++ xs = xs
```

### Proof

#### LHS -> Holds for `P(⊥)`

```haskell
(⊥ ++ ys) ++ zs
= ⊥ ++ zs                       {case exhaustion}
= ⊥                             {case exahustion}
```

#### RHS -> Holds for `P(⊥)`

```haskell
⊥ ++ (ys ++ zs)
= ⊥                             {case exahustion}
```

#### LHS -> Holds for `P([])`

```haskell
([] ++ ys) ++ zs
= ys ++ zs                      {definition ++ 1}
```

#### RHS -> Holds for `P([])`

```haskell
[] ++ (ys ++ zs)
= ys ++ zs                      {definition ++ 1}
```

#### LHS -> Holds for `∀x,xs: P(xs) -> P(x:xs)`

```haskell
((x:xs) ++ ys) ++ zs
= (x:(xs ++ ys)) ++ zs          {definition ++ 2}
= x:((xs ++ ys) ++ zs)          {definition ++ 2}
= x:(xs ++ (ys ++ zs))          {induction hypothesis}
```

#### RHS -> Holds for `∀x,xs: P(xs) -> P(x:xs)`

```haskell
(x:xs) ++ (ys ++ zs)
= x:(xs ++ (ys ++ zs))          {definition ++}
```

## Reverse

### Property

For all finite lists
```haskell
reverse (reverse xs) = xs
```

### Definition

```haskell
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
```

### Lemma

```haskell
reverse (ys ++ [x]) = x:reverse ys
```

### Proof

#### Holds for `P([])`

```haskell
reverse (reverse [])
= reverse []                            {definition reverse 1}
= []                                    {definition reverse 1}
```

#### Holds for `∀x,xs: P(xs) -> P(x:xs)`

```haskell
reverse (reverse (x:xs))
= reverse (reverse xs ++ [x])           {definition reverse 2}
= x : reverse (reverse xs)              {lemma}
= x : xs                                {induction hypothesis}
```

## Concatenation flatten

### Property

```haskell
concat (xss ++ yss) = concat xss ++ concat yss
```

### Definition

```haskell
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss
```

### Proof

#### LHS -> Holds for `P(⊥)`

```haskell
concat (⊥ ++ yss)
= concat ⊥                              {case exahustion}
= ⊥                                     {case exahustion (pattern matching - strict on the first argument)}
```

#### RHS -> Holds for `P(⊥)`

```haskell
concat ⊥ ++ concat yss
= ⊥ ++ concat yss                       {case exahustion (pattern matching - strict on the first argument)}
= ⊥                                     {case exahustion}
```

#### LHS -> Holds for `P([])`

```haskell
concat ([] ++ yss)
= concat yss                            {definition ++}
```

#### RHS -> Holds for `P([])`

```haskell
concat [] ++ concat yss
= [] ++ concat yss                      {definition concat 1}
= concat yss                            {definition ++}
```

#### LHS -> Holds for `∀x,xs: P(xs) -> P(x:xs)`

```haskell
concat ((xs:xss) ++ yss)
= concat (xs:(xss ++ yss))              {definition ++}
= xs ++ concat (xss ++ yss)             {definition concat 2}
= xs ++ (concat xss ++ concat yss)      {induction hypothesis}
```

#### RHS -> Holds for `∀x,xs: P(xs) -> P(x:xs)`

```haskell
concat (xs:xss) ++ concat yss
= (xs ++ concat xss) ++ concat yss      {definition concat 2}
= xs ++ (concat xss ++ concat yss)      {definition ++}
```

## Length

### Property

```haskell
length (xs ++ ys) = length xs + length ys
```

### Definition

```haskell
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs
```

### Proof

#### LHS -> Holds for `P(⊥)`

```haskell
length (⊥ ++ ys)
= length ⊥                              {case exahustion}
= ⊥                                     {case exahustion}
```

#### RHS -> Holds for `P(⊥)`

```haskell
length ⊥ + length ys
= ⊥ + length ys                         {case exahustion}
= ⊥                                     {case exahustion}
```

#### LHS -> Holds for `P([])`

```haskell
length ([] ++ ys)
= length ys                             {definition ++}
```

#### RHS -> Holds for `P([])`

```haskell
length [] + length ys
= 0 + length ys                         {definition length 1}
= length ys                             {definition +}
```

#### LHS -> Holds for `∀x,xs: P(xs) -> P(x:xs)`

```haskell
length ((x:xs) ++ ys)
= length (x:(xs ++ ys))                 {definition ++}
= 1 + length (xs ++ ys)                 {definition length 1}
= 1 + (length xs + length ys)           {induction hypothesis}
```

#### RHS -> Holds for `∀x,xs: P(xs) -> P(x:xs)`

```haskell
length (x:xs) + length ys
= (1 + length xs) + length ys           {definition length 2}
= 1 + (length xs + length ys)           {definition +}
```

# Principles

* Extensionality: f = g if ∀x: f x = g x (uses previous process)
* Intensionality: Directly f = g (uses algebra of programs | we need pointfree style and laws)

For example, for `double x = x * 2`:

* Extensionality: `double = {(0,0), (1,2), (2,4), (3,6)}`
* Intensionality: `double = (*2)`

# Fusion Laws

1. If `f` is strict, `f a = b` and `∀x,y: f (g x y) = h x (f y)`. Then, `f . foldr g a = foldr h b`
2. If `f` is strict, `f a = b` and `∀x,y: f (g x y) = h (f x) y`. Then, `f . foldl g a = foldl h b`

## Instances

* Fold-map Fusion => `foldr f a . map g = foldr (f . g) a`
* Fold-concat Fusion => `foldr f a . concat = foldr (flip (foldr f)) a`
* Bookkeeping Law => `foldr f a . concat = foldr f a . map (foldr f a)`

# Summary

* Equational reasoning is a elegant way to prove properties of a program
* Can be used to establish a relation between a "correct" program and a "efficient" program
* Prioritize generic laws