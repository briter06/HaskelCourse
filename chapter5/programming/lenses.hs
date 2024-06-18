{-# LANGUAGE TemplateHaskell #-}

import Control.Lens (Lens', makeLenses, set, view)

newtype Grade = Grade {_gpa :: Float} deriving (Show)

data Person = Person {_name :: String, _age :: Int, _grade :: Grade} deriving (Show)

name :: Lens' Person String
name f (Person n a g) = fmap (\n' -> Person n' a g) (f n)

age :: Lens' Person Int
age f (Person n a g) = fmap (\a' -> Person n a' g) (f a)

grade :: Lens' Person Grade
grade f (Person n a g) = fmap (Person n a) (f g)

gpaGrade :: Lens' Grade Float
gpaGrade f (Grade g) = fmap Grade (f g)

gpa :: Lens' Person Float
gpa = grade . gpaGrade

-- makeLenses ''Person

person :: Person
person = Person "Briter" 23 (Grade 18.3)

-- type MyLens' s a = forall f. (Functor f) => (a -> f a) -> s -> f s

-- name :: MyLens' Person String
-- name elt_fn (Person n a) = fmap (`Person` a) (elt_fn n)