-- Type synonyms
type CustomType = String

-- Data type
data Person = Person String Integer

-- NewType
newtype NewInt = NewInt Int

-- Infix constructor
data InfixData a = Int :/ a

x :: InfixData NewInt
x = 3 :/ NewInt 6