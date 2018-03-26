import Control.Monad( liftM, liftM2 )
import Data.List( nub )
import Test.QuickCheck( quickCheck,
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )

--Problem 1

data Fruit = Apple(String, Bool)
| Orange(String, Int)
-- This allows us to print out Fruit in the same way we print out a list, an Int or a Bool.
deriving (Show)


--Apple("Granny Smith", False) -- a Granny Smith apple with no worm
--Apple("Braeburn", True) -- a Braeburn apple with a worm
--Orange("Sanguinello", 10) -- a Sanguinello orange with 10 segments

isBloodOrange :: Fruit -> Bool
isBloodOrange (Apple _) = False
isBloodOrange (Orange kind) | fst kind == "Tarocco" = True
			  	    | fst kind == "Moro" = True
                    | fst kind == "Sanguinello" = True
                    | otherwise = False
--Problem 2
--Write a function bloodOrangeSegments :: [Fruit] -> Int which returns the total number of blood orange segments in a list of fruit.


bloodOrangeSegments :: [Fruit] -> Int
bloodOrangeSegments list = sum [val | (Orange (kind,val)) <- list, kind == "Tarocco"
                                                                || kind == "Moro"
                                                                || kind == "Sanguinello"]

--Problem 3
--Write a function worms :: [Fruit] -> Int which returns the number of apples that contain worms.
worms :: [Fruit] -> Int
worms list = sum [1 | (Apple(_, worms)) <- list, worms == True]

--Problem 4
type Name = String
data Prop = Var Name
| F
| T
| Not Prop
| Prop :|: Prop
| Prop :&: Prop
deriving (Eq, Ord, Show)

--Problem 4
p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

-- This is used to test your function; don't touch it.
test1 :: () -> Prop
test1 () = p1

--Problem 5

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: ((Not(Var "P")) :&: (Not(Var "Q")))

test2 :: () -> Prop
test2 () = p2

--Problem 6
p3 :: Prop
p3 = ((Var "P" :&: (Var "Q" :|: Var "R")) :&: ((( Not (Var "P")) :|: ((Not (Var "Q")))) :&: ((Not (Var "P")) :|: (Not (Var "R")))))

test3 :: () -> Prop
test3 () = p3

--Problem 7
tautology :: Prop -> Bool
tautology p | length [True | x <- (envs (names p)), eval x p == True] == length (envs (names p)) = True
		 	| otherwise = False

--Problem 8a (either P is a tautology, or ~P is satisfiable)
prop_taut1 :: Prop -> Bool
prop_taut1 p = tautology p || satisfiable (Not p)

--Problem 8b (either P is not satisfiable, or ~P is not a tautology)
prop_taut2 :: Prop -> Bool
prop_taut2 p = not (satisfiable p) || not (tautology (Not p))

--Problem 9
-- Functions for handling Props

-- turns a Prop into a string approximating mathematical notation
showProp :: Prop -> String
showProp (Var x)        =  x
showProp (F)            =  "F"
showProp (T)            =  "T"
showProp (Not p)        =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)      =  "(" ++ showProp p ++ "|" ++ showProp q ++ ")"
showProp (p :&: q)      =  "(" ++ showProp p ++ "&" ++ showProp q ++ ")"
showProp (p :->: q)     =  "(" ++ showProp p ++ "->" ++ showProp q ++ ")"
showProp (p :<->: q)    =  "(" ++ showProp p ++ "<->" ++ showProp q ++ ")"

-- lookup a variable's value in an environment
lookUp :: Name -> Env -> Bool
lookUp n [] = error "lookup failure"
lookUp n ((s,v):rest) | n==s = v
lookUp n (_:rest) = lookUp n rest

-- evaluates a proposition in a given environment
eval :: Env -> Prop -> Bool
eval e (Var x)        =  lookUp x e
eval e (F)            =  False
eval e (T)            =  True
eval e (Not p)        =  not (eval e p)
eval e (p :|: q)      =  eval e p || eval e q
eval e (p :&: q)      =  eval e p && eval e q
eval e (p :->: q)	  | eval e p == True && eval e q == False = False
					  | otherwise = True
eval e (p :<->: q)	  | eval e p == eval e q = True
                      | otherwise = False

--Problem 10
names :: Prop -> Names
names (Var x)        =  [x]
names (F)            =  []
names (T)            =  []
names (Not p)        =  names p
names (p :|: q)      =  nub (names p ++ names q)
names (p :&: q)      =  nub (names p ++ names q)
names (p :->: q)	 =  nub (names p ++ names q)
names (p :<->: q)	 =  nub (names p ++ names q)

--Problem 11
p4 :: Prop
p4 = ((Var "P" :->: Var "Q") :&: (Var "P" :<->: Var "Q"))

p5 :: Prop
p5 = ((Var "P" :->: Var "Q") :&: (Var "P" :&: (Not (Var "Q"))))

p6 :: Prop
p6 = (Var "P" :<->: Var "Q") :&: ((Var "P" :&: (Not (Var "Q"))) :|: ((Not (Var "P")) :&: Var "Q"))


--Problem 12
--Equivalent - Returns true if bothprops are equal
equivalent :: Prop -> Prop -> Bool
equivalent p q = and [(eval x p) == (eval x q) | x <- (envs (names p ++ names q))]

--Problem 13 (Hard)
equivalent2 :: Prop -> Prop -> Bool
equivalent2 p q  =  tautology (p :<->: q)

--Problem 14 (Hard)
subformulas :: Prop -> [Prop]
subformulas (Not p)      = Not p : subformulas p
subformulas (p :|: q)    = (p :|: q)   : nub (subformulas p ++ subformulas q)
subformulas (p :&: q)    = (p :&: q)   : nub (subformulas p ++ subformulas q)
subformulas (p :->: q)   = (p :->: q)  : nub (subformulas p ++ subformulas q)
subformulas (p :<->: q)  = (p :<->: q) : nub (subformulas p ++ subformulas q)
subformulas p            = [p]																	AWQZ≈≈≈¡
