--Brad Tully, Programming Assignment 3, 22 April 2018

module A3b where

  import Data.List
  import Data.Char
  import Data.Maybe

  {-
  onlyLowercase takes a list of strings and returns the list with the strings that begin with a lowercase letter
  param- (x:xs) a list of strings
  -}
  onlyLowercase [] = []
  onlyLowercase (x:xs) = filter (\z -> let (y:ys) = z in if ((isLower y) == True) then True else False) (x:xs)

  {-
  longestString takes a list of strings and returns the longest one, "" if empty, if tie the first one
  param- (x:xs) a list of strings
  -}
  longestString [] = ""
  longestString (x:xs) = foldl (\y z -> if (length y >= length z) then y else z) (x) (xs)

  {-
  longestString' takes a list of strings and returns the longest one, "" if empty, if tie the last one
  param- (x:xs) a list of strings
  -}
  longestString' [] = ""
  longestString' (x:xs) = foldl (\y z -> if (length y > length z) then y else z) (x) (xs)

  {-
  longestStringHelper takes a function and a list, and finds the longest string in the matter of the function that is passed in
  param- fun is a function that is used to find the longest string
  param- (x:xs) a list of strings
  -}
  longestStringHelper fun [] = []
  longestStringHelper fun (x:xs) = foldl (\y z -> if (fun (length y) (length z)) then y else z) (x) (xs)

  {-
  longestSTring3 takes a list and returns and returns the longest one, "" if empty, if tie the first one, it uses partial application of longestStringHelper
  param- (x:xs) a list of strings
  -}
  longestString3 [] = ""
  longestString3 (x:xs) = longestStringHelper (>=) (x:xs)

  {-
  longestString4 takes a list of strings and returns the longest one, "" if empty, if tie the last one, it uses partial application of longestStringHelper
  param- (x:xs) a list of strings
  -}
  longestString4 [] = ""
  longestString4 (x:xs) = longestStringHelper (>) (x:xs)

  {-
  longestLowercase takes a string list and finds the longest one that starts with a lowercase letter, "" if empty, first one in a tie
  this uses partian application and function compisition with longestString and onlyLowercase
  param- (x:xs) a list of strings
  -}
  longestLowercase [] = ""
  longestLowercase (x:xs) = (longestString . onlyLowercase) (x:xs)

  {-
  revStringRev takes a string and reverses it and makes all letters lowercase
  param- list is a string (char list)
  rev takes a list and converts it to lower case
  param- (x:xs) a string (char list)
  -}
  revStringRev [] = ""
  revStringRev list = (reverse . rev) list
    where rev [] = ""
          rev (x:xs) = case isLower (x) of
                        True -> [x] ++ (rev xs)
                        False -> [(toLower x)] ++ (rev xs)

  {-
  firstAnswer takes a function and list and applies the function to the list returning Nothing if it all returns Nothing
  otherwise it returns Just x value
  param- fun a function that is applied to the List
  param- (x:xs) a list
  -}
  firstAnswer :: ( a -> Maybe b ) -> [a] -> Maybe b
  firstAnswer fun [] = Nothing
  firstAnswer fun (x:xs) = case fun (x) of
    Nothing -> (firstAnswer fun xs)
    Just x -> Just x

  {-
  allAnswers takes a function and a list and applies the function to the list, if it ever returns Nothing then it returns Nothing
  otherwise it returns a list of Just [lst] values
  param- fun is a function applied to the list
  param- (x:xs) a list
  -}
  allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]
  allAnswers fun [] = Just []
  allAnswers fun (x:xs) =
    case fun (x) of
      Nothing -> Nothing
      Just [x] -> Just [x]
      Just [] -> Just []


  {-
  WildcardPat matches everything and produces the empty list of bindings
  VariablePat s matches any value v and produces the one-element list holding (s, v).
  UnitPat matches only Unit and produces the empty list of bindings
  ConstantPat 17 matches only against Constant 17 and produces the empty list of bindings (and similarly for other integers)
  ConstructorPat (s1, p) matches Constructor(s2, v) if s1 and s2 are the same string (you can compare them with ==) and p
      matches v. The list of bindings produced in the list from the nested pattern match. We call the strings s1 and s2 the constructor names.
  TuplePat ps matches a value of the form Tuple vs if ps and vs have the same length and for all i, the ith element of ps matches the ith
      element of vs. The list of bindings produced is all the lists from the nested pattern matches appended together.
  Nothing else matters™. In case that's not clear, in all other cases you return Nothing.
  -}
  data Pattern = WildcardPat | VariablePat (String) | UnitPat | ConstantPat (Int) | ConstructorPat (String, Pattern) | TuplePat ([Pattern]) deriving (Show)

  data Value = Constant (Int) | Unit | Constructor (String, Value) | Tuple [Value] deriving (Show)

  {-
  g takes two functions as arguments (f1 and f2) and a Pattern data type.
  There is a nested function r that uses the function g with the functions f1 and f2 that checks for the different patterns and what to do.
  For WildcardPat it takes f1 and returns a number
  For Variable pat it takes f2 and returns the a number based off of the corresponding number x with it
  ConstructorPat takes a wild card and pattern and applies function r on it
  TuplePate takes values and uses foldl on it
  _ returns 0
  -}
  g f1 f2 pat = let
                  r = g f1 f2
                    in
                      case pat of
                        WildcardPat -> f1 ()
                        VariablePat x -> f2 x
                        ConstructorPat (_, p) -> r p
                        TuplePat values -> foldl (\i p -> (r p) + i) 0 values
                        _ -> 0

  {-
  countWildcards takes a pattern and returns how many WildcardPat it contains
  param- pattern is a Pattern type value
  -}
  countWildcards pattern = g (\x-> 1) (\y -> (length y)) pattern

  {-
  countWildAndVariableLengthstakes a pattern and returns the number of Wildcard patterns it contains, plus the sum of the string lengths of all
  the variables in the variable patterns it contains
  param- pattern is a Pattern type value
  -}
  countWildAndVariableLengths pattern = g (\x-> 1) (\y -> (length y)) pattern

  {-
  countAVar takes a string and a pattern (as a pair) and returns the number of times the string appears as a variable in the pattern.
  param- s is a string
  param- p is a Pattern type value
  -}
  countAVar (s, p) = g (\x-> 1) (\y -> if y == s then 1 else 0) p

  {-
  checkPat takes a Pattern and returns true if and only if all the variables appearing in the pattern are distinct from each other
  param- pattern is a Pattern type value
  -}
  checkPat pattern = case pattern of
    WildcardPat -> True
    VariablePat s -> True
    UnitPat -> True
    ConstantPat i -> True
    ConstructorPat (s, p) -> case p of
      WildcardPat -> True
      VariablePat s -> True
      UnitPat -> True
      ConstantPat i -> True
      ConstructorPat (s, p) -> False
      TuplePat ([p]) -> True || (checkPat p)
    TuplePat ([p]) -> case p of
      WildcardPat -> True
      VariablePat s -> True
      UnitPat -> True
      ConstantPat i -> True
      ConstructorPat (s, p) -> True || (checkPat p)
      TuplePat ([p]) -> False

  {-
  match takes a (Value, Pattern) and returns a Maybe [(String, Value)] it returns Nothing if the Pattern does not match and Just lst
  where lst is the list of bindings if it does
  param- p is a Pattern type value
  param- v is a Value type value
  -}
  match (v, p) = case p of
    WildcardPat -> case v of
      Constant it -> Just [("", v)]
      Unit -> Just [("", Unit)]
      Constructor (st, va) -> Just [("", v)]
      Tuple [va] -> Just [("", v)]
    VariablePat s -> case v of
      Constant it -> Just [(s, v)]
      Unit -> Just [(s, Unit)]
      Constructor (st, va) -> Just [(s, v)]
      Tuple [va] -> Just [(s, v)]
    UnitPat -> case v of
      Constant it -> Nothing
      Unit -> Just [("", v)]
      Constructor (st, va) -> Nothing
      Tuple [va] -> Nothing
    ConstantPat i -> case v of
      Constant it -> if i == it
        then Just []
        else Nothing
      Unit -> Nothing
      Constructor (st, va) -> Nothing
      Tuple [va] -> Nothing
    ConstructorPat (s, p) -> case v of
      Constant it -> Nothing
      Unit -> Nothing
      Constructor (st, va) -> if (st == s)
        then Just []
        else Just [(s, v)]
      Tuple [va] -> Nothing
    TuplePat ([p]) ->case v of
      Constant it -> Nothing
      Unit -> Nothing
      Constructor (st, va) -> Nothing
      Tuple [va] -> Just []

  {-
  takes a Value and a list of Patterns and returns a Maybe [(String, Value)], in particular, Nothing if no pattern in the list matches,
  or Just lst, where lst is the list of bindings for the first pattern in the list that matches
  param- val is a Value type values
  param- (x:xs) is a list of pattern type values
  -}
  firstMatch val (x:xs) =
    let
      help p = match (val, p)
    in
      firstAnswer help (x:xs)




  --
