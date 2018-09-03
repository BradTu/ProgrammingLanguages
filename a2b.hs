--Made by Brad Tully 11 April 2018
--Programming Languages Assignment 2: Haskell Types, Pattern Matching and Type Classes

module A2b where

  --removeAllExcept- takes a value and a list and removes elements in the list that aren't the same as the value
  --param- val- this is a value of the same type that the list is made up of
  --param- (x:xs)- a list of elements
  --result- the function returns a list composing of only items that match val, if any
  removeAllExcept :: Eq a => a -> [a] -> [a]
  removeAllExcept val [] = []
  removeAllExcept val [x] = if x == val
    then [x]
    else []
  removeAllExcept val (x:xs) = if x == val
    then [x] ++ (removeAllExcept (val) (xs))
    else [] ++ (removeAllExcept (val) (xs))

  --removeAll- takes a value and a list of items and removes an occurences that match val from the list passed in
  --param- val- an arbitrary value of the same type of the items in the list used to remove matching values in the list
  --param- (x:xs)- a list of items
  --result- a list comprising of values that aren't equal to val, if any
  removeAll :: Eq a => a -> [a] -> [a]
  removeAll val [] = []
  removeAll val [x] = if x /= val
    then [x]
    else []
  removeAll val (x:xs) = if x /= val
    then [x] ++ (removeAll (val) (xs))
    else [] ++ (removeAll (val) (xs))

  --substitute- takes two values the first "find" is replaced by "replace" in the list passed in
  --param- find- a value that is used to identify values in the list to replace
  --param- replace- a value that replaces any items in the list that match "find"
  --param- (x:xs)- a list of items
  --result- this returns a list where any occurences of "find" is swapped with "replace" in the returning list
  substitute :: Eq a => a -> a -> [a] -> [a]
  substitute find replace [] = []
  substitute find replace [x] = if x == find
    then [replace]
    else [x]
  substitute find replace (x:xs) = if x == find
    then [replace] ++ (substitute (find) (replace) (xs))
    else [x] ++ (substitute (find) (replace) (xs))

  --mergeSorted3- takes 3 sorted lists in increasing order and combines them into one list in increasing order
  --param- (x:xs)- a list in increasing order
  --param- (y:ys)- a list in increasing order
  --param- (z:zs)- a list in increasing order
  --result- a new list in sorted increasing order made up of the elements from the three lists passed in
  mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
  mergeSorted3 [] [] [] = []
  mergeSorted3 (x:xs) [] [] = [x] ++ (mergeSorted3 (xs) [] [])
  mergeSorted3 [] (y:ys) [] = [y] ++ (mergeSorted3 [] (ys) [])
  mergeSorted3 [] [] (z:zs) = [z] ++ (mergeSorted3 [] [] (zs))
  mergeSorted3 (x:xs) (y:ys) [] = if x <= y
    then [x] ++ (mergeSorted3 (xs) (y:ys) [])
    else [y] ++ (mergeSorted3 (x:xs) (ys) [])
  mergeSorted3 (x:xs) [] (z:zs) = if x <= z
    then [x] ++ (mergeSorted3 (xs) [] (z:zs))
    else [z] ++ (mergeSorted3 (x:xs) [] (zs))
  mergeSorted3 [] (y:ys) (z:zs) = if y <= z
    then [y] ++ (mergeSorted3 [] (ys) (z:zs))
    else [z] ++ (mergeSorted3 [] (y:ys) (zs))
  mergeSorted3 (x:xs) (y:ys) (z:zs) = if (x <= y) && (x <= z)
    then [x] ++ (mergeSorted3 (xs) (y:ys) (z:zs))
    else if (y <= x) && (y <= z)
      then [y] ++ (mergeSorted3 (x:xs) (ys) (z:zs))
      else if (z <= x) && (z <= y)
        then [z] ++ (mergeSorted3 (x:xs) (y:ys) (zs))
        else (mergeSorted3 (x:xs) (y:ys) (z:zs))

  --TriTree data type can either be an EmptyNode or a TriNode with a value and three more TriTrees
  data TriTree a = EmptyNode | TriNode a (TriTree a) (TriTree a) (TriTree a) deriving (Show)

  instance (Eq a) => Eq (TriTree a) where
    EmptyNode           == EmptyNode = True
    TriNode a la ma ra  == TriNode b lb mb rb = (a == b) &&
                                                (la == lb) &&
                                                (ma == mb) &&
                                                (ra == rb)
    _ == _ = False

  --nodeValue- takes a TriTree and returns its node value if it has one, throws an error if it's an EmptyNode
  --param- tt- a TriTree
  --result- either returns the value of the node if there is one or throws an error
  nodeValue :: TriTree a -> a
  nodeValue tt =
    case tt of
      EmptyNode -> error "This is an empty node"
      TriNode aValue left middle right-> aValue

  --leftChild- takes a TriTree and returns it's left child, if it's empty it returns an error
  --param- tt- a TriTree
  --result- A TriNode, EmptyNode or an error if it's an EmptyNode
  leftChild :: TriTree a -> TriTree a
  leftChild tt =
    case tt of
      EmptyNode -> error "This is an empty node"
      TriNode aValue left middle right-> left

  --middleChild- takes a TriTree and returns it's middle child, if it's empty it returns an error
  --param- tt- a TriTree
  --result- A TriNode, EmptyNode or an error if it's an EmptyNode
  middleChild :: TriTree a -> TriTree a
  middleChild tt =
    case tt of
      EmptyNode -> error "This is an empty node"
      TriNode aValue left middle right-> middle

  --rightChild- takes a TriTree and returns it's right child, if it's empty it returns an error
  --param- tt- a TriTree
  --result- A TriNode, EmptyNode or an error if it's an EmptyNode
  rightChild :: TriTree a -> TriTree a
  rightChild tt =
    case tt of
      EmptyNode -> error "This is an empty node"
      TriNode aValue left middle right-> right

  --inTree- takes a value and a TriTree and determines whether or not that value is in the tree
  --param- find- a value used to find in the tree
  --param- tt- a TriTree
  --result- True if "find" is in tt, False if it is not
  inTree :: Eq a => a -> TriTree a -> Bool
  inTree find tt =
    case tt of
      EmptyNode -> False
      TriNode aValue left middle right-> if aValue == find
        then True
        else (inTree find left) || (inTree find middle) || (inTree find right)

  --leafList- takes a TriTree and returns a list of all the leaves (a TriNode with three EmptyNodes for children)
  --param- tt- a TriTree
  --result- a list of leaf values if there are any
  leafList :: TriTree a -> [a]
  leafList tt =
    case tt of
      EmptyNode -> []
      TriNode aValue EmptyNode EmptyNode EmptyNode -> [aValue]
      TriNode aValue left EmptyNode EmptyNode -> (leafList left)
      TriNode aValue EmptyNode middle EmptyNode -> (leafList middle)
      TriNode aValue EmptyNode EmptyNode right -> (leafList right)
      TriNode aValue left middle EmptyNode -> (leafList left) ++ (leafList middle)
      TriNode aValue left EmptyNode right -> (leafList left) ++ (leafList right)
      TriNode aValue EmptyNode middle right -> (leafList middle) ++ (leafList right)
      TriNode aValue left middle right -> (leafList left) ++ (leafList middle) ++ (leafList right)

  --inOrderMap- Takes a function and a TriTree moves through the tree in order and applies the function on the node values
  --param- func- a function ie (\x -> x + 5)
  --param- tritree- a trinary tree
  --result- returns a modified TriTree
  inOrderMap :: (a -> b) -> TriTree a -> TriTree b
  inOrderMap func tritree =
    case tritree of
      EmptyNode -> EmptyNode
      TriNode aValue EmptyNode EmptyNode EmptyNode -> let
                                                        tt = TriNode (func aValue) EmptyNode EmptyNode EmptyNode
                                                          in tt
      TriNode aValue left EmptyNode EmptyNode -> let
                                                  tt = TriNode (func aValue) (inOrderMap func left) EmptyNode EmptyNode
                                                    in tt
      TriNode aValue EmptyNode middle EmptyNode -> let
                                                    tt = TriNode (func aValue) EmptyNode (inOrderMap func middle) EmptyNode
                                                      in tt
      TriNode aValue EmptyNode EmptyNode right -> let
                                                    tt = TriNode (func aValue) EmptyNode EmptyNode (inOrderMap func right)
                                                    in tt
      TriNode aValue left middle EmptyNode -> let
                                                tt = (inOrderMap func left)
                                                in let
                                                  tt2 = (inOrderMap func middle)
                                                  in let
                                                    tt3 = TriNode (func aValue) (tt) (tt2) EmptyNode
                                                    in tt3
      TriNode aValue left EmptyNode right -> let
                                              tt = (inOrderMap func left)
                                              in let
                                                tt2 = (inOrderMap func right)
                                                in let
                                                  tt3 = TriNode (func aValue) tt EmptyNode (tt2)
                                                  in tt3
      TriNode aValue EmptyNode middle right -> let
                                                tt = (inOrderMap func middle)
                                                in let
                                                  tt2 = (inOrderMap func right)
                                                  in let
                                                    tt3 = TriNode (func aValue) EmptyNode (tt) (tt2)
                                                    in tt3
      TriNode aValue left middle right -> let
                                            tt = (inOrderMap func left)
                                            in let
                                              tt2 = (inOrderMap func middle)
                                              in let
                                                tt3 = (inOrderMap func right)
                                                in let
                                                  tt4 = TriNode (func aValue) tt tt2 tt3
                                                  in tt4

  --preOrderFold- takes a function and applies it to the accumulator value and the node values of the TriTree moving through it in a pre order fashion
  --param- func- a function ie (\x y -> x * y)
  --param- accum- the accumulator value a number that will be passed and returned from the function
  --param- tritree- a TriTree
  --result- the function returns a number that is the result of the function being applied to the accumulator and node values
  preOrderFold:: (b -> a -> b) -> b -> TriTree a -> b
  preOrderFold func accum tritree =
    case tritree of
      EmptyNode -> accum
      TriNode aValue EmptyNode EmptyNode EmptyNode -> (preOrderFold func (func accum aValue) EmptyNode)
      TriNode aValue left EmptyNode EmptyNode -> (preOrderFold func (func accum aValue) (left))
      TriNode aValue EmptyNode middle EmptyNode -> (preOrderFold func (func accum aValue) (middle))
      TriNode aValue EmptyNode EmptyNode right -> (preOrderFold func (func accum aValue) (right))
      TriNode aValue left middle EmptyNode -> (preOrderFold func (func (preOrderFold func (accum) (left)) aValue) (middle))
      TriNode aValue left EmptyNode right -> (preOrderFold func (func (preOrderFold func (accum) (left)) aValue) (right))
      TriNode aValue EmptyNode middle right -> (preOrderFold func (func (preOrderFold func (accum) (middle)) aValue) (right))
      TriNode aValue left middle right -> (preOrderFold func (func (preOrderFold func ((preOrderFold func (accum) (left))) (middle)) aValue) (right))
