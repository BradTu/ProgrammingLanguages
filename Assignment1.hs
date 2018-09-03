--Brad Tully 30 March 2018
--Programming Assignment 1: Programming Languages

module A1b where

  --sDotProduct takes a single number and two pairs of numbers and finds the dot prouct between them, then scales it by the single numbers
  --param- scale- number that is multiplied by the dot product of the two sumTwoPairs
  --param- (a, b) a pair of two numbers used to find the dot product
  --param- (c, d) another pair of two numbers used to find the dot product
  --result- the function returns a number
  sDotProduct scale (a, b) (c, d) = scale * ((a * c) + (b * d))

  --distance- takes two 2D coordinates (a pair of numbers) and finds the distance between them
  --param- (x1, y1) the first 2D coordinates (numbers)
  --param- (x2, y2) the second 2D coordinates (numbers)
  --result- the function returns a number, more than likely a float
  distance (x1, y1) (x2, y2) = sqrt(((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)))

  --tripleDistance- takes two 3D coordinates (3-tuples) and finds the distance between them
  --param- (x1, y1, z1) the first 3D coordinate (numbers)
  --param- (x2, y2, z2) the second 3D coordinate (numbers)
  --result- the function returns a number, more than likely a float
  tripleDistance  (x1, y1, z1) (x2, y2, z2) = sqrt(((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)) + ((z2 - z1) * (z2 - z1)))

  --compareMax- this is a helper function for findMax, it takes two numbers and returns the larger one
  --param- a- a number
  --param- b- a number
  --result- a number either a or b
  compareMax a b = if a > b
    then a
    else b

  --findMax- takes a list of numbers and finds the maximum number in the list
  --param- list is a list of numbers
  --result- the largest number from the list
  findMax list = if null list
    then 0
    else compareMax (head list) (findMax (tail list))

  --tupleDotProduct- takes two lists of numbers and finds the dot product between them
  --param- list1 is a list of numbers
  --param- list2 is a list of numbers
  --result- this will return a number, specifically the dot product of the two lists
  tupleDotProduct list1 list2 = if null list1
    then 0
    else ((head list1) * (head list2)) + (tupleDotProduct (tail list1) (tail list2))

  --revZip2Lists- takes two lists and makes a new list of pairs of each corresponding entry in each list, but in reverse order
  --param- list1 is a list of things
  --param- list2 is a list of things
  --result- a list of tuples
  revZip2Lists list1 list2 = if null list1
    then []
    else [(last list2, last list1)] ++ (revZip2Lists (init list1) (init list2))

  --everyThird- this takes a list and returns a new list of everyThird item from the list argument
  --param- (first:tailList) is a list of things where first is the first element and tailList is the remaining elements
  --result- a list made up of every third item from the passed in list
  everyThird (first:tailList) = if length (first:tailList) < 3
    then []
    else if length (first:tailList) == 3
      then [head (tail tailList)]
      else [head (tail tailList)] ++ (everyThird (tail (tail tailList)))
