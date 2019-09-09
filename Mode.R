#No Built in function for mode in R
#Mode Function
mode <- function(vector)
{
  t = unique(vector)
  t[which.max(tabulate(match(vector, t)))]
}
#which.max()
#which.max returns the position of the element with the maximal value 
#in a vector. 
#Match()
# match returns a vector of the positions of (first)
# matches of its first argument in its second.
#Tabulate()
# tabulate() function takes the integer-valued vector bin and counts 
# the number of times each integer occurs in it.