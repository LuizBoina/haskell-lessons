a = [1,2,3,4,5]
{-  head - first element of list
    last - last element of list
    tail - list without first element
    init - list without last element
    length
    null - return true if the list is null
    reverse
    take n - take n elements of the list
    drop n - take the list after n elements
    minimum, maximum, sum, product
    n `elem` []- search n in the list  
-}
comprehensions = [2*x | x <- [1..10], x*2 >=12] --first condiction, then range and then filtering	
impares xs = [if x < 10 then "BOOM" else "BANG" | x <- xs, odd x]
--we can have multiple predicates in the comprehensions but an element must satify all predicates
multiplyLists xs ys = [x*y|x<-xs, y<-ys]
length' xs = [1|_<-xs]--replace all elements on the list by 1 and _ means that we dont care about what will draw
