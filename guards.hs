bmiTell :: (RealFloat a) => a->a->String
bmiTell weight height
    | bmi <= skinny = "magrin"
    | bmi <= normal = "normal"
    | bmi <= fat = "gordo"
    | otherwise = "gordo pra carai"
    where bmi = weight / height ^ 2
          skinny = 18.0
          normal = 25.0
          fat = 30.0
          --(skinny, normal, fat) = (18.0,25.0,30.0)
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname   
          
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  
