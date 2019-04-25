--palindromo xs = if xs == reverse xs then "eh palindromo" else "nao eh palindromo"
mdc x y = if x==y then x else if x>y then mdc (x-y) y else mdc (y-x) y
--palindromo x = if reverso x 0== x then True else False
contaDigitos x = conta x 0
conta x y = if x == 0 then y else conta (x `div` 10) (y+1)
