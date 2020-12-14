-- list of smaller case characters from 'a' to 'z' with numbers starting from 1
list = [(char, num) | (char, num) <- zip ['a'..'z'] [1..]]

-- Returns odd number characters
fun1 = [char | (char, num) <- list, odd num]

-- Returns characters that have a number that is a product of two odd positive integers x and y, where x/=1 and y/=1
fun2 = [char | (char, num) <- list, productOfTwoOdds num]

productOfTwoOdds n = not (null [x*y | x<-[3,5..n], y<-[3,5..n], mod n (x*y)==0, odd n])