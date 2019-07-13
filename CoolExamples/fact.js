/*
	Factorial function.
	Calculates N!, where N is a number read from standard input.
*/
fact = function(n) {
    if (n == 1) {
        return n;
    } else {
        return fact(n-1)*n
    }
    return a
}

while(true) {
    print("Factorial function: Enter N")
    n = readNum()
    print(fact(n))
}