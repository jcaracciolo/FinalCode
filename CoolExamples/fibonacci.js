/*
	Fibonacci series, using memoization.
	Reads the N fibonacci number from the standard input.
*/
fibonacci = function(n) {
    if(n==0) {
        return 0
    }

    if(n==1) {
        return 1
    }

    return fibonacci(n-1) + fibonacci(n-2)
}

while(true) {
    print("Enter the fibonacci index")
    n = readNum()
    print(fibonacci(n))
}
