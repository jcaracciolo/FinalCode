/*
	Fibonacci series, using memoization.
	Reads the N fibonacci number from the standard input.
*/
fibonacci = function(n) {
    let f = {}
    let i = 2

    f[0] = 0
    f[1] = 1

    while(i <= n){
        f[i] = f[i-1]+ f[i-2]
        i=i+1
    }
    return f[n]
}
print("Fibonacci series: Enter N")
var n = readNum()
print(n $ " fibonacci number: " $ fibonacci(n))