/*
	Binary Search as seen @www.wikipedia.com.
	Enter the array of strings and the string you are looking for,
	returns the position.
	Uses quicksort for sorting.
*/

floor = function(x) {
    f = 0
    while(f < x) {
        f = f+1
    }
    return f - 1
}

partition = function(lo, hi) {
    let pivot = arr[hi];
    let i = lo - 1;

    let j = lo;
    while (j <= hi) {
        if (arr[j] <= pivot) {
            i = i + 1;
            if (i != j) {
                let temp = arr[i];
                arr[i] = arr[j];
                arr[j] = temp;
            }
        }
        j = j + 1;
    }
    return i
}


quicksort = function(lo, hi) {
    if (lo < hi) {
        let p = partition(lo, hi)
        quicksort(lo, p - 1)
        quicksort(p + 1, hi)
    }
}

binarysearch = function(arr, size, n) {
    let l = 0;
    let r = size - 1;

    while (l <= r) {
        let m = floor((l + r) / 2)
        if (arr[m] < n) {
            l = m + 1
        } else {
            if (arr[m] > n) {
                r = m - 1
            } else {
                return m
            }
        }
    }
    return -1
}

print("Binary Search: Enter the amount of numbers")
let it = readNum()
let counter = 0
arr = {}
while(it > 0) {
    let nextNum = readNum()
    arr[counter] = nextNum
    counter = counter + 1
    it = it - 1
}

print("Now enter the number you are looking for")
// hack to easily remove preceding and trailing spaces
let n = readNum()

print(arr)
quicksort(0, counter - 1)
print(arr)

let position = binarysearch(arr, counter, n)

if (position >= 0) {
    print("The number " $ n $ " is at the " $ position $ " position.")
} else {
    print("The number " $ n $ " is not at the array.")
}
