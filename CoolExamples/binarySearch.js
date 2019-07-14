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

partition = function(arr, lo, hi) {
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


quicksort = function(arr, lo, hi) {
    if (lo < hi) {
        let p = partition(arr, lo, hi)
        quicksort(arr, lo, p - 1)
        quicksort(arr, p + 1, hi)
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

print("Binary Search: Enter the numbers")
let counter = 0
continue = true
arr = {}
while(continue) {
    let nextNum = readNum()
    arr[counter] = nextNum
    counter = counter + 1
    print("Want to stop? (0: no)")
    let stop = readNum()
    if(stop != 0) {
        continue = false
    }
}

print("Now enter the number you are looking for")
// hack to easily remove preceding and trailing spaces
let n = readNum()

quicksort(arr, 0, counter - 1)

let position = binarysearch(arr, counter, n)

if (position >= 0) {
    print("The number " $ n $ " is at the " $ position $ " position.")
} else {
    print("The number " $ n $ " is not at the array.")
}

print(arr)
