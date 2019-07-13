print("Please enter your age")
age = 0
while(age < 18) {
    age = readNum()

    if (age < 18) {
        print("You are not old enough to use this compiler")
        print("Retry, please enter your age")
    } else {
        print("Congratulations you are old enough to use this compiler")
    }
}

