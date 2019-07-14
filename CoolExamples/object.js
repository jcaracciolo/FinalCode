/*
	Simple example of object usage in Sauerkraut.
	Follow the command line prompts and insert users in a db.
	Afterward, you may consults users by their names.
*/

print("Hello User! Welcome to our simple registry. ")
print("Let's begin by inserting some users.")

continue = true
bag = {}
while (continue) {
    let user = {}
    print("Please type a name")
    user["name"] = readLn()
    print("Please type their age")
    user["age"] = readNum()
    print("Please type their grade")
    user["grade"] = readNum()
    bag[user["name"]] = user
    print("Excellent! Would you like to continue? [y: 1/n: 0]")
    let ans = readNum()
    let b = true;
    while (b) {
        if (ans == 0) {
            b = false
            continue = false
        } else {
            if (ans == 1) {
                b = false
                continue = true
            } else {
                print("Wrong answer. Would you like to continue? [y/n]")
                ans = readNum()
            }
        }
    }
}

print("All your users are now in your registry.")
print("You may consult for a user by typing its name")

while (true) {
    let str = readLn()
    let user = bag[str]
    print("User " $ user["name"] $ " is " $ user["age"] $ " years old and he got a " $ user["grade"] $ "." )
}