/*
	Simple example of object usage in Sauerkraut.
	Follow the command line prompts and insert users in a db.
	Afterward, you may consults users by their names.
*/

print("Hello User! Welcome to our simple registry. ")
print("Let's begin by inserting some users.")

var continue = 1
var bag = {}
var user = {}

    print("Please type a name")
    user.name = readLn()
    print("Please type their age")
    user.age = readNum()
    print("Please type their greade")
    user.grade = readNum()
    print("User " $ user.name $ " is " $ user.age $" years old and he got a " $  user.grade )

print("All your users are now in your registry.")
print("You may consult for a user by typing its name")

