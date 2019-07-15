/*
	Simple example of object usage in Sauerkraut.
	Follow the command line prompts and insert users in a db.
	Afterward, you may consults users by their names.
*/

print("Hello User! Welcome to our simple registry. ")
print("Let's begin by inserting some users.")

continue = true
bag = {}
size = 0

addUser = function() {
    let user = {}
    print("Please type a name")
    user["name"] = readLn()
    print("Please type their age")
    user["age"] = readNum()
    print("Please type their grade")
    user["grade"] = readNum()
    bag[user["name"]] = user
    bag[size] = user
    size = size + 1
}

while (continue) {
    addUser()
    print("Excellent! Would you like to continue? [1/0]")
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
                print("Wrong answer. Would you like to continue? [1/0]")
                ans = readNum()
            }
        }
    }
}

print("All your users are now in your registry.")
print("You may consult for a user by typing its name")

getName = function() {
    print("Type user name")
    let str = readLn()
    let user = bag[str]
    print("User " $ user["name"] $ " is " $ user["age"] $ " years old and he got a " $ user["grade"] $ "." )
}


getMin = function(value) {
    print("Min")
    let i = 0
    let minUser = bag[0]
    while(i < size) {
        let check =  bag[i]
        if(check[value] < minUser[value]) {
            minUser = check
        }
        i = i+1
    }

    return minUser
}


getMax = function(value) {
    let i = 0
    let maxUser = bag[0]
    while(i < size) {
        let check =  bag[i]
        if(check[value] > maxUser[value]) {
            maxUser = check
        }
        i = i+1
    }

    return maxUser
}


getAggregatedMM = function(value, min) {
    let user = {};
    if(min) {
        user = getMin(value)
    } else {
        user = getMax(value)
    }

    print("User " $ user["name"] $ " is " $ user["age"] $ " years old and he got a " $ user["grade"] $ "." )
}

getAverage = function(value) {
    let acu = 0
    let count = 0
    while(count < size) {
        let u = bag[count]
        acu = acu + u[value]
        count = count + 1
    }


    print("Average " $ value $ " is " $ (acu / count) $ "." )

}

getAggregated = function(value) {
    print("1: Min")
    print("2: Average")
    print("3: Max")
    c = readNum()
    if(c == 1) {
        getAggregatedMM(value, true)
    } else {
        if(c == 2) {
            getAverage(value)
        } else {
            if(c==3) {
                getAggregatedMM(value, false)
            } else {
                print("Not valid command")
            }
        }
    }

}

while (true) {
    print("Pick a function")
    print("1 look for user")
    print("2 Get aggregated age")
    print("3 Get aggregated grade")
    print("4 Add User")
    let c = readNum()
    if(c == 1) {
        getName()
    }
    if(c == 2) {
        getAggregated("age")
    }

    if(c == 3) {
        getAggregated("grade")
    }

    if(c == 4) {
        addUser()
    }

}