a = function() {
    print("This is OK to execute")

    z = function(x) {
        return x
    }

    if(2==2) {
        return 1
        print("THIS SHOULD NOT BE EXECUTED")
    }
    print("NEITHER SHOULD THIS")
}

execute = function() {
    b= a()
    uno = z(1)
    if(b==uno) {
        print("Variable B y Z(1) is UNO!!!")
    } else {
        print("NOPE")
    }

     return 1
}

execute()
