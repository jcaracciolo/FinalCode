two = 0
answer = 0
//Create a Global variables and functions

getOneAndCreateZ = function() {
                        var two = 2 //creates a local variable

                        z = function(x) { return l + x } //Creates a global function

                        if(two==2) {        //Will check against the local variable instead of the global one
                            let answer = 1  //Create block variable
                            return answer    //Will check against the local variable instead of the global one
                            print("THIS SHOULD NOT BE EXECUTED")
                        }

                        print("NEITHER SHOULD THIS")
                    }

l = 0

execute = function() {
    b=getOneAndCreateZ()

    //Function z is global, also l
    if(b==z(1)) {
        print("Variable B y Z(1) are One!!!")
    } else {
        print("NOPE")
    }

     return 1
}

i = 0
while(i<10) {
    i=1+i; execute()
}
