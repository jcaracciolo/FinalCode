let user1= {
    name: "JuanFra",
    age: 15,
    grade: 8
}
let user2= {
    name: "Tomas",
    age: 14,
    grade: 6
}

let user3= {
    name: "OTA",
    age: 11,
    grade: 9
}

users = {}
users[0] = user1
users[1] = user2
users[2] = user3

let i = 3
let promAge = 0
let promGrade = 0

while(i>0) {
    i = i-1
    promAge = promAge + users[i].age
    promGrade = promGrade + users[i].grade
}

print("Promedio de edad " $ promAge/3)
print("Promedio de nota " $ promGrade/3)