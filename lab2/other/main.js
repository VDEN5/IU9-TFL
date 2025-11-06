let automat = [
    {"a":7, "b":7, "c":11, "d":4, "e":5},    // state 0
    {"a":7, "b":7, "c":7, "d":6, "e":7},     // state 1
    {"a":7, "b":7, "c":5, "d":7, "e":7},     // state 2
    {"a":7, "b":7, "c":7, "d":7, "e":0},     // state 3
    {"a":7, "b":7, "c":1, "d":0, "e":7},     // state 4
    {"a":7, "b":7, "c":2, "d":0, "e":3},     // state 5
    {"a":7, "b":7, "c":10, "d":7, "e":7},    // state 6
    {"a":7, "b":7, "c":7, "d":7, "e":7},     // state 7 (trap)
    {"a":7, "b":7, "c":12, "d":8, "e":5},    // state 8
    {"a":7, "b":7, "c":13, "d":8, "e":9},    // state 9
    {"a":7, "b":7, "c":14, "d":8, "e":9},    // state 10
    {"a":17, "b":7, "c":15, "d":15, "e":15}, // state 11
    {"a":17, "b":7, "c":15, "d":18, "e":15}, // state 12
    {"a":17, "b":7, "c":16, "d":15, "e":15}, // state 13
    {"a":17, "b":7, "c":21, "d":19, "e":20}, // state 14
    {"a":7, "b":7, "c":7, "d":7, "e":7},     // state 15 (final)
    {"a":7, "b":7, "c":2, "d":0, "e":3},     // state 16 (final)
    {"a":7, "b":7, "c":0, "d":7, "e":7},     // state 17 (final)
    {"a":7, "b":7, "c":10, "d":7, "e":7},    // state 18 (final)
    {"a":7, "b":7, "c":12, "d":8, "e":5},    // state 19 (final)
    {"a":7, "b":7, "c":13, "d":8, "e":9},    // state 20 (final)
    {"a":17, "b":7, "c":21, "d":19, "e":20}  // state 21 (final)
];

function getRes(word) {
    let fin;
    
    function getState(word1, curState) {
        if (word1 == "") {
            fin = curState;
            return;
        }
        let symbol = word1[0];
        let nextState = automat[curState][symbol];
        getState(word1.substring(1), nextState);
    }
    
    getState(word, 0);
    
    // Проверяем, является ли конечное состояние принимающим (15-21)
    // return fin
    if (fin >= 15 && fin <= 21) {
        return true;
    }
    return false;
}

// Тестовые примеры
// getRes("aaa")
// function generateLexWords(maxCount = 100) {
//     const alphabet = ['a', 'b', 'c', 'd', 'e'];
//     const words = [''];
//     let index = 0;
    
//     while (words.length < maxCount && index < words.length) {
//         const currentWord = words[index];
        
//         for (const char of alphabet) {
//             if (words.length >= maxCount) break;
//             words.push(currentWord + char);
//         }
        
//         index++;
//     }
    
//     // Убираем пустое слово если нужно
//     return words.filter(word => word !== '');
// }
// let test_words=generateLexWords(100000), mem=new Array(22)
// for (let t of test_words){
//     let res=getRes(t)
//     if (!mem[res]) mem[res]=t 
// }
// for (let i=0;i<22;i++){
//     console.log(i,mem[i])
// }
// console.log(getRes("c")); // true - должно попасть в состояние 15
// console.log(getRes("a")); // false - должно попасть в состояние 7
// console.log(getRes("ddcd")); // можно тестировать разные слова
let words=["","dc","ec","ee","d","e",
    "dcd","a", "dcdcd","dcdce","dcdc",
    "c", "dcdcdc", "dcdcec","dcdcc", "cc",
    "dcdcecc","ca","dcdcdcd","dcdccd","dcdcce","dcdccc",  
]
let pref=["","a","cc","dcd","dc", "eeedce","cdcd", "cca","dcca", "eca"], ma=new Map()
for (let i=0;i<words.length;i++){
    let arr=[]
    for (let j=0;j<pref.length;j++){
        arr.push(getRes(words[i]+pref[j]) ? "+": "-")
    }
    let str=arr.join(" ")
    if (ma.has(str)){
        ma.get(str).push(i)
    } else{
        ma.set(str,[i])
    }
    console.log(arr.join(" "))
}
// console.log(getRes("ca"),getRes("cc"),getRes("cca"))