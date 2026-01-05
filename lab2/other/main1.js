let automat = [
    {"c": [3, 20], "e": [6], "d": [12, 1]},                    // state 0
    {"d": [2]},                                                // state 1
    {"c": [3, 20], "e": [6], "d": [12, 1]},                    // state 2
    {"a": [4]},                                                // state 3
    {"c": [5]},                                                // state 4
    {"c": [3, 20], "e": [6], "d": [12, 1]},                    // state 5
    {"d": [9], "c": [7], "e": [10]},                           // state 6
    {"c": [8]},                                                // state 7
    {"e": [10], "c": [7], "d": [9]},                           // state 8
    {"c": [3, 20], "e": [6], "d": [12, 1]},                    // state 9
    {"e": [11]},                                               // state 10
    {"c": [3, 20], "e": [6], "d": [12, 1]},                    // state 11
    {"c": [13]},                                               // state 12
    {"d": [14]},                                               // state 13
    {"c": [16, 14]},                                           // state 14
    {},                                                        // state 15 (не используется)
    {"c": [3, 20], "e": [6, 18], "d": [12, 1, 19]},            // state 16
    {},                                                        // state 17 (не используется)
    {"c": [3, 20], "e": [6, 18], "d": [12, 1]},                // state 18
    {"c": [3, 20], "e": [6], "d": [12, 1, 19]},                // state 19
    {"a": [21], "c": [21], "e": [21], "d": [21]},              // state 20
    {}                                                         // state 21 (final)
];

// Финальное состояние
const finalState = 21;

function getRes(word,sta) {
    let res = false;
    
    function processWord(word1, currentState) {
        if (res || (word1 === "" && currentState === finalState)) {
            res = true;
            return;
        }
        if (word1 === "") return;
        
        let symbol = word1[0];
        let nextStates = automat[currentState][symbol];

        if (!nextStates || nextStates.length === 0) return;

        for (let t of nextStates) {
            processWord(word1.substring(1), t);
        }
    }
    
    processWord(word, sta);
    return res;
}

// Примеры использования:
let words=["ca","c","","e","ec","ee"
]
let pref=["","a","ca", "dca", "cdca", "eca"], ma=new Map()
for (let i=0;i<words.length;i++){
    let arr=[]
    for (let j=0;j<pref.length;j++){
        arr.push(getRes(words[i]+pref[j],0) ? "+": "-")
    }
    let str=arr.join(" ")
    if (ma.has(str)){
        ma.get(str).push(i)
    } else{
        ma.set(str,[i])
    }
    console.log(arr.join(" "))
}