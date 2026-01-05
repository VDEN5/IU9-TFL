let automat = {
    0: {'a': [1, 15], 'c': [1, 15], 'd': [1, 15], 'e': [1, 15],},
    1: {'d': [2,13], 'e': [8, 13], 'c': [11,13], 'a': [13]},
    2: {'c': [3], 'd': [1]},
    3: {'d': [4]},
    4: {'c': [5]},
    5: {'c': [5], 'd': [1, 6], 'e': [1, 7]},
    6: {'d': [1, 6]},
    7: {'e': [1, 7]},
    8: {'d': [1], 'c': [9], 'e': [10]},
    9: {'c': [8]},
    10: {'e': [1]},
    11: {'a': [12]},
    12: {'c': [1]},
    13: {'a': [14], 'c': [14], 'd': [14], 'e': [14]},
    14: {}, // финальное состояние
    15: {'a': [15], 'e': [15], 'c': [15, 16], 'd': [15]},
    16: {'a': [17], 'c': [17], 'd': [17], 'e': [17]},
    17: {}  // финальное состояние
};

// Финальное состояние
const finalState = 21;

function getRes(word,sta) {    
    function processWord(word1, currentState) {
        if ((word1 === "" && (currentState === 14 || currentState === 17))) {
            return true;
        }
        if (word1 === "") return false;
        
        let symbol = word1[0];
        let nextStates = automat[currentState][symbol];

        if (!nextStates || nextStates.length === 0) return false;

        if (currentState===0){//alternative state
            for (let t of nextStates) {
                if (!processWord(word1, t)) {
                    return false
                }
            }
            return true
        }
        let r=false

        for (let t of nextStates) {
            r=r||processWord(word1.substring(1), t);
        }
        return r
    }
    
    return processWord(word, sta);;
}
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