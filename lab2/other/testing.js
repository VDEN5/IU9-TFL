function generateRandomString(n) {
    let result = '';
    const characters = 'acde';
    
    for (let i = 0; i < n; i++) {
        const randomIndex = Math.floor(Math.random() * characters.length);
        result += characters[randomIndex];
    }
    
    return result;
}

let automat1 = [
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

function getRes1(word) {
    let fin;
    
    function getState(word1, curState) {
        if (word1 == "") {
            fin = curState;
            return;
        }
        let symbol = word1[0];
        let nextState = automat1[curState][symbol];
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

let automat2 = [
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

function getRes2(word) {
    let res = false;
    
    function processWord(word1, currentState) {
        if (res || (word1 === "" && currentState === finalState)) {
            res = true;
            return;
        }
        if (word1 === "") return;
        
        let symbol = word1[0];
        let nextStates = automat2[currentState][symbol];

        if (!nextStates || nextStates.length === 0) return;

        for (let t of nextStates) {
            processWord(word1.substring(1), t);
        }
    }
    
    processWord(word, 0);
    return res;
}

let automat3 = {
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

function getRes3(word) {    
    function processWord(word1, currentState) {
        if ((word1 === "" && (currentState === 14 || currentState === 17))) {
            return true;
        }
        if (word1 === "") return false;
        
        let symbol = word1[0];
        let nextStates = automat3[currentState][symbol];

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
    
    return processWord(word, 0);;
}

function getRes(word){
    return /^(dcdc*c(d*|e*e)|e(cc)*(ee|d)|cac|dd)*c(a|c|d|e)$/.test(word)
}

for (let i=0;i<1000;i++){
    let word=generateRandomString(2000)
    let res=(getRes(word) && getRes1(word) && getRes2(word) && getRes3(word)) || (!getRes(word) && !getRes1(word) && !getRes2(word) &&!getRes3(word))
    if (getRes(word)) console.log(word)
    if (!res) console.log(res, getRes(word), getRes1(word), getRes2(word, 0))
}