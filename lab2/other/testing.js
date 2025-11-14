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
    {}, // state 0 (не используется)
    {"d": [2], "e": [8], "c": [11, 13]},                    // state 1
    {"c": [3], "d": [1]},                                   // state 2
    {"d": [4]},                                             // state 3
    {"c": [5]},                                             // state 4
    {"c": [5], "d": [1, 6], "e": [1, 7]},                  // state 5
    {"d": [1, 6]},                                          // state 6
    {"e": [1, 7]},                                          // state 7
    {"d": [1], "c": [9], "e": [10]},                       // state 8
    {"c": [8]},                                             // state 9
    {"e": [1]},                                             // state 10
    {"a": [12]},                                            // state 11
    {"c": [1]},                                             // state 12
    {"a": [14], "c": [14], "d": [14], "e": [14]},          // state 13
    {}                                                      // state 14 (final)
];

// Финальное состояние
const finalState = 14;

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
    
    processWord(word, 1);
    return res;
}

let automat3 = {
    0: {'ε': [1, 20]}, // начальное состояние с эпсилон-переходами
    
    1: {'a': [8], 'c': [10], 'd': [5], 'e': [6]},    // бывшее 0
    2: {'a': [8], 'c': [8], 'd': [7], 'e': [8]},     // бывшее 1
    3: {'a': [8], 'c': [6], 'd': [8], 'e': [8]},     // бывшее 2
    4: {'a': [8], 'c': [8], 'd': [8], 'e': [1]},     // бывшее 3
    5: {'a': [8], 'c': [2], 'd': [1], 'e': [8]},     // бывшее 4
    6: {'a': [8], 'c': [3], 'd': [1], 'e': [4]},     // бывшее 5
    7: {'a': [9], 'c': [9], 'd': [9], 'e': [9]},     // бывшее 6
    8: {'a': [8], 'c': [8], 'd': [8], 'e': [8]},     // бывшее 7 (ловушка)
    9: {'a': [9], 'c': [11], 'd': [9], 'e': [9]},    // бывшее 8
    10: {'a': [13], 'c': [12], 'd': [12], 'e': [12]},// бывшее 9
    11: {'a': [14], 'c': [15], 'd': [14], 'e': [14]},// бывшее 10
    12: {'a': [8], 'c': [8], 'd': [8], 'e': [8]},    // бывшее 11 (final)
    13: {'a': [8], 'c': [1], 'd': [8], 'e': [8]},    // бывшее 12 (final)
    14: {'a': [9], 'c': [11], 'd': [9], 'e': [9]},   // бывшее 13 (final)
    15: {'a': [14], 'c': [15], 'd': [14], 'e': [14]},// бывшее 14 (final)
    
    // Второй автомат
    16: {'a': [16], 'c': [16], 'd': [17], 'e': [16]}, // бывшее 19
    17: {'a': [16], 'c': [18], 'd': [17], 'e': [16]}, // бывшее 20
    18: {'a': [16], 'c': [16], 'd': [19], 'e': [16]}, // бывшее 21
    19: {'a': [8], 'c': [20], 'd': [8], 'e': [8]},    // бывшее 22
    20: {'a': [16], 'c': [20], 'd': [21], 'e': [22]}, // бывшее 23
    21: {'a': [8], 'c': [18], 'd': [21], 'e': [16]}, // бывшее 24
    22: {'a': [8], 'c': [16], 'd': [17], 'e': [22]}  // бывшее 25
};

// Финальные состояния для третьего автомата (объединение финальных состояний обоих автоматов)
const finalStates3 = new Set([12, 13, 14, 15, 16, 17, 18,  19, 20, 21, 22]);

function getRes3(word) {    
    function processWord(word1, currentState) {
        if (currentState === 0) { // начальное состояние
            let nextStates = automat3[currentState]['ε'];
            let result = true;
            for (let state of nextStates) {
                result = result && processWord(word1, state);
            }
            return result;
        }
        
        if (word1 === "") {
            return finalStates3.has(currentState);
        }
        
        let symbol = word1[0];
        let nextStates = automat3[currentState][symbol];

        if (!nextStates || nextStates.length === 0) return false;

        let result = false;
        for (let nextState of nextStates) {
            result = result || processWord(word1.substring(1), nextState);
        }
        return result;
    }
    
    return processWord(word, 0);
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