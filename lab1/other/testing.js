function generateRandomString(n) {
    let result = '';
    const characters = 'ab';
    
    for (let i = 0; i < n; i++) {
        const randomIndex = Math.floor(Math.random() * characters.length);
        result += characters[randomIndex];
    }
    
    return result;
}

function findAllSubstringPositions(str, substring) {
    const positions = [];
    let index = str.indexOf(substring);
    
    while (index !== -1) {
        positions.push(index);
        index = str.indexOf(substring, index + 1);
    }
    
    return positions;
}

let rules = [
    {from: "aaaa", to: "a"},
    {from: "aaab", to: "b"},
    {from: "bbba", to: "ba"},
    {from: "bbbb", to: "bb"},
    {from: "ababb", to: "babb"},
    {from: "bba", to: "baaba"},
    {from: "bbb", to: "baabb"},
    {from: "bbaaa", to: "baab"},
    {from: "bbaab", to: "baa"},
    {from: "bbabb", to: "abab"},
    {from: "baba", to: "baa"},
    {from: "babbaa", to: "babba"},
    {from: "babbab", to: "abb"},
];

function randomReduce(str, count) {
    let arr = [];
    
    for (let rule of rules) {
        let poses = findAllSubstringPositions(str, rule.from);
        if (poses.length == 0) continue;
        
        for (let pos of poses) {
            arr.push({
                pos: pos,
                rule: rule
            });
        }
    }
    
    if (arr.length == 0 || count==6) return [str, true];
    
    let randomIndex = Math.floor(Math.random() * arr.length);
    let el = arr[randomIndex];
    
    // Правильное замещение подстроки
    return [str.substring(0, el.pos) + el.rule.to + str.substring(el.pos + el.rule.from.length), false];
}

function randomNormalize(str1){
    let countRules=0
    while(true){
        let [str, res]=randomReduce(str1, countRules)
        countRules++
        if (res) return str
        str1=str
    }
}
let rules1 = [
    {from: "aaaa", to: "a"},
    {from: "aaab", to: "b"},
    {from: "bbaaa", to: "babbb"},
    {from: "aaaba", to: "baba"},
    {from: "baba", to: "baab"},
    {from: "bab", to: "baa"},
    {from: "baab", to: "baaa"},
    {from: "aaba", to: "bbb"},
    {from: "baa", to: "abb"},
    {from: "bba", to: "bab"},
    {from: "aba", to: "bb"},
    {from: "bb", to: "ba"},
];
function fuzz(str1, str2){
    let res=false, mem=new Set()
    function dfs(tempStr){
        if(tempStr.length<str2.length) return
        if (tempStr==str2 || res){
            res=true
            return
        }
        if(mem.has(tempStr))return
        mem.add(tempStr)
        for (let rule of rules1){
            let poses=findAllSubstringPositions(tempStr, rule.from)
            for (let pos of poses){
                dfs(tempStr.substring(0, pos) + rule.to + tempStr.substring(pos + rule.from.length))
            }
        }
    }
    dfs(str1)
    return res
}

function firstCompare(str1, str2){
    return str1.includes('b') == str2.includes('b')
}

function secondCompare(str1, str2){
    let Regex=[
        /^$/,                          // 1. пустое слово
        /^a(aaa)*$/,                   // 2. a(aaa)*
        /^(b|aaa(aaa)*b)$/,            // 3. b|aaa(aaa)*b  
        /^aa(aaa)*$/,                  // 4. aa(aaa)*
        /^a(aaa)*b$/,                  // 5. a(aaa)*b
        /^(ba|aaa(aaa)*ba|bb|aaa(aaa)*bb|a(aaa)*ba|a(aaa)*bb|aa(aaa)*ba|aa(aaa)*bb)(a|b)*$/, // 6. сложная
        /^aaa(aaa)*$/,                 // 7. aaa(aaa)*
        /^aa(aaa)*b$/                  // 8. aa(aaa)*b
    ];
    let gramInd
    for (let i=0;i<Regex.length; i++){
        if (Regex[i].test(str1)){
            gramInd=i
            break
        }
    }
    return Regex[gramInd].test(str2)
}

function randomReduce1(str, count) {
    let arr = [];
    
    for (let rule of rules1) {
        let poses = findAllSubstringPositions(str, rule.from);
        if (poses.length == 0) continue;
        
        for (let pos of poses) {
            arr.push({
                pos: pos,
                rule: rule
            });
        }
    }
    
    if (arr.length == 0 || count==5) return [str, true];
    
    let randomIndex = Math.floor(Math.random() * arr.length);
    let el = arr[randomIndex];
    
    // Правильное замещение подстроки
    return [str.substring(0, el.pos) + el.rule.to + str.substring(el.pos + el.rule.from.length), false];
}

function randomNormalize1(str1){
    let countRules=0
    while(true){
        let [str, res]=randomReduce1(str1, countRules)
        countRules++
        if (res) return str
        str1=str
    }
}

function meta(str1){
    let str=randomNormalize1(str1)
    return firstCompare(str1, str) && secondCompare(str1, str)
}
function testing(){
    for (let i=0;i<100; i++){
        let testString = generateRandomString(20);
        let reducedString = randomNormalize(testString);
        if (i%10==0) console.log(i)
        if (!((fuzz(testString, reducedString) || (fuzz(reducedString, testString))) && meta(testString))){
            if (testString.length>reducedString.length || (testString.length==reducedString.length && testString>reducedString)){
                console.log(testString, reducedString)      
            } else{
                console.log(reducedString, testString)
            }
        } 
    }
}
testing()