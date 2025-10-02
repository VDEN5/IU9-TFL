let rules = [
    {from: "aaaa", to: "a"},
    {from: "aaab", to: "b"},
    {from: "bbba", to: "ba"},
    {from: "bbbb", to: "bb"},
    {from: "ababb", to: "babb"},
    {from: "baaba", to: "bba"},
    {from: "baabb", to: "bbb"},
    {from: "bbaaa", to: "baab"},
    {from: "bbaab", to: "baa"},
    {from: "bbabb", to: "abab"},
    {from: "baba", to: "baa"},
    {from: "babbaa", to: "babba"},
    {from: "babbab", to: "abb"},
    //1
    {from: "babbb", to: "bab"},
    {from: "babbb", to: "abb"},
    {from: "babbb", to: "baaa"},
    {from: "babb", to: "bab"},
    {from: "bab", to: "abb"},
    {from: "baaa", to: "bab"},
    {from: "baab", to: "abb"},
    {from: "abb", to: "bb"},
    //2
    {from: "abaa", to: "baa"},
    {from: "baa", to: "bb"},
    {from: "bbaa", to: "bb"},
    {from: "bbaa", to: "baa"},
    {from: "bba", to: "aba"},
    {from: "baa", to: "ba"},
    {from: "bba", to: "baa"},
    {from: "bbb", to: "bb"},
    {from: "aba", to: "ba"},
    {from: "bb", to: "ba"},
]

function getAllIntersect(str1, str2){
    let res = []
    for (let i = 1; i < str1.length; i++){
        if (str1.substring(str1.length - i, str1.length) == str2.substring(0, i)){
            res.push(str1.substring(0, str1.length - i) + str2.substring(0, i) + str2.substring(i))
        }
    }
    return res
}

function getFirstEl(str, rule) {
    return rule.to + str.substring(rule.from.length)
}

function getLastEl(str, rule){
    return str.substring(0, str.length - rule.from.length) + rule.to
}
function getMidEl(str,rule){
    let pos=str.indexOf(rule.from)
    return str.substring(0, pos)+rule.to+str.substring(pos+rule.from.length)
}
function getAllApplications(str, rule) {
    let results = [];
    let pos = 0;
    
    // Ищем все вхождения rule.from в строке
    while ((pos = str.indexOf(rule.from, pos)) !== -1) {
        // Применяем правило в текущей позиции
        let newStr = str.substring(0, pos) + rule.to + str.substring(pos + rule.from.length);
        results.push(newStr);
        pos++; // Переходим к следующей возможной позиции
    }
    
    return results;
}

function normalize(str){
    let current = str
    
    while (true) {
        let foundRule = false
        
        // Проверяем все правила
        for (let rule of rules) {
            // Если находим подстроку rule.from в current
            if (current.includes(rule.from)) {
                // Заменяем первое вхождение
                current = current.replace(rule.from, rule.to)
                foundRule = true
                break // выходим из цикла for и начинаем проверку заново
            }
        }
        
        // Если ни одно правило не применилось - строка в нормальной форме
        if (!foundRule) {
            break
        }
    }
    
    return current
}
function compare(str1, str2){
    if (str1.length>str2.length) return [str1,str2]
    if (str2.length>str1.length) return [str2,str1]
    if(str1<str2)return [str2,str1]
    return [str1,str2]
}
function getRes(ind1, ind2){
    let res=[]
    let rule1=rules[ind1]
    let rule2=rules[ind2]
    if (rule2.from==rule1.from){
        if (normalize(rule1.to)!=normalize(rule2.to)) res.push(normalize(rule1.to), normalize(rule2.to))
    }
    if (rule1.from.includes(rule2.from) && rule2.from.length<rule1.from.length){
        let arr=getAllApplications(rule1.from, rule2)
        for (let a1 of arr){
            let a2=rule1.to
        let na1=normalize(a1),na2=normalize(a2)
        if (na1!=na2) res.push(compare(na1,na2))
        }
    }
    if (rule2.from.includes(rule1.from) && rule1.from.length<rule2.from.length){
        let arr=getAllApplications(rule2.from, rule1)
        for (let a1 of arr){
            let a2=rule2.to
        let na1=normalize(a1),na2=normalize(a2)
        if (na1!=na2) res.push(compare(na1,na2))
        }
    }
    let strs=getAllIntersect(rule1.from, rule2.from)
    for (let str of strs){
        let a1=getFirstEl(str, rule1)
    let a2=getLastEl(str, rule2)
    let na1=normalize(a1)
    let na2=normalize(a2)
    if (na1!=na2) res.push(compare(na1,na2))
    }
if (res.length!=0) return res
}
for(let i=0;i<rules.length;i++){
    for (let j=i;j<rules.length;j++){
        let a1=getRes(j,i), a2=getRes(i,j)
    if (a1 && a1.length>0) console.log(a1)
    if (a2 && a2.length>0) console.log(a2)
    }
if (i%10==0)console.log(i)
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
function allForms(str){
    let res=new Set()
    function dfs(tempStr){
        if (res.has(tempStr)){
            return
        }
        res.add(tempStr)
        for (let rule of rules){
            let poses=findAllSubstringPositions(tempStr, rule.from)
            for (let pos of poses){
                dfs(tempStr.substring(0, pos) + rule.to + tempStr.substring(pos + rule.from.length))
            }
        }
    }
    dfs(str)
    return res
}
//console.log(allForms("ababbbb"))//baaba
let rules1 = [
    {from: "aaaa", to: "a"},
    {from: "aaab", to: "b"},
    {from: "bbba", to: "bbaa"},
    {from: "bbbb", to: "bbba"},
    {from: "ababb", to: "bbbb"},
    {from: "baaba", to: "bbbb"},
    {from: "baabb", to: "baaba"},
    {from: "bbaaa", to: "babbb"},
    {from: "bbaab", to: "babbb"},
    {from: "bbabb", to: "abab"},
    {from: "baba", to: "baaa"},
    {from: "babbaa", to: "babba"},
    {from: "babbab", to: "babbb"},
    //1
    {from: "babbb", to: "baabb"},
    {from: "babb", to: "baab"},
    {from: "bab", to: "baa"},
    {from: "baaa", to: "abaa"},
    {from: "baab", to: "baaa"},
    {from: "abb", to: "aba"},
    //2
    {from: "abaa", to: "bbb"},
    {from: "baa", to: "abb"},
    {from: "bbaa", to: "babb"},
    {from: "bbaa", to: "baba"},
    {from: "bba", to: "bab"},
    {from: "bbb", to: "bba"},
    {from: "aba", to: "bb"},
    {from: "bb", to: "ba"},
]
let arr=[]
for (let r of rules1){
    if (r.to=="abaa"){
        arr.push(r)
    }
}
arr.sort((a, b) => {
    // По длине (от большей к меньшей)
    if (a.from.length > b.from.length) return -1;
    if (a.from.length < b.from.length) return 1;
    
    // При равной длине - лексикографически
    if (a.from < b.from) return 1;
    if (a.from > b.from) return -1;
    return 0;
});
console.log(arr)