function f(str, pos1){
    let pos=str.indexOf("bbSbb", pos1)
    if (pos==-1) return {newStr: str, newPos:pos, isFinal: true}
    pos+=2
    let ri=pos+3,ri_a=0
    while(true){
        if (ri+2<str.length && str[ri]=="a" && str[ri+1]=="b" && str[ri+2]=="b"){
            ri+=3
            ri_a++
        } else{
            break
        }
    }
    let le=pos-3,le_a=0
    while(true){
        if (le-2>=0 && str[le]=="a" && str[le-1]=="b" && str[le-2]=="b"){
            le-=3
            le_a++
        } else{
            break
        }
    }
    if (Math.ceil(Math.log2(le_a + 1))<ri_a){//Math.ceil(Math.log2(le_a + 1))<ri_a это условие будет полностью эквивалентно аттрибутному, сам проверял и доказал
        return {newStr: str.slice(0,le+1)+"S"+str.slice(ri), newPos:0, isFinal: true}
    }
    return {newStr: str, newPos:pos, isFinal: false}
}
function parse(str){
    str=str.replaceAll("aaa", "S")
    while (true){
        while (str.indexOf("SbS")!=-1)
            str=str.replaceAll("SbS", "S")
        let pos=0
        while(true){
            let res=f(str, pos)
            if (res.isFinal){
                str=res.newStr
                if (res.newPos==-1) return str=="S"
                break
            }
            pos=res.newPos
        }
    }
}
function generateRandomWord(length) {
    let word = '';
    for (let i = 0; i < length; i++) {
        word += Math.random() < 0.5 ? 'a' : 'b';
    }
    return word;
}
const memoParseS = new Map();  // (start, end) -> boolean
const memoParseT = new Map();  // (start, end) -> number[]

function parseT(str, start, end) {
    const key = `${start},${end}`;
    
    if (memoParseT.has(key)) {
        return memoParseT.get(key);
    }
    
    const attrs = new Set();
    
    // Правило T -> b b (атрибут 0)
    if (end - start === 2 && str.slice(start, end) === 'bb') {
        attrs.add(0);
    }
    
    // Правило T -> T a T (атрибут max(a1, a2) + 1)
    for (let k = start + 1; k < end - 1; k++) {
        if (str[k] === 'a') {
            const attrs1 = parseT(str, start, k);
            const attrs2 = parseT(str, k + 1, end);
            
            for (const a1 of attrs1) {
                for (const a2 of attrs2) {
                    attrs.add(Math.max(a1, a2) + 1);
                }
            }
        }
    }
    
    const result = Array.from(attrs);
    memoParseT.set(key, result);
    return result;
}

function parseS(str, start, end) {
    const key = `${start},${end}`;
    
    if (memoParseS.has(key)) {
        return memoParseS.get(key);
    }
    
    // Правило S -> a a a
    if (end - start === 3 && str.slice(start, end) === 'aaa') {
        memoParseS.set(key, true);
        return true;
    }
    
    // Правило S -> S b S
    for (let k = start + 1; k < end - 1; k++) {
        if (str[k] === 'b') {
            if (parseS(str, start, k) && parseS(str, k + 1, end)) {
                memoParseS.set(key, true);
                return true;
            }
        }
    }
    
    // Правило S -> T S T с условием T1.a < T2.a
    for (let i = start + 1; i < end; i++) {
        for (let j = i; j < end; j++) {
            const attrs1 = parseT(str, start, i);
            if (attrs1.length === 0) continue;
            
            if (!parseS(str, i, j)) continue;
            
            const attrs2 = parseT(str, j, end);
            if (attrs2.length === 0) continue;        
            // Проверка условия: существует пара a1 < a2
            if (attrs1.some(a1 => attrs2.some(a2 => a1 < a2))) {
                memoParseS.set(key, true);
                return true;
            }
        }
    }
    
    memoParseS.set(key, false);
    return false;
}

function isStringInLanguage(str) {
    memoParseS.clear();
    memoParseT.clear();
    
    return parseS(str, 0, str.length);
}
for (let i=0;i<300;i++){
    let word=generateRandomWord(30)
    let res1=isStringInLanguage(word)
    let res2=parse(word)
    let res=(res1&&res2) || (!res1&&!res2)
    console.log(res,res1, res2)
}
function test(){
    let word1="bbaaabbabbbbbabbaaabbabbabb"//27
    let start = performance.now()
    let result = parse(word1)
    let time = performance.now() - start
    let start1 = performance.now()
    let result1 = isStringInLanguage(word1)
    let time1 = performance.now() - start1
    console.log(result, result1, time, time1)

    word1="bbabbbbaaabbabbbbbabbaaabbabbabbbbabbabb"//40
    start = performance.now()
    result = parse(word1)
    time = performance.now() - start
    start1 = performance.now()
    result1 = isStringInLanguage(word1)
    time1 = performance.now() - start1
    console.log(result, result1, time, time1)

    word1="bbabbbbaaabbabbbbbabbaaabbabbabbbbabbabbbbbabbaaabbabbabb"//57
    start = performance.now()
    result = parse(word1)
    time = performance.now() - start
    start1 = performance.now()
    result1 = isStringInLanguage(word1)
    time1 = performance.now() - start1
    console.log(result, result1, time, time1)

    word1="bbabbaaabbabbabbbbbabbbbaaabbabbbbbabbaaabbabbabbbbabbabbbbbabbaaabbabbabb"//74
    start = performance.now()
    result = parse(word1)
    time = performance.now() - start
    start1 = performance.now()
    result1 = isStringInLanguage(word1)
    time1 = performance.now() - start1
    console.log(result, result1, time, time1)

    word1="bbabbabbbbabbaaabbabbabbbbbabbbbaaabbabbbbbabbaaabbabbabbbbabbabbbbbabbaaabbabbabbbbabbabbabb"//93
    start = performance.now()
    result = parse(word1)
    time = performance.now() - start
    start1 = performance.now()
    result1 = isStringInLanguage(word1)
    time1 = performance.now() - start1
    console.log(result, result1, time, time1)

    word1="bbaaabbbabbbbbbabbabaabbabbabb"//30
    start = performance.now()
    result = parse(word1)
    time = performance.now() - start
    start1 = performance.now()
    result1 = isStringInLanguage(word1)
    time1 = performance.now() - start1
    console.log(result, result1, time, time1)

    word1="bbabbabbaaabbabbbabbabbaaabbabbabbbbabbabb"//42
    start = performance.now()
    result = parse(word1)
    time = performance.now() - start
    start1 = performance.now()
    result1 = isStringInLanguage(word1)
    time1 = performance.now() - start1
    console.log(result, result1, time, time1)

    word1="bbabbabbaaabbabbbbbabbaaabbabbabbbbabbabbabbbabbaaabbabbabb"//59
    start = performance.now()
    result = parse(word1)
    time = performance.now() - start
    start1 = performance.now()
    result1 = isStringInLanguage(word1)
    time1 = performance.now() - start1
    console.log(result, result1, time, time1)

    word1="bbabbaaabbabbabbbbbabbabbaaabbabbbbbabbaaabbabbabbbbabbabbbbbabbaaabbabbabb"//75
    start = performance.now()
    result = parse(word1)
    time = performance.now() - start
    start1 = performance.now()
    result1 = isStringInLanguage(word1)
    time1 = performance.now() - start1
    console.log(result, result1, time, time1)

    word1="bbabbabbabbabbbbabbaaabbabbabbbbbabbbbaaabbabbbbbabbaaabbabbabbbbabbabbbbbabbaaabbabbabbbbabbabbabb"//99
    start = performance.now()
    result = parse(word1)
    time = performance.now() - start
    start1 = performance.now()
    result1 = isStringInLanguage(word1)
    time1 = performance.now() - start1
    console.log(result, result1, time, time1)
}
