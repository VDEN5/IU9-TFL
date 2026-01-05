function countSubstrings(w, sub) {
    let count = 0;
    const len = sub.length;
    for (let i = 0; i <= w.length - len; i++) {
        if (w.substring(i, i + len) === sub) {
            count++;
        }
    }
    return count;
}

function isSquare(s) {
    if (s.length % 2 !== 0) return false;
    const half = s.length / 2;
    return s.substring(0, half) === s.substring(half);
}

function countSquares(w) {
    let count = 0;
    // Перебираем все подстроки
    for (let i = 0; i < w.length; i++) {
        for (let j = i + 1; j <= w.length; j++) {
            const sub = w.substring(i, j);
            if (isSquare(sub)) {
                count++;
            }
        }
    }
    return count;
}

function isInLanguage(w) {
    const squaresCount = countSquares(w);
    const abCount = countSubstrings(w, 'ab');
    return squaresCount < abCount;
}

// Генерация всех слов над {a, b} длины до maxLength
function generateAllWords(maxLength) {
    const words = [''];
    const alphabet = ['a', 'b'];
    
    for (let len = 1; len <= maxLength; len++) {
        const newWords = [];
        for (const word of words) {
            if (word.length === len - 1) {
                for (const letter of alphabet) {
                    newWords.push(word + letter);
                }
            }
        }
        words.push(...newWords);
    }
    
    return words.filter(w => w.length > 0);
}

// Основная функция
function analyzeLanguage(maxLength) {
    console.log(`Слова над {a, b} длины до ${maxLength}, принадлежащие языку:\n`);
    console.log('(C_square < C_ab)\n');
    
    const allWords = generateAllWords(maxLength);
    const wordsInLanguage = [];
    
    for (const word of allWords) {
        const squares = countSquares(word);
        const ab = countSubstrings(word, 'ab');
        
        if (squares < ab) {
            wordsInLanguage.push({
                word: word,
                squares: squares,
                ab: ab
            });
        }
    }
    
    // Сортируем по длине слова
    wordsInLanguage.sort((a, b) => a.word.length - b.word.length);
    
    // Выводим результаты
    for (const {word, squares, ab} of wordsInLanguage) {
        console.log(`"${word}": квадратов=${squares}, ab=${ab} (${squares} < ${ab})`);
    }
    
    console.log(`\nВсего слов в языке: ${wordsInLanguage.length}`);
    
    return wordsInLanguage;
}

// Анализируем слова длины до 6
const results = analyzeLanguage(16);

// Дополнительно: выводим только слова для краткости
console.log('\n--- Только слова в языке ---');
console.log(results.map(item => `"${item.word}"`).join(', '));