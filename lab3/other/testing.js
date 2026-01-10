function cykParse(word, grammar, startSymbols) {
    if (typeof startSymbols === 'string') {
        startSymbols = [startSymbols];
    }
    
    const n = word.length;
    if (n === 0) {
        return startSymbols.some(s => 
            grammar.some(rule => rule.lhs === s && rule.rhs.length === 0)
        );
    }

    const dp = Array.from({ length: n }, () => Array(n + 1).fill().map(() => new Set()));

    for (let i = 0; i < n; i++) {
        const symbol = word[i];
        for (const rule of grammar) {
            if (rule.rhs.length === 1 && rule.rhs[0] === symbol) {
                dp[i][1].add(rule.lhs);
            }
        }
    }

    for (let l = 2; l <= n; l++) {
        for (let i = 0; i <= n - l; i++) {
            for (let k = 1; k < l; k++) {
                const leftSet = dp[i][k];
                const rightSet = dp[i + k][l - k];
                if (leftSet.size > 0 && rightSet.size > 0) {
                    for (const rule of grammar) {
                        if (rule.rhs.length === 2) {
                            const [B, C] = rule.rhs;
                            if (leftSet.has(B) && rightSet.has(C)) {
                                dp[i][l].add(rule.lhs);
                            }
                        }
                    }
                }
            }
        }
    }

    return startSymbols.some(start => dp[0][n].has(start));
}

function generateRandomWord(length) {
    let word = '';
    for (let i = 0; i < length; i++) {
        word += Math.random() < 0.5 ? 'a' : 'b';
    }
    return word;
}

// 1
const grammar1 = [
    { lhs: 'S', rhs: ['T', 'S3'] },
    { lhs: 'S3', rhs: ['S', 'T'] },
    { lhs: 'S', rhs: ['S', 'S4'] },
    { lhs: 'S4', rhs: ['B1', 'S'] },
    { lhs: 'S', rhs: ['A1', 'S5'] },
    { lhs: 'S5', rhs: ['A2', 'A3'] },
    { lhs: 'T', rhs: ['B1', 'B2'] },
    { lhs: 'T', rhs: ['T', 'T1'] },
    { lhs: 'T1', rhs: ['A1', 'T'] },
    { lhs: 'A1', rhs: ['a'] },
    { lhs: 'A2', rhs: ['a'] },
    { lhs: 'A3', rhs: ['a'] },
    { lhs: 'B1', rhs: ['b'] },
    { lhs: 'B2', rhs: ['b'] }
];
const startSymbols1 = 'S';

// 2
const grammar2 = [
    { lhs: 'A', rhs: ['a'] },
    { lhs: 'B', rhs: ['b'] },
    { lhs: 'AA', rhs: ['A', 'A'] },
    
    { lhs: 'T03', rhs: ['T03', 'X1'] },
    { lhs: 'X1', rhs: ['A', 'T13'] },
    { lhs: 'T13', rhs: ['T13', 'X2'] },
    { lhs: 'X2', rhs: ['A', 'T13'] },
    { lhs: 'T33', rhs: ['T33', 'X3'] },
    { lhs: 'X3', rhs: ['A', 'T13'] },
    { lhs: 'T56', rhs: ['T56', 'X4'] },
    { lhs: 'X4', rhs: ['A', 'T46'] },
    { lhs: 'T46', rhs: ['T46', 'X5'] },
    { lhs: 'X5', rhs: ['A', 'T46'] },
    { lhs: 'T66', rhs: ['T66', 'X6'] },
    { lhs: 'X6', rhs: ['A', 'T46'] },
    
    { lhs: 'T03', rhs: ['B', 'B'] },
    { lhs: 'T13', rhs: ['B', 'B'] },
    { lhs: 'T33', rhs: ['B', 'B'] },
    { lhs: 'T56', rhs: ['B', 'B'] },
    { lhs: 'T46', rhs: ['B', 'B'] },
    { lhs: 'T66', rhs: ['B', 'B'] },
    
    { lhs: 'S06', rhs: ['T03', 'Z1'] },
    { lhs: 'Z1', rhs: ['S35', 'T56'] },
    { lhs: 'S06', rhs: ['T03', 'Z2'] },
    { lhs: 'Z2', rhs: ['S36', 'T66'] },
    { lhs: 'S36', rhs: ['T33', 'Z3'] },
    { lhs: 'Z3', rhs: ['S35', 'T56'] },
    { lhs: 'S36', rhs: ['T33', 'Z4'] },
    { lhs: 'Z4', rhs: ['S36', 'T66'] },
    
    { lhs: 'S05', rhs: ['S05', 'Y1'] },
    { lhs: 'Y1', rhs: ['B', 'S05'] },
    { lhs: 'S05', rhs: ['S06', 'Y2'] },
    { lhs: 'Y2', rhs: ['B', 'S05'] },
    { lhs: 'S06', rhs: ['S05', 'Y3'] },
    { lhs: 'Y3', rhs: ['B', 'S06'] },
    { lhs: 'S06', rhs: ['S06', 'Y4'] },
    { lhs: 'Y4', rhs: ['B', 'S06'] },
    { lhs: 'S35', rhs: ['S35', 'Y5'] },
    { lhs: 'Y5', rhs: ['B', 'S05'] },
    { lhs: 'S35', rhs: ['S36', 'Y6'] },
    { lhs: 'Y6', rhs: ['B', 'S05'] },
    { lhs: 'S36', rhs: ['S35', 'Y7'] },
    { lhs: 'Y7', rhs: ['B', 'S06'] },
    { lhs: 'S36', rhs: ['S36', 'Y8'] },
    { lhs: 'Y8', rhs: ['B', 'S06'] },
    
    { lhs: 'S05', rhs: ['A', 'AA'] },
    { lhs: 'S35', rhs: ['A', 'AA'] },
];
const startSymbols2 = ['S05', 'S06'];

// 3
const grammar3 = [
    { lhs: 'A', rhs: ['a'] },
    { lhs: 'B', rhs: ['b'] },
    { lhs: 'AA', rhs: ['A', 'A'] },
    
    { lhs: 'T01', rhs: ['T01', 'X1'] },
    { lhs: 'X1', rhs: ['A', 'T41'] },
    { lhs: 'T41', rhs: ['T41', 'X2'] },
    { lhs: 'X2', rhs: ['A', 'T41'] },
    { lhs: 'T21', rhs: ['T21', 'X3'] },
    { lhs: 'X3', rhs: ['A', 'T41'] },
    { lhs: 'T31', rhs: ['T31', 'X4'] },
    { lhs: 'X4', rhs: ['A', 'T41'] },
    
    { lhs: 'T01', rhs: ['B', 'B'] },
    { lhs: 'T41', rhs: ['B', 'B'] },
    { lhs: 'T21', rhs: ['B', 'B'] },
    { lhs: 'T31', rhs: ['B', 'B'] },
    
    { lhs: 'S01', rhs: ['T01', 'Z1'] },
    { lhs: 'Z1', rhs: ['S13', 'T31'] },
    { lhs: 'S01', rhs: ['T01', 'Z2'] },
    { lhs: 'Z2', rhs: ['S12', 'T21'] },
    { lhs: 'S31', rhs: ['T31', 'Z3'] },
    { lhs: 'Z3', rhs: ['S13', 'T31'] },
    { lhs: 'S31', rhs: ['T31', 'Z4'] },
    { lhs: 'Z4', rhs: ['S12', 'T21'] },
    
    { lhs: 'S01', rhs: ['S02', 'Y1'] },
    { lhs: 'Y1', rhs: ['B', 'S31'] },
    { lhs: 'S01', rhs: ['S01', 'Y2'] },
    { lhs: 'Y2', rhs: ['B', 'S31'] },
    { lhs: 'S02', rhs: ['S02', 'Y3'] },
    { lhs: 'Y3', rhs: ['B', 'S32'] },
    { lhs: 'S02', rhs: ['S01', 'Y4'] },
    { lhs: 'Y4', rhs: ['B', 'S32'] },
    { lhs: 'S31', rhs: ['S32', 'Y5'] },
    { lhs: 'Y5', rhs: ['B', 'S31'] },
    { lhs: 'S31', rhs: ['S31', 'Y6'] },
    { lhs: 'Y6', rhs: ['B', 'S31'] },
    { lhs: 'S32', rhs: ['S32', 'Y7'] },
    { lhs: 'Y7', rhs: ['B', 'S32'] },
    { lhs: 'S32', rhs: ['S31', 'Y8'] },
    { lhs: 'Y8', rhs: ['B', 'S32'] },
    { lhs: 'S12', rhs: ['S12', 'Y9'] },
    { lhs: 'Y9', rhs: ['B', 'S32'] },
    { lhs: 'S12', rhs: ['S11', 'Y10'] },
    { lhs: 'Y10', rhs: ['B', 'S32'] },
    { lhs: 'S13', rhs: ['S12', 'Y11'] },
    { lhs: 'Y11', rhs: ['B', 'S33'] },
    { lhs: 'S13', rhs: ['S11', 'Y12'] },
    { lhs: 'Y12', rhs: ['B', 'S33'] },
    { lhs: 'S11', rhs: ['S12', 'Y13'] },
    { lhs: 'Y13', rhs: ['B', 'S31'] },
    { lhs: 'S11', rhs: ['S11', 'Y14'] },
    { lhs: 'Y14', rhs: ['B', 'S31'] },
    { lhs: 'S33', rhs: ['S32', 'Y15'] },
    { lhs: 'Y15', rhs: ['B', 'S33'] },
    { lhs: 'S33', rhs: ['S31', 'Y16'] },
    { lhs: 'Y16', rhs: ['B', 'S33'] },
    
    { lhs: 'S02', rhs: ['A', 'AA'] },
    { lhs: 'S12', rhs: ['A', 'AA'] },
    { lhs: 'S32', rhs: ['A', 'AA'] },
];
const startSymbols3 = ['S01', 'S02'];
for (let test = 0; test < 500; test++) {
    const word = generateRandomWord(50);
    const result1 = cykParse(word, grammar1, startSymbols1);
    const result2 = cykParse(word, grammar2, startSymbols2);
    const result3 = cykParse(word, grammar3, startSymbols3);
    const allAccept = result1 && result2 && result3;
    const allReject = !result1 && !result2 && !result3;
    if (!(allAccept || allReject)) console.log(word)
}