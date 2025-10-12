import System.Random
import Data.List (isInfixOf, tails, isPrefixOf, elemIndex)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Text.Regex.Posix ((=~))
import Control.Monad (when)
import System.Timeout (timeout)

-- Генерация случайной строки
generateRandomString :: Int -> IO String
generateRandomString n = do
    g <- newStdGen
    let chars = "ab"
    return $ take n $ map (chars !!) $ randomRs (0, length chars - 1) g

-- Поиск всех вхождений подстроки
findAllSubstringPositions :: String -> String -> [Int]
findAllSubstringPositions pattern text =
    [i | (i, tail) <- zip [0..] (tails text), pattern isPrefixOf tail]

-- Правила для первой системы
rules :: [(String, String)]
rules = [
    ("aaaa", "a"),
    ("aaab", "b"),
    ("bbba", "ba"),
    ("bbbb", "bb"),
    ("ababb", "babb"),
    ("bba", "baaba"),
    ("bbb", "baabb"),
    ("bbaaa", "baab"),
    ("bbaab", "baa"),
    ("bbabb", "abab"),
    ("baba", "baa"),
    ("babbaa", "babba"),
    ("babbab", "abb")
    ]

-- Случайное сокращение для первой системы с ограничением
randomReduce :: String -> Int -> IO (String, Bool)
randomReduce str count = do
    let possibleChanges = do
            (pattern, replacement) <- rules
            pos <- findAllSubstringPositions pattern str
            return (pos, pattern, replacement)
    
    if null possibleChanges || count >= 6
        then return (str, True)
        else do
            randomIndex <- randomRIO (0, length possibleChanges - 1)
            let (pos, pattern, replacement) = possibleChanges !! randomIndex
            let newStr = take pos str ++ replacement ++ drop (pos + length pattern) str
            return (newStr, False)

-- Нормализация для первой системы с таймаутом
randomNormalize :: String -> IO String
randomNormalize str = do
    result <- timeout (5 * 1000000) (normalize str 0)
    case result of
        Just res -> return res
        Nothing -> return str
  where
    normalize str' count = do
        (newStr, done) <- randomReduce str' count
        if done
            then return newStr
            else normalize newStr (count + 1)

-- Правила для второй системы
rules1 :: [(String, String)]
rules1 = [
    ("aaaa", "a"),
    ("aaab", "b"),
    ("bab", "baa"),
    ("baa", "abb"),
    ("aba", "bb"),
    ("bb", "ba")
    ]

-- Детерминированная нормализация
deterministicNormalize :: String -> String
deterministicNormalize str = go str
  where
    go current =
      case findApplicableRule current of
        Just (pattern, replacement, pos) ->
          let newStr = take pos current ++ replacement ++ drop (pos + length pattern) current
          in go newStr
        Nothing -> current
    
    findApplicableRule str = 
      let matches = do
            (pattern, replacement) <- rules1
            pos <- findAllSubstringPositions pattern str
            return (pattern, replacement, pos)
      in case matches of
          [] -> Nothing
          (first:_) -> Just first

-- Эквивалентная версия fuzz с BFS вместо DFS (чтобы без бесконечной рекурсии было)
fuzz :: String -> String -> IO Bool
fuzz str1 str2 = do
    let normStr1 = deterministicNormalize str1
    
    -- Нормализуем str2 и записываем путь
    let (way, reachedTarget) = normalizeWithWay str2 normStr1 []
    
    if reachedTarget
        then return True  -- Успешно нормализовались к одному результату
        else if null way
            then return False  -- Не смогли нормализовать str2
            else do
                -- Пытаемся найти обратный путь через BFS
                let reversedWay = reverse way
                result <- bfsSearch str2 reversedWay Set.empty (Seq.singleton str2) 50
                return result
  where
    -- Нормализация с записью пути (ограниченная)
    normalizeWithWay :: String -> String -> [Int] -> ([Int], Bool)
    normalizeWithWay current target currentWay =
        if current == target
            then (currentWay, True)  -- Достигли цели
            else
                case findApplicableRule current of
                    Just (pattern, replacement, pos) ->
                        let newStr = take pos current ++ replacement ++ drop (pos + length pattern) current
                            ruleIdx = case elemIndex (pattern, replacement) rules1 of
                                        Just idx -> idx
                                        Nothing -> -1
                            newWay = currentWay ++ [ruleIdx]
                        in if length newWay > 20  -- Ограничение глубины
                            then (newWay, False)  -- Прерываем если слишком глубоко
                            else normalizeWithWay newStr target newWay
                    Nothing -> (currentWay, False)  -- Не можем применить правила
    
    findApplicableRule str = 
      let matches = do
            (pattern, replacement) <- rules1
            pos <- findAllSubstringPositions pattern str
            return (pattern, replacement, pos)
      in case matches of
          [] -> Nothing
          (first:_) -> Just first
    
    -- BFS поиск с ограничением
    bfsSearch :: String -> [Int] -> Set.Set String -> Seq.Seq String -> Int -> IO Bool
    bfsSearch _ _ _ _ 0 = return False  -- Превышен лимит итераций
    bfsSearch current way visited queue steps
        | Seq.null queue = return False
        | otherwise =
            let (headStr Seq.:<| restQueue) = queue
            in if headStr == current
                then return True  -- Нашли путь
                else if Set.member headStr visited
                    then bfsSearch current way visited restQueue (steps - 1)
                    else do
                        let currentIndex = length way - steps
                        if currentIndex >= length way
                            then return False
                            else do
                                let ruleIdx = way !! currentIndex
                                if ruleIdx < 0 || ruleIdx >= length rules1
                                    then return False
                                    else do
                                        let (pattern, replacement) = rules1 !! ruleIdx
                                        let positions = findAllSubstringPositions replacement headStr
                                        let newStrings = map (\pos -> 
                                                take pos headStr ++ pattern ++ drop (pos + length replacement) headStr
                                             ) positions
                                        let unvisited = filter (\s -> not (Set.member s visited)) newStrings
                                        let newVisited = Set.union visited (Set.fromList unvisited)
                                        let newQueue = restQueue Seq.>< Seq.fromList unvisited
                                        bfsSearch current way newVisited newQueue (steps - 1)

-- Случайная нормализация с ограничением
randomSimpleNormalizeLimited :: Int -> String -> IO String
randomSimpleNormalizeLimited maxSteps str = go str 0
  where
    go current step
      | step >= maxSteps = return current
      | otherwise = do
          let allApplications = do
                (pattern, replacement) <- rules1
                pos <- findAllSubstringPositions pattern current
                return (pos, pattern, replacement)
          
          if null allApplications
            then return current
            else do
              randomIndex <- randomRIO (0, length allApplications - 1)
              let (pos, pattern, replacement) = allApplications !! randomIndex
              let newStr = take pos current ++ replacement ++ drop (pos + length pattern) current
              go newStr (step + 1)

-- Первое сравнение
firstCompare :: String -> String -> Bool
firstCompare str1 str2 = ('b' elem str1) == ('b' elem str2)

-- Второе сравнение (регулярные выражения)
secondCompare :: String -> String -> Bool
secondCompare str1 str2 =
    let regexPatterns = [
            "^$",                          -- 1. пустое слово
            "^a(aaa)*$",                   -- 2. a(aaa)*
            "^(b|aaa(aaa)*b)$",            -- 3. b|aaa(aaa)*b  
            "^aa(aaa)*$",                  -- 4. aa(aaa)*
            "^a(aaa)*b$",                  -- 5. a(aaa)*b
            "^(ba|aaa(aaa)*ba|bb|aaa(aaa)*bb|a(aaa)*ba|a(aaa)*bb|aa(aaa)*ba|aa(aaa)*bb)(a|b)*$", -- 6. сложная
            "^aaa(aaa)*$",                 -- 7. aaa(aaa)*
            "^aa(aaa)*b$"                  -- 8. aa(aaa)*b
            ]
        getClass str = 
            case filter (\p -> str =~ p) regexPatterns of
                [] -> "unknown"
                (p:_) -> p
    in getClass str1 == getClass str2

-- Третье сравнение
thirdCompare :: String -> String -> Bool
thirdCompare str1 str2 = classifyWord str1 == classifyWord str2
  where
    classifyWord :: String -> String
    classifyWord "" = "L_ε"
    classifyWord str =
        case elemIndex 'b' str of
            Nothing ->
                case length str mod 3 of
                    1 -> "L_a"
                    2 -> "L_aa"
                    0 -> "L_aaa"
            Just firstBIndex ->
                if firstBIndex < length str - 1
                    then "L_ba"
                    else case firstBIndex mod 3 of
                        0 -> "L_b"
                        1 -> "L_ab"
                        2 -> "L_aab"

-- Четвертое сравнение
fourthCompare :: String -> String -> Bool
fourthCompare str1 str2
    | hasBaOrBb str1 && hasBaOrBb str2 = True
    | not (hasBaOrBb str1) && not (hasBaOrBb str2) =
        let countA1 = length $ filter (== 'a') str1
            countA2 = length $ filter (== 'a') str2
        in (countA1 mod 3) == (countA2 mod 3)
    | otherwise = False
  where
    hasBaOrBb str = "ba" isInfixOf str || "bb" isInfixOf str

randomNormalize1 :: String -> IO String
randomNormalize1 str = do
    result <- timeout (3 * 1000000) (randomSimpleNormalizeLimited 20 str)
    case result of
        Just res -> return res
        Nothing -> return str

-- Мета-функция проверки
meta :: String -> IO Bool
meta str1 = do
    str2 <- randomNormalize1 str1
    let comparisons = firstCompare str1 str2 
                  && secondCompare str1 str2 
                  && thirdCompare str1 str2 
                  && fourthCompare str1 str2
    return comparisons

-- Основная функция тестирования
testing :: IO ()
testing = testLoop 0
  where
    testLoop :: Int -> IO ()
    testLoop i
        | i >= 50 = do
            putStrLn "Тестирование завершено!"
            return ()
        | otherwise = do
            when (i mod 10 == 0) $ putStrLn $ "Прогресс: " ++ show i ++ "/50"
            
            testString <- generateRandomString 12
            reducedString <- randomNormalize testString
            
            -- Таймаут для каждого теста
            testResult <- timeout (1 * 1000000) $ do
                fuzzResult <- fuzz testString reducedString
                metaResult <- meta testString
                return (fuzzResult && metaResult)
            
            case testResult of
                Just True -> testLoop (i + 1)
                Just False -> do
                    putStrLn "Найдено несоответствие:"
                    putStrLn $ "  Исходная: " ++ testString
                    putStrLn $ "  Сокращенная: " ++ reducedString
                    putStrLn "---"
                    testLoop (i + 1)
                Nothing -> do
                    putStrLn $ "Таймаут на тесте " ++ show i
                    testLoop (i + 1)

-- Главная функция
main :: IO ()
main = do
    putStrLn "Запуск тестирования с эквивалентным фаззингом..."
    testing
    putStrLn "Готово!"
