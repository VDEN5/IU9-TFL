import qualified Data.Map as M
import System.Random (randomRIO)
import Text.Regex.Posix ((=~))
import Control.Monad (when)

-- Генерация случайной строки
generateRandomString :: Int -> IO String
generateRandomString n = 
  sequence $ replicate n $ do
    idx <- randomRIO (0, 3)
    return $ "acde" !! idx

-- Детерминированный автомат 1
automat1 :: [M.Map Char Int]
automat1 = [
    M.fromList [('a',7), ('b',7), ('c',11), ('d',4), ('e',5)],    -- state 0
    M.fromList [('a',7), ('b',7), ('c',7), ('d',6), ('e',7)],     -- state 1
    M.fromList [('a',7), ('b',7), ('c',5), ('d',7), ('e',7)],     -- state 2
    M.fromList [('a',7), ('b',7), ('c',7), ('d',7), ('e',0)],     -- state 3
    M.fromList [('a',7), ('b',7), ('c',1), ('d',0), ('e',7)],     -- state 4
    M.fromList [('a',7), ('b',7), ('c',2), ('d',0), ('e',3)],     -- state 5
    M.fromList [('a',7), ('b',7), ('c',10), ('d',7), ('e',7)],    -- state 6
    M.fromList [('a',7), ('b',7), ('c',7), ('d',7), ('e',7)],     -- state 7 (trap)
    M.fromList [('a',7), ('b',7), ('c',12), ('d',8), ('e',5)],    -- state 8
    M.fromList [('a',7), ('b',7), ('c',13), ('d',8), ('e',9)],    -- state 9
    M.fromList [('a',7), ('b',7), ('c',14), ('d',8), ('e',9)],    -- state 10
    M.fromList [('a',17), ('b',7), ('c',15), ('d',15), ('e',15)], -- state 11
    M.fromList [('a',17), ('b',7), ('c',15), ('d',18), ('e',15)], -- state 12
    M.fromList [('a',17), ('b',7), ('c',16), ('d',15), ('e',15)], -- state 13
    M.fromList [('a',17), ('b',7), ('c',21), ('d',19), ('e',20)], -- state 14
    M.fromList [('a',7), ('b',7), ('c',7), ('d',7), ('e',7)],     -- state 15 (final)
    M.fromList [('a',7), ('b',7), ('c',2), ('d',0), ('e',3)],     -- state 16 (final)
    M.fromList [('a',7), ('b',7), ('c',0), ('d',7), ('e',7)],     -- state 17 (final)
    M.fromList [('a',7), ('b',7), ('c',10), ('d',7), ('e',7)],    -- state 18 (final)
    M.fromList [('a',7), ('b',7), ('c',12), ('d',8), ('e',5)],    -- state 19 (final)
    M.fromList [('a',7), ('b',7), ('c',13), ('d',8), ('e',9)],    -- state 20 (final)
    M.fromList [('a',17), ('b',7), ('c',21), ('d',19), ('e',20)]  -- state 21 (final)
  ]

getRes1 :: String -> Bool
getRes1 word = 
  let fin = getState word 0
  in fin >= 15 && fin <= 21
  where
    getState :: String -> Int -> Int
    getState [] curState = curState
    getState (c:cs) curState =
      let nextState = M.findWithDefault 7 c (automat1 !! curState)
      in getState cs nextState

-- Недетерминированный автомат 2
automat2 :: [M.Map Char [Int]]
automat2 = [
    M.fromList [('c', [3, 20]), ('e', [6]), ('d', [12, 1])],                    -- state 0
    M.fromList [('d', [2])],                                                    -- state 1
    M.fromList [('c', [3, 20]), ('e', [6]), ('d', [12, 1])],                    -- state 2
    M.fromList [('a', [4])],                                                    -- state 3
    M.fromList [('c', [5])],                                                    -- state 4
    M.fromList [('c', [3, 20]), ('e', [6]), ('d', [12, 1])],                    -- state 5
    M.fromList [('d', [9]), ('c', [7]), ('e', [10])],                           -- state 6
    M.fromList [('c', [8])],                                                    -- state 7
    M.fromList [('e', [10]), ('c', [7]), ('d', [9])],                           -- state 8
    M.fromList [('c', [3, 20]), ('e', [6]), ('d', [12, 1])],                    -- state 9
    M.fromList [('e', [11])],                                                   -- state 10
    M.fromList [('c', [3, 20]), ('e', [6]), ('d', [12, 1])],                    -- state 11
    M.fromList [('c', [13])],                                                   -- state 12
    M.fromList [('d', [14])],                                                   -- state 13
    M.fromList [('c', [16, 14])],                                               -- state 14
    M.empty,                                                                    -- state 15 (не используется)
    M.fromList [('c', [3, 20]), ('e', [6, 18]), ('d', [12, 1, 19])],            -- state 16
    M.empty,                                                                    -- state 17 (не используется)
    M.fromList [('c', [3, 20]), ('e', [6, 18]), ('d', [12, 1])],                -- state 18
    M.fromList [('c', [3, 20]), ('e', [6]), ('d', [12, 1, 19])],                -- state 19
    M.fromList [('a', [21]), ('c', [21]), ('e', [21]), ('d', [21])],            -- state 20
    M.empty                                                                     -- state 21 (final)
  ]

finalState2 :: Int
finalState2 = 21

getRes2 :: String -> Bool
getRes2 word = processWord word 0
  where
    processWord :: String -> Int -> Bool
    processWord [] currentState = currentState == finalState2
    processWord (c:cs) currentState =
      let nextStates = M.findWithDefault [] c (automat2 !! currentState)
      in any (\nextState -> processWord cs nextState) nextStates

-- Недетерминированный автомат 3
automat3 :: M.Map Int (M.Map Char [Int])
automat3 = M.fromList [
    (0, M.fromList [('a', [1, 15]), ('c', [1, 15]), ('d', [1, 15]), ('e', [1, 15])]),
    (1, M.fromList [('d', [2, 13]), ('e', [8, 13]), ('c', [11, 13]), ('a', [13])]),
    (2, M.fromList [('c', [3]), ('d', [1])]),
    (3, M.fromList [('d', [4])]),
    (4, M.fromList [('c', [5])]),
    (5, M.fromList [('c', [5]), ('d', [1, 6]), ('e', [1, 7])]),
    (6, M.fromList [('d', [1, 6])]),
    (7, M.fromList [('e', [1, 7])]),
    (8, M.fromList [('d', [1]), ('c', [9]), ('e', [10])]),
    (9, M.fromList [('c', [8])]),
    (10, M.fromList [('e', [1])]),
    (11, M.fromList [('a', [12])]),
    (12, M.fromList [('c', [1])]),
    (13, M.fromList [('a', [14]), ('c', [14]), ('d', [14]), ('e', [14])]),
    (14, M.empty), -- финальное состояние
    (15, M.fromList [('a', [15]), ('e', [15]), ('c', [15, 16]), ('d', [15])]),
    (16, M.fromList [('a', [17]), ('c', [17]), ('d', [17]), ('e', [17])]),
    (17, M.empty)  -- финальное состояние
  ]

getRes3 :: String -> Bool
getRes3 word = processWord word 0
  where
    processWord :: String -> Int -> Bool
    processWord [] currentState = currentState == 14 || currentState == 17
    processWord str@(c:cs) currentState =
      case M.lookup currentState automat3 of
        Nothing -> False
        Just transitions ->
          case M.lookup c transitions of
            Nothing -> False
            Just nextStates ->
              if currentState == 0  -- alternative state
                then all (\t -> processWord str t) nextStates
                else any (\t -> processWord cs t) nextStates

-- Регулярное выражение
getRes :: String -> Bool
getRes word = word =~ "^(dcdc*c(d*|e*e)|e(cc)*(ee|d)|cac|dd)*c(a|c|d|e)$"

-- Основная функция с выводом прогресса
main :: IO ()
main = do
  putStrLn "Начало тестирования автоматов..."
  let testCount = 1000
  words <- mapM (const $ generateRandomString 20) [1..testCount]
  
  let processWord :: Int -> String -> IO ()
      processWord i word = do
        -- Вывод прогресса каждые 100 итераций
        when (i `mod` 100 == 0) $ do
          putStrLn $ "=== Прогресс: обработано " ++ show i ++ " из " ++ show testCount ++ " итераций ==="
        
        let regexResult = getRes word
            res1 = getRes1 word
            res2 = getRes2 word
            res3 = getRes3 word
            allMatch = (regexResult && res1 && res2 && res3) || 
                      (not regexResult && not res1 && not res2 && not res3)
        
        -- Вывод принятых слов
        when (getRes word) $ 
          putStrLn $ "[" ++ show i ++ "] Принято: " ++ word
        
        -- Вывод несоответствий
        when (not allMatch) $ do
          putStrLn $ "!!! ВНИМАНИЕ: Несоответствие на итерации " ++ show i ++ " !!!"
          putStrLn $ "    Строка: " ++ word
          putStrLn $ "    Результаты: regex=" ++ show regexResult ++ 
                    ", res1=" ++ show res1 ++ ", res2=" ++ show res2 ++ 
                    ", res3=" ++ show res3
  
  -- Обрабатываем слова с номерами итераций
  sequence_ [processWord i word | (i, word) <- zip [1..testCount] words]
  
  putStrLn $ replicate 50 '='
  putStrLn "Тестирование завершено!"
  putStrLn $ "Всего протестировано: " ++ show testCount ++ " строк"

-- Дополнительная функция для тестирования конкретных строк
testSpecificWords :: IO ()
testSpecificWords = do
  let testWords = ["cac", "ddc", "dcdcccd", "eeccd", "invalid"]
  
  putStrLn "Тестирование конкретных строк:"
  mapM_ (\w -> do
    putStrLn $ "Строка: " ++ w
    putStrLn $ "  Regex: " ++ show (getRes w)
    putStrLn $ "  Автомат 1: " ++ show (getRes1 w)
    putStrLn $ "  Автомат 2: " ++ show (getRes2 w)
    putStrLn $ "  Автомат 3: " ++ show (getRes3 w)
    putStrLn ""
    ) testWords