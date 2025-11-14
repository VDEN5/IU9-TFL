import scala.util.Random
import scala.collection.mutable

object AutomataChecker {
  private val random = new Random()
  private val characters = "acde"
  
  def generateRandomString(n: Int): String = {
    (1 to n).map(_ => characters(random.nextInt(characters.length))).mkString
  }
  
  // Детерминированный автомат 1 (без изменений)
  private val automat1: Array[Map[Char, Int]] = Array(
    Map('a' -> 7, 'b' -> 7, 'c' -> 11, 'd' -> 4, 'e' -> 5),    // state 0
    Map('a' -> 7, 'b' -> 7, 'c' -> 7, 'd' -> 6, 'e' -> 7),     // state 1
    Map('a' -> 7, 'b' -> 7, 'c' -> 5, 'd' -> 7, 'e' -> 7),     // state 2
    Map('a' -> 7, 'b' -> 7, 'c' -> 7, 'd' -> 7, 'e' -> 0),     // state 3
    Map('a' -> 7, 'b' -> 7, 'c' -> 1, 'd' -> 0, 'e' -> 7),     // state 4
    Map('a' -> 7, 'b' -> 7, 'c' -> 2, 'd' -> 0, 'e' -> 3),     // state 5
    Map('a' -> 7, 'b' -> 7, 'c' -> 10, 'd' -> 7, 'e' -> 7),    // state 6
    Map('a' -> 7, 'b' -> 7, 'c' -> 7, 'd' -> 7, 'e' -> 7),     // state 7 (trap)
    Map('a' -> 7, 'b' -> 7, 'c' -> 12, 'd' -> 8, 'e' -> 5),    // state 8
    Map('a' -> 7, 'b' -> 7, 'c' -> 13, 'd' -> 8, 'e' -> 9),    // state 9
    Map('a' -> 7, 'b' -> 7, 'c' -> 14, 'd' -> 8, 'e' -> 9),    // state 10
    Map('a' -> 17, 'b' -> 7, 'c' -> 15, 'd' -> 15, 'e' -> 15), // state 11
    Map('a' -> 17, 'b' -> 7, 'c' -> 15, 'd' -> 18, 'e' -> 15), // state 12
    Map('a' -> 17, 'b' -> 7, 'c' -> 16, 'd' -> 15, 'e' -> 15), // state 13
    Map('a' -> 17, 'b' -> 7, 'c' -> 21, 'd' -> 19, 'e' -> 20), // state 14
    Map('a' -> 7, 'b' -> 7, 'c' -> 7, 'd' -> 7, 'e' -> 7),     // state 15 (final)
    Map('a' -> 7, 'b' -> 7, 'c' -> 2, 'd' -> 0, 'e' -> 3),     // state 16 (final)
    Map('a' -> 7, 'b' -> 7, 'c' -> 0, 'd' -> 7, 'e' -> 7),     // state 17 (final)
    Map('a' -> 7, 'b' -> 7, 'c' -> 10, 'd' -> 7, 'e' -> 7),    // state 18 (final)
    Map('a' -> 7, 'b' -> 7, 'c' -> 12, 'd' -> 8, 'e' -> 5),    // state 19 (final)
    Map('a' -> 7, 'b' -> 7, 'c' -> 13, 'd' -> 8, 'e' -> 9),    // state 20 (final)
    Map('a' -> 17, 'b' -> 7, 'c' -> 21, 'd' -> 19, 'e' -> 20)  // state 21 (final)
  )
  
  def getRes1(word: String): Boolean = {
    @annotation.tailrec
    def getState(word1: String, curState: Int): Int = {
      if (word1.isEmpty) curState
      else {
        val symbol = word1.head
        val nextState = automat1(curState).getOrElse(symbol, 7)
        getState(word1.tail, nextState)
      }
    }
    
    val fin = getState(word, 0)
    fin >= 15 && fin <= 21
  }
  
  // Новый недетерминированный автомат 2
  private val automat2: Array[Map[Char, List[Int]]] = Array(
    Map.empty[Char, List[Int]], // state 0 (не используется)
    Map('d' -> List(2), 'e' -> List(8), 'c' -> List(11, 13)),                    // state 1
    Map('c' -> List(3), 'd' -> List(1)),                                        // state 2
    Map('d' -> List(4)),                                                        // state 3
    Map('c' -> List(5)),                                                        // state 4
    Map('c' -> List(5), 'd' -> List(1, 6), 'e' -> List(1, 7)),                  // state 5
    Map('d' -> List(1, 6)),                                                     // state 6
    Map('e' -> List(1, 7)),                                                     // state 7
    Map('d' -> List(1), 'c' -> List(9), 'e' -> List(10)),                       // state 8
    Map('c' -> List(8)),                                                        // state 9
    Map('e' -> List(1)),                                                        // state 10
    Map('a' -> List(12)),                                                       // state 11
    Map('c' -> List(1)),                                                        // state 12
    Map('a' -> List(14), 'c' -> List(14), 'd' -> List(14), 'e' -> List(14)),    // state 13
    Map.empty[Char, List[Int]]                                                  // state 14 (final)
  )
  
  private val finalState2 = 14
  
  def getRes2(word: String): Boolean = {
    val memo = mutable.Map[(String, Int), Boolean]()
    
    def processWord(word1: String, currentState: Int): Boolean = {
      memo.getOrElseUpdate((word1, currentState), {
        if (word1.isEmpty && currentState == finalState2) true
        else if (word1.isEmpty) false
        else {
          val symbol = word1.head
          val nextStates = automat2(currentState).getOrElse(symbol, Nil)
          nextStates.exists(nextState => processWord(word1.tail, nextState))
        }
      })
    }
    
    processWord(word, 1)
  }
  
  // Новый автомат 3 (пересечение)
  private val automat3: Map[Int, Map[Char, List[Int]]] = Map(
    0 -> Map('ε' -> List(1, 16)), // начальное состояние с эпсилон-переходами
    
    1 -> Map('a' -> List(8), 'c' -> List(10), 'd' -> List(5), 'e' -> List(6)),    // бывшее 0
    2 -> Map('a' -> List(8), 'c' -> List(8), 'd' -> List(7), 'e' -> List(8)),     // бывшее 1
    3 -> Map('a' -> List(8), 'c' -> List(6), 'd' -> List(8), 'e' -> List(8)),     // бывшее 2
    4 -> Map('a' -> List(8), 'c' -> List(8), 'd' -> List(8), 'e' -> List(1)),     // бывшее 3
    5 -> Map('a' -> List(8), 'c' -> List(2), 'd' -> List(1), 'e' -> List(8)),     // бывшее 4
    6 -> Map('a' -> List(8), 'c' -> List(3), 'd' -> List(1), 'e' -> List(4)),     // бывшее 5
    7 -> Map('a' -> List(9), 'c' -> List(9), 'd' -> List(9), 'e' -> List(9)),     // бывшее 6
    8 -> Map('a' -> List(8), 'c' -> List(8), 'd' -> List(8), 'e' -> List(8)),     // бывшее 7 (ловушка)
    9 -> Map('a' -> List(9), 'c' -> List(11), 'd' -> List(9), 'e' -> List(9)),    // бывшее 8
    10 -> Map('a' -> List(13), 'c' -> List(12), 'd' -> List(12), 'e' -> List(12)),// бывшее 9
    11 -> Map('a' -> List(14), 'c' -> List(15), 'd' -> List(14), 'e' -> List(14)),// бывшее 10
    12 -> Map('a' -> List(8), 'c' -> List(8), 'd' -> List(8), 'e' -> List(8)),    // бывшее 11 (final)
    13 -> Map('a' -> List(8), 'c' -> List(1), 'd' -> List(8), 'e' -> List(8)),    // бывшее 12 (final)
    14 -> Map('a' -> List(9), 'c' -> List(11), 'd' -> List(9), 'e' -> List(9)),   // бывшее 13 (final)
    15 -> Map('a' -> List(14), 'c' -> List(15), 'd' -> List(14), 'e' -> List(14)),// бывшее 14 (final)
    
    // Второй автомат
    16 -> Map('a' -> List(16), 'c' -> List(16), 'd' -> List(17), 'e' -> List(16)), // бывшее 19
    17 -> Map('a' -> List(16), 'c' -> List(18), 'd' -> List(17), 'e' -> List(16)), // бывшее 20
    18 -> Map('a' -> List(16), 'c' -> List(16), 'd' -> List(19), 'e' -> List(16)), // бывшее 21
    19 -> Map('a' -> List(8), 'c' -> List(20), 'd' -> List(8), 'e' -> List(8)),    // бывшее 22
    20 -> Map('a' -> List(16), 'c' -> List(20), 'd' -> List(21), 'e' -> List(22)), // бывшее 23
    21 -> Map('a' -> List(8), 'c' -> List(18), 'd' -> List(21), 'e' -> List(16)),  // бывшее 24
    22 -> Map('a' -> List(8), 'c' -> List(16), 'd' -> List(17), 'e' -> List(22))   // бывшее 25
  )
  
  private val finalStates3 = Set(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
  
  def getRes3(word: String): Boolean = {
    val memo = mutable.Map[(String, Int), Boolean]()
    
    def processWord(word1: String, currentState: Int): Boolean = {
      memo.getOrElseUpdate((word1, currentState), {
        if (currentState == 0) { // начальное состояние
          val nextStates = automat3(currentState)('ε')
          nextStates.forall(state => processWord(word1, state))
        } else if (word1.isEmpty) {
          finalStates3.contains(currentState)
        } else {
          val symbol = word1.head
          val nextStates = automat3.get(currentState).flatMap(_.get(symbol)).getOrElse(Nil)
          nextStates.exists(nextState => processWord(word1.tail, nextState))
        }
      })
    }
    
    processWord(word, 0)
  }
  
  // Регулярное выражение
  def getRes(word: String): Boolean = {
    val regex = "^(dcdc*c(d*|e*e)|e(cc)*(ee|d)|cac|dd)*c(a|c|d|e)$".r
    regex.matches(word)
  }
  
  def main(args: Array[String]): Unit = {
    for (i <- 0 until 100) {
      val word = generateRandomString(100)
      val regexResult = getRes(word)
      val res1 = getRes1(word)
      val res2 = getRes2(word)
      val res3 = getRes3(word)
      
      val allMatch = (regexResult && res1 && res2 && res3) || (!regexResult && !res1 && !res2 && !res3)
      
      if (getRes(word)) println(s"Accepted: $word")
      if (i % 10 == 0) println(s"Progress: $i")
      if (!allMatch) {
        println(s"Mismatch at word: $word")
        println(s"  regex: $regexResult, res1: $res1, res2: $res2, res3: $res3")
      }
    }
    
    println("Testing completed")
  }
}