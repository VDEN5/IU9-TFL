import scala.util.Random

object AutomataChecker {
  private val random = new Random()
  private val characters = "acde"
  
  def generateRandomString(n: Int): String = {
    (1 to n).map(_ => characters(random.nextInt(characters.length))).mkString
  }
  
  // Детерминированный автомат 1
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
  
  // Недетерминированный автомат 2
  private val automat2: Array[Map[Char, List[Int]]] = Array(
    Map('c' -> List(3, 20), 'e' -> List(6), 'd' -> List(12, 1)),                    // state 0
    Map('d' -> List(2)),                                                            // state 1
    Map('c' -> List(3, 20), 'e' -> List(6), 'd' -> List(12, 1)),                    // state 2
    Map('a' -> List(4)),                                                            // state 3
    Map('c' -> List(5)),                                                            // state 4
    Map('c' -> List(3, 20), 'e' -> List(6), 'd' -> List(12, 1)),                    // state 5
    Map('d' -> List(9), 'c' -> List(7), 'e' -> List(10)),                           // state 6
    Map('c' -> List(8)),                                                            // state 7
    Map('e' -> List(10), 'c' -> List(7), 'd' -> List(9)),                           // state 8
    Map('c' -> List(3, 20), 'e' -> List(6), 'd' -> List(12, 1)),                    // state 9
    Map('e' -> List(11)),                                                           // state 10
    Map('c' -> List(3, 20), 'e' -> List(6), 'd' -> List(12, 1)),                    // state 11
    Map('c' -> List(13)),                                                           // state 12
    Map('d' -> List(14)),                                                           // state 13
    Map('c' -> List(16, 14)),                                                       // state 14
    Map.empty[Char, List[Int]],                                                     // state 15 (не используется, так как его просто и нет)
    Map('c' -> List(3, 20), 'e' -> List(6, 18), 'd' -> List(12, 1, 19)),            // state 16
    Map.empty[Char, List[Int]],                                                     // state 17 (не используется)
    Map('c' -> List(3, 20), 'e' -> List(6, 18), 'd' -> List(12, 1)),                // state 18
    Map('c' -> List(3, 20), 'e' -> List(6), 'd' -> List(12, 1, 19)),                // state 19
    Map('a' -> List(21), 'c' -> List(21), 'e' -> List(21), 'd' -> List(21)),        // state 20
    Map.empty[Char, List[Int]]                                                      // state 21 (final)
  )
  
  private val finalState2 = 21
  
  def getRes2(word: String): Boolean = {
    def processWord(word1: String, currentState: Int): Boolean = {
      if (word1.isEmpty && currentState == finalState2) true
      else if (word1.isEmpty) false
      else {
        val symbol = word1.head
        val nextStates = automat2(currentState).getOrElse(symbol, Nil)
        nextStates.exists(nextState => processWord(word1.tail, nextState))
      }
    }
    
    processWord(word, 0)
  }
  
  // автомат 3
  private val automat3: Map[Int, Map[Char, List[Int]]] = Map(
    0 -> Map('a' -> List(1, 15), 'c' -> List(1, 15), 'd' -> List(1, 15), 'e' -> List(1, 15)),
    1 -> Map('d' -> List(2, 13), 'e' -> List(8, 13), 'c' -> List(11, 13), 'a' -> List(13)),
    2 -> Map('c' -> List(3), 'd' -> List(1)),
    3 -> Map('d' -> List(4)),
    4 -> Map('c' -> List(5)),
    5 -> Map('c' -> List(5), 'd' -> List(1, 6), 'e' -> List(1, 7)),
    6 -> Map('d' -> List(1, 6)),
    7 -> Map('e' -> List(1, 7)),
    8 -> Map('d' -> List(1), 'c' -> List(9), 'e' -> List(10)),
    9 -> Map('c' -> List(8)),
    10 -> Map('e' -> List(1)),
    11 -> Map('a' -> List(12)),
    12 -> Map('c' -> List(1)),
    13 -> Map('a' -> List(14), 'c' -> List(14), 'd' -> List(14), 'e' -> List(14)),
    14 -> Map.empty[Char, List[Int]], // финальное состояние
    15 -> Map('a' -> List(15), 'e' -> List(15), 'c' -> List(15, 16), 'd' -> List(15)),
    16 -> Map('a' -> List(17), 'c' -> List(17), 'd' -> List(17), 'e' -> List(17)),
    17 -> Map.empty[Char, List[Int]]  // финальное состояние
  )
  
  def getRes3(word: String): Boolean = {
    def processWord(word1: String, currentState: Int): Boolean = {
      if (word1.isEmpty && (currentState == 14 || currentState == 17)) true
      else if (word1.isEmpty) false
      else {
        val symbol = word1.head
        val nextStates = automat3.get(currentState).flatMap(_.get(symbol)).getOrElse(Nil)
        
        if (currentState == 0) { // alternative state
          nextStates.forall(t => processWord(word1, t))
        } else {
          nextStates.exists(t => processWord(word1.tail, t))
        }
      }
    }
    
    processWord(word, 0)
  }
  
  // Регулярное выражение
  def getRes(word: String): Boolean = {
    val regex = "^(dcdc*c(d*|e*e)|e(cc)*(ee|d)|cac|dd)*c(a|c|d|e)$".r
    regex.matches(word)
  }
  
  def main(args: Array[String]): Unit = {
    for (i <- 0 until 1000) {
      val word = generateRandomString(2000)
      val regexResult = getRes(word)
      val res1 = getRes1(word)
      val res2 = getRes2(word)
      val res3 = getRes3(word)
      
      val allMatch = (regexResult && res1 && res2 && res3) || (!regexResult && !res1 && !res2 && !res3)
      
      if (getRes(word)) println(word)
      if (i%100 == 0) println(i)
      if (!allMatch) println(s"Mismatch: $allMatch, regex: $regexResult, res1: $res1, res2: $res2, res3: $res3")
    }
  }
}