import scala.util.Random

object JDoodle {
  def main(args: Array[String]) {
    testAutomata()
  }
  
  def generateRandomString(n: Int): String = {
    val characters = "acde"
    val random = new Random()
    
    (1 to n).map { _ =>
      val randomIndex = random.nextInt(characters.length)
      characters.charAt(randomIndex)
    }.mkString
  }
  
  // Первый автомат (детерминированный)
  val automat1: Array[Map[Char, Int]] = Array(
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
    def getState(word1: String, curState: Int): Int = {
      if (word1 == "") {
        curState
      } else {
        val symbol = word1.charAt(0)
        val nextState = automat1(curState).getOrElse(symbol, 7)
        getState(word1.substring(1), nextState)
      }
    }
    
    val fin = getState(word, 0)
    if (fin >= 15 && fin <= 21) true else false
  }
  
  // Второй автомат (недетерминированный)
  val automat2: Array[Map[Char, List[Int]]] = Array(
    Map(), // state 0 (не используется)
    Map('d' -> List(2), 'e' -> List(8), 'c' -> List(11, 13)),                    // state 1
    Map('c' -> List(3), 'd' -> List(1)),                                        // state 2
    Map('d' -> List(4)),                                                        // state 3
    Map('c' -> List(1, 5)),                                                     // state 4
    Map('c' -> List(1, 5), 'd' -> List(1, 6), 'e' -> List(1, 7)),               // state 5
    Map('d' -> List(1, 6)),                                                     // state 6
    Map('e' -> List(1, 7)),                                                     // state 7
    Map('d' -> List(1), 'c' -> List(9), 'e' -> List(10)),                       // state 8
    Map('c' -> List(8)),                                                        // state 9
    Map('e' -> List(1)),                                                        // state 10
    Map('a' -> List(12)),                                                       // state 11
    Map('c' -> List(1)),                                                        // state 12
    Map('a' -> List(14), 'c' -> List(14), 'd' -> List(14), 'e' -> List(14)),    // state 13
    Map()                                                                       // state 14 (final)
  )
  
  val finalState2 = 14
  
  def getRes2(word: String): Boolean = {
    var res = false
    
    def processWord(word1: String, currentState: Int): Unit = {
      if (res || (word1 == "" && currentState == finalState2)) {
        res = true
        return
      }
      if (word1 == "") return
      
      val symbol = word1.charAt(0)
      val nextStates = automat2(currentState).getOrElse(symbol, List())
      
      if (nextStates.nonEmpty) {
        for (t <- nextStates) {
          processWord(word1.substring(1), t)
        }
      }
    }
    
    processWord(word, 1)
    res
  }
  
  // Третий автомат (с эпсилон-переходами)
  val automat3: Map[Int, Map[Char, List[Int]]] = Map(
    0 -> Map('ε' -> List(1, 23)), // начальное состояние с эпсилон-переходами
    
    // Первый автомат (состояния 1-22)
    1 -> Map('a' -> List(8), 'c' -> List(12), 'd' -> List(5), 'e' -> List(6)),
    2 -> Map('a' -> List(8), 'c' -> List(8), 'd' -> List(7), 'e' -> List(8)),
    3 -> Map('a' -> List(8), 'c' -> List(6), 'd' -> List(8), 'e' -> List(8)),
    4 -> Map('a' -> List(8), 'c' -> List(8), 'd' -> List(8), 'e' -> List(1)),
    5 -> Map('a' -> List(8), 'c' -> List(2), 'd' -> List(1), 'e' -> List(8)),
    6 -> Map('a' -> List(8), 'c' -> List(3), 'd' -> List(1), 'e' -> List(4)),
    7 -> Map('a' -> List(8), 'c' -> List(11), 'd' -> List(8), 'e' -> List(8)),
    8 -> Map('a' -> List(8), 'c' -> List(8), 'd' -> List(8), 'e' -> List(8)), // ловушка
    9 -> Map('a' -> List(8), 'c' -> List(13), 'd' -> List(9), 'e' -> List(6)),
    10 -> Map('a' -> List(8), 'c' -> List(14), 'd' -> List(9), 'e' -> List(10)),
    11 -> Map('a' -> List(8), 'c' -> List(15), 'd' -> List(9), 'e' -> List(10)),
    12 -> Map('a' -> List(18), 'c' -> List(16), 'd' -> List(16), 'e' -> List(16)),
    13 -> Map('a' -> List(18), 'c' -> List(16), 'd' -> List(19), 'e' -> List(16)),
    14 -> Map('a' -> List(18), 'c' -> List(17), 'd' -> List(16), 'e' -> List(16)),
    15 -> Map('a' -> List(18), 'c' -> List(22), 'd' -> List(20), 'e' -> List(21)),
    16 -> Map('a' -> List(8), 'c' -> List(8), 'd' -> List(8), 'e' -> List(8)),
    17 -> Map('a' -> List(8), 'c' -> List(3), 'd' -> List(1), 'e' -> List(4)),
    18 -> Map('a' -> List(8), 'c' -> List(1), 'd' -> List(8), 'e' -> List(8)),
    19 -> Map('a' -> List(8), 'c' -> List(12), 'd' -> List(8), 'e' -> List(8)),
    20 -> Map('a' -> List(8), 'c' -> List(13), 'd' -> List(9), 'e' -> List(6)),
    21 -> Map('a' -> List(8), 'c' -> List(14), 'd' -> List(9), 'e' -> List(10)),
    22 -> Map('a' -> List(18), 'c' -> List(22), 'd' -> List(20), 'e' -> List(21)),
    
    // Второй автомат (состояния 23-30)
    23 -> Map('a' -> List(23), 'c' -> List(23), 'd' -> List(23), 'e' -> List(24)),
    24 -> Map('a' -> List(23), 'c' -> List(25), 'd' -> List(23), 'e' -> List(24)),
    25 -> Map('a' -> List(23), 'c' -> List(26), 'd' -> List(23), 'e' -> List(24)),
    26 -> Map('a' -> List(23), 'c' -> List(27), 'd' -> List(23), 'e' -> List(24)),
    27 -> Map('a' -> List(23), 'c' -> List(28), 'd' -> List(23), 'e' -> List(24)),
    28 -> Map('a' -> List(8), 'c' -> List(29), 'd' -> List(23), 'e' -> List(30)),
    29 -> Map('a' -> List(23), 'c' -> List(28), 'd' -> List(23), 'e' -> List(24)),
    30 -> Map('a' -> List(8), 'c' -> List(8), 'd' -> List(8), 'e' -> List(23))
  )
  
  val finalStates3 = Set(16, 17, 18, 19, 20, 21, 22, 23, 24, 26)
  
  def getRes3(word: String): Boolean = {    
    def processWord(word1: String, currentState: Int): Boolean = {
      if (currentState == 0) { // начальное состояние
        val nextStates = automat3(currentState)('ε')
        nextStates.forall(state => processWord(word1, state))
      } else if (word1 == "") {
        finalStates3.contains(currentState)
      } else {
        val symbol = word1.charAt(0)
        val nextStates = automat3.get(currentState) match {
          case Some(transitions) => transitions.getOrElse(symbol, List())
          case None => List()
        }
        
        if (nextStates.isEmpty) false
        else {
          nextStates.exists { nextState =>
            processWord(word1.substring(1), nextState)
          }
        }
      }
    }
    
    processWord(word, 0)
  }
  
  // Регулярное выражение
  def getRes(word: String): Boolean = {
    val pattern = "^(dcdc*c(d*|e*e)|e(cc)*(ee|d)|cac|dd)*c(a|c|d|e)$".r
    pattern.matches(word)
  }
  
  def testAutomata(): Unit = {

    for (i <- 1 to 1000) {
      val word = generateRandomString(7)
      val regexResult = getRes(word)
      val res1 = getRes1(word)
      val res2 = getRes2(word)
      val res3 = getRes3(word)
      
      val allMatch = (regexResult && res1 && res2 && res3) || (!regexResult && !res1 && !res2 && !res3)
      
      if (!allMatch) {
        println(s"$word: regex=$regexResult, res1=$res1, res2=$res2, res3=$res3")
      }
    }
    
    println("Testing completed.")
  }
}