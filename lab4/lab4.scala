import scala.collection.mutable
import scala.util.Random

object Parser {
  case class ParseResult(newStr: String, newPos: Int, isFinal: Boolean)
  //Утилита для обработки TST
  def f(str: String, pos1: Int): ParseResult = {
    val pos = str.indexOf("bbSbb", pos1)
    
    if (pos == -1) return ParseResult(str, pos, isFinal = true)
    
    val adjustedPos = pos + 2
    
    // Поиск вправо
    var ri = adjustedPos + 3
    var ri_a = 0
    var foundRight = true
    
    while (foundRight && ri + 2 < str.length) {
      if (str(ri) == 'a' && str(ri + 1) == 'b' && str(ri + 2) == 'b') {
        ri += 3
        ri_a += 1
      } else {
        foundRight = false
      }
    }
    
    // Поиск влево
    var le = adjustedPos - 3
    var le_a = 0
    var foundLeft = true
    
    while (foundLeft && le - 2 >= 0) {
      if (str(le) == 'a' && str(le - 1) == 'b' && str(le - 2) == 'b') {
        le -= 3
        le_a += 1
      } else {
        foundLeft = false
      }
    }
    
    if (Math.ceil(Math.log(le_a + 1) / Math.log(2)) < ri_a) {
      val newStr = str.substring(0, le + 1) + "S" + str.substring(ri)
      return ParseResult(newStr, 0, isFinal = true)
    }
    
    ParseResult(str, adjustedPos, isFinal = false)
  }
  //Оптимизированный парсинг
  def parse(str: String): Boolean = {
    var currentStr = str.replace("aaa", "S")
    
    var shouldContinue = true
    
    while (shouldContinue) {
      // Замена всех "SbS" на "S"
      var replaced = true
      while (replaced) {
        val oldStr = currentStr
        currentStr = currentStr.replace("SbS", "S")
        replaced = oldStr != currentStr
      }
      
      var pos = 0
      var innerContinue = true
      
      while (innerContinue) {
        val res = f(currentStr, pos)
        if (res.isFinal) {
          currentStr = res.newStr
          if (res.newPos == -1) return currentStr == "S"
          innerContinue = false
        } else {
          pos = res.newPos
        }
      }
    }
    
    false
  }

  def generateRandomWord(length: Int): String = {
    val random = new Random()
    (0 until length).map { _ =>
      if (random.nextBoolean()) 'a' else 'b'
    }.mkString
  }

  val memoParseS = mutable.Map[(Int, Int), Boolean]()
  val memoParseT = mutable.Map[(Int, Int), Set[Int]]()

  def parseT(str: String, start: Int, end: Int): Set[Int] = {
    val key = (start, end)
    
    if (memoParseT.contains(key)) {
      return memoParseT(key)
    }
    
    val attrs = mutable.Set[Int]()
    
    // Правило T -> b b (атрибут 0)
    if (end - start == 2 && str.substring(start, end) == "bb") {
      attrs.add(0)
    }
    
    // Правило T -> T a T (атрибут max(a1, a2) + 1)
    for (k <- start + 1 until end - 1) {
      if (str(k) == 'a') {
        val attrs1 = parseT(str, start, k)
        val attrs2 = parseT(str, k + 1, end)

        for (a1 <- attrs1; a2 <- attrs2) {
          attrs.add(math.max(a1, a2) + 1)
        }
      }
    }
    
    val result = attrs.toSet
    memoParseT.put(key, result)
    result
  }

  def parseS(str: String, start: Int, end: Int): Boolean = {
    val key = (start, end)
    
    if (memoParseS.contains(key)) {
      return memoParseS(key)
    }
    
    // Правило S -> a a a
    if (end - start == 3 && str.substring(start, end) == "aaa") {
      memoParseS.put(key, true)
      return true
    }
    
    // Правило S -> S b S
    for (k <- start + 1 until end - 1) {
      if (str(k) == 'b') {
        if (parseS(str, start, k) && parseS(str, k + 1, end)) {
          memoParseS.put(key, true)
          return true
        }
      }
    }
    
    // Правило S -> T S T с условием T1.a < T2.a
    var found = false
    var i = start + 1
    
    while (!found && i < end) {
      var j = i
      while (!found && j < end) {
        val attrs1 = parseT(str, start, i)
        if (attrs1.nonEmpty) {
          if (parseS(str, i, j)) {
            val attrs2 = parseT(str, j, end)
            if (attrs2.nonEmpty && attrs1.exists(a1 => attrs2.exists(a2 => a1 < a2))) {
              found = true
            }
          }
        }
        j += 1
      }
      i += 1
    }
    
    memoParseS.put(key, found)
    found
  }

  def isStringInLanguage(str: String): Boolean = {
    memoParseS.clear()
    memoParseT.clear()
    parseS(str, 0, str.length)
  }

  def runTests(): Unit = {
    for (i <- 0 until 300) {
      val word = generateRandomWord(30)
      val res1 = isStringInLanguage(word)
      val res2 = parse(word)
      val res = (res1 && res2) || (!res1 && !res2)
      println(s"$res $res1 $res2")
    }
  }

  def benchmark(): Unit = {
    val words = List(
      "bbaaabbabbbbbabbaaabbabbabb", // 27
      "bbabbbbaaabbabbbbbabbaaabbabbabbbbabbabb", // 40
      "bbabbbbaaabbabbbbbabbaaabbabbabbbbabbabbbbbabbaaabbabbabb", // 57
      "bbabbaaabbabbabbbbbabbbbaaabbabbbbbabbaaabbabbabbbbabbabbbbbabbaaabbabbabb", // 74
      "bbabbabbbbabbaaabbabbabbbbbabbbbaaabbabbbbbabbaaabbabbabbbbabbabbbbbabbaaabbabbabbbbabbabbabb", // 93
      "bbaaabbbabbbbbbabbabaabbabbabb", // 30
      "bbabbabbaaabbabbbabbabbaaabbabbabbbbabbabb", // 42
      "bbabbabbaaabbabbbbbabbaaabbabbabbbbabbabbabbbabbaaabbabbabb", // 59
      "bbabbaaabbabbabbbbbabbabbaaabbabbbbbabbaaabbabbabbbbabbabbbbbabbaaabbabbabb", // 75
      "bbabbabbabbabbbbabbaaabbabbabbbbbabbbbaaabbabbbbbabbaaabbabbabbbbabbabbbbbabbaaabbabbabbbbabbabbabb" // 99
    )
    
    for (word <- words) {
      val start = System.nanoTime()
      val result = parse(word)
      val time = (System.nanoTime() - start) / 1e6
      
      val start1 = System.nanoTime()
      val result1 = isStringInLanguage(word)
      val time1 = (System.nanoTime() - start1) / 1e6
      
      println(s"$result $result1 $time $time1")
    }
  }

  def main(args: Array[String]): Unit = {
    println("Running tests...")
    runTests()
    
    println("\nRunning benchmark...")
    benchmark()
  }
}