import scala.annotation.tailrec
import scala.util.Random

object StringRewritingSystem {
  private val random = new Random()

  //просто рандомная строка из а и б, тут в правилах не нашем ничего такого, чтоб увелить вероятность выпадания определенной подстроки
  def generateRandomString(n: Int): String = {
    val characters = "ab"
    (1 to n).map(_ => characters(random.nextInt(characters.length))).mkString
  }
  
  def findAllSubstringPositions(str: String, substring: String): List[Int] = {
    @tailrec
    def findPositions(acc: List[Int], start: Int): List[Int] = {
      val index = str.indexOf(substring, start)
      if (index == -1) acc.reverse
      else findPositions(index :: acc, index + 1)
    }
    findPositions(Nil, 0)
  }
  
  case class Rule(from: String, to: String)
  
  val rules: List[Rule] = List(
    Rule("aaaa", "a"),
    Rule("aaab", "b"),
    Rule("bbba", "ba"),
    Rule("bbbb", "bb"),
    Rule("ababb", "babb"),
    Rule("bba", "baaba"),
    Rule("bbb", "baabb"),
    Rule("bbaaa", "baab"),
    Rule("bbaab", "baa"),
    Rule("bbabb", "abab"),
    Rule("baba", "baa"),
    Rule("babbaa", "babba"),
    Rule("babbab", "abb")
  )
  
  def randomReduce(str: String, count: Int): (String, Boolean) = {
    val arr = rules.flatMap { rule =>
      findAllSubstringPositions(str, rule.from).map { pos =>
        (pos, rule)
      }
    }
    
    if (arr.isEmpty || count == 6) (str, true)
    else {
      val (pos, rule) = arr(random.nextInt(arr.length))
      (str.substring(0, pos) + rule.to + str.substring(pos + rule.from.length), false)
    }
  }
  
  def randomNormalize(str: String): String = {
    @tailrec
    def normalizeHelper(currentStr: String, count: Int): String = {
      val (newStr, isDone) = randomReduce(currentStr, count)
      if (isDone) newStr
      else normalizeHelper(newStr, count + 1)
    }
    normalizeHelper(str, 0)
  }
  
  val rules1: List[Rule] = List(
    Rule("aaaa", "a"),
    Rule("aaab", "b"),
    Rule("bbaaa", "babbb"),
    Rule("aaaba", "baba"),
    Rule("baba", "baab"),
    Rule("bab", "baa"),
    Rule("baab", "baaa"),
    Rule("aaba", "bbb"),
    Rule("baa", "abb"),
    Rule("bba", "bab"),
    Rule("aba", "bb"),
    Rule("bb", "ba")
  )
  
  def fuzz(str1: String, str2: String): Boolean = {
    var result = false
    val mem = scala.collection.mutable.Set[String]()
    
    def dfs(tempStr: String): Unit = {
      if (tempStr.length < str2.length || result) return
      if (tempStr == str2 || result) {
        result = true
        return
      }
      if (mem.contains(tempStr)) return
      
      mem.add(tempStr)
      
      rules1.foreach { rule =>
        findAllSubstringPositions(tempStr, rule.from).foreach { pos =>
          val newStr = tempStr.substring(0, pos) + rule.to + tempStr.substring(pos + rule.from.length)
          dfs(newStr)
        }
      }
    }
    
    dfs(str1)
    result
  }
  
  def firstCompare(str1: String, str2: String): Boolean = {
    str1.contains('b') == str2.contains('b')
  }
  
  def secondCompare(str1: String, str2: String): Boolean = {
    val regexes = List(
      "^$".r,                          // 1. пустое слово
      "^a(aaa)*$".r,                   // 2. a(aaa)*
      "^(b|aaa(aaa)*b)$".r,            // 3. b|aaa(aaa)*b  
      "^aa(aaa)*$".r,                  // 4. aa(aaa)*
      "^a(aaa)*b$".r,                  // 5. a(aaa)*b
      "^(ba|aaa(aaa)*ba|bb|aaa(aaa)*bb|a(aaa)*ba|a(aaa)*bb|aa(aaa)*ba|aa(aaa)*bb)(a|b)*$".r, // 6. сложная
      "^aaa(aaa)*$".r,                 // 7. aaa(aaa)*
      "^aa(aaa)*b$".r                  // 8. aa(aaa)*b
    )
    
    val gramInd = regexes.indexWhere(_.findFirstIn(str1).isDefined)
    if (gramInd >= 0) regexes(gramInd).findFirstIn(str2).isDefined
    else false
  }
  
  def randomReduce1(str: String, count: Int): (String, Boolean) = {
    val arr = rules1.flatMap { rule =>
      findAllSubstringPositions(str, rule.from).map { pos =>
        (pos, rule)
      }
    }
    
    if (arr.isEmpty || count == 5) (str, true)
    else {
      val (pos, rule) = arr(random.nextInt(arr.length))
      (str.substring(0, pos) + rule.to + str.substring(pos + rule.from.length), false)
    }
  }
  
  def randomNormalize1(str: String): String = {
    @tailrec
    def normalizeHelper(currentStr: String, count: Int): String = {
      val (newStr, isDone) = randomReduce1(currentStr, count)
      if (isDone) newStr
      else normalizeHelper(newStr, count + 1)
    }
    normalizeHelper(str, 0)
  }
  
  def meta(str1: String): Boolean = {
    val str = randomNormalize1(str1)
    firstCompare(str1, str) && secondCompare(str1, str)
  }

  //советую проверять на длине 15, на 20 он будет работать тоже, но медленне (может быть даже, что и больше минуты на 100 тестах)
  def testing(): Unit = {
    for (i <- 0 until 100) {
      val testString = generateRandomString(15)
      val reducedString = randomNormalize(testString)
      
      if (i % 10 == 0) println(i)
      
      if (!((fuzz(testString, reducedString) || fuzz(reducedString, testString)) && meta(testString))) {
      //тут мы завалились
        if (testString.length > reducedString.length || 
            (testString.length == reducedString.length && testString > reducedString)) {
          println(s"$testString, $reducedString")
        } else {
          println(s"$reducedString, $testString")
        }
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    testing()
  }
}