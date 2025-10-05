import scala.util.Random
import scala.annotation.tailrec

object StringReduction {
  private val random = new Random()
  
  def generateRandomString(n: Int): String = {
    val characters = "ab"
    (0 until n).map { _ =>
      val randomIndex = random.nextInt(characters.length)
      characters.charAt(randomIndex)
    }.mkString
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
      val poses = findAllSubstringPositions(str, rule.from)
      poses.map(pos => (pos, rule))
    }
    
    if (arr.isEmpty || count == 6) (str, true)
    else {
      val randomIndex = random.nextInt(arr.length)
      val (pos, rule) = arr(randomIndex)
      val newStr = str.substring(0, pos) + rule.to + str.substring(pos + rule.from.length)
      (newStr, false)
    }
  }
  
  def randomNormalize(str1: String): String = {
    @tailrec
    def normalizeLoop(currentStr: String, countRules: Int): String = {
      val (newStr, res) = randomReduce(currentStr, countRules)
      if (res) newStr
      else normalizeLoop(newStr, countRules + 1)
    }
    normalizeLoop(str1, 0)
  }
  
  val rules1: List[Rule] = List(
    Rule("aaaa", "a"),
    Rule("aaab", "b"),
    Rule("bab", "baa"),
    Rule("baa", "abb"),
    Rule("aba", "bb"),
    Rule("bb", "ba")
  )
  
  def fuzz(str1: String, str2: String): Boolean = {
    var normStr1: String = ""
    var way: List[Int] = Nil
    var err: Boolean = false
    
    def normalize(tempStr: String, withWay: Boolean = false): Unit = {
      var found = false
      
      if (withWay && tempStr == normStr1) {
        return
      }
      
      for (i <- rules1.indices if !found) {
        val rule = rules1(i)
        val pos = tempStr.indexOf(rule.from)
        if (pos != -1) {
          val newStr = tempStr.substring(0, pos) + rule.to + tempStr.substring(pos + rule.from.length)
          if (withWay) way = i :: way
          normalize(newStr, withWay)
          found = true
        }
      }
      
      if (!found && withWay && tempStr != normStr1) {
        err = true
        return
      }
      
      if (!found && !withWay) {
        normStr1 = tempStr
      }
    }
    
    normalize(str1)
    normalize(str2, true)
    
    if (err) return false
    if (way.isEmpty) return true
    
    way = way.reverse
    
    var res = false
    val mem = scala.collection.mutable.Set.empty[String]
    
    def dfs(tempStr: String, index_way: Int): Unit = {
      if (index_way == way.length || res) return
      if (tempStr == str2 || res) {
        res = true
        return
      }
      if (mem.contains(tempStr)) return
      
      mem.add(tempStr)
      val ruleIndex = way(index_way)
      val rule = rules1(ruleIndex)
      val poses = findAllSubstringPositions(tempStr, rule.to)
      
      for (pos <- poses) {
        val newStr = tempStr.substring(0, pos) + rule.from + tempStr.substring(pos + rule.to.length)
        dfs(newStr, index_way + 1)
      }
    }
    
    dfs(str2, 0)
    res
  }
  
  def firstCompare(str1: String, str2: String): Boolean = {
    str1.contains('b') == str2.contains('b')
  }
  
  def secondCompare(str1: String, str2: String): Boolean = {
    val regexList = List(
      "^$".r,                          // 1. пустое слово
      "^a(aaa)*$".r,                   // 2. a(aaa)*
      "^(b|aaa(aaa)*b)$".r,            // 3. b|aaa(aaa)*b  
      "^aa(aaa)*$".r,                  // 4. aa(aaa)*
      "^a(aaa)*b$".r,                  // 5. a(aaa)*b
      "^(ba|aaa(aaa)*ba|bb|aaa(aaa)*bb|a(aaa)*ba|a(aaa)*bb|aa(aaa)*ba|aa(aaa)*bb)(a|b)*$".r, // 6. сложная
      "^aaa(aaa)*$".r,                 // 7. aaa(aaa)*
      "^aa(aaa)*b$".r                  // 8. aa(aaa)*b
    )
    
    val gramInd = regexList.indexWhere(_.findFirstIn(str1).isDefined)
    if (gramInd == -1) false
    else regexList(gramInd).findFirstIn(str2).isDefined
  }
  
  def thirdCompare(str1: String, str2: String): Boolean = {
    def classifyWord(str: String): String = {
      if (str.isEmpty) {
        return "L_ε"
      }
      
      val firstBIndex = str.indexOf('b')
      if (firstBIndex == -1) {
        val lengthMod3 = str.length % 3
        lengthMod3 match {
          case 1 => "L_a"
          case 2 => "L_aa" 
          case 0 => "L_aaa"
        }
      } else {
        if (firstBIndex < str.length - 1) {
          return "L_ba"
        }
        
        val aCountBeforeB = firstBIndex
        val aCountMod3 = aCountBeforeB % 3
        
        aCountMod3 match {
          case 0 => "L_b"
          case 1 => "L_ab"
          case 2 => "L_aab"
        }
      }
    }
    
    classifyWord(str1) == classifyWord(str2)
  }
  
  def fourthCompare(str1: String, str2: String): Boolean = {
    def hasBaOrBb(str: String): Boolean = {
      str.contains("ba") || str.contains("bb")
    }
    
    val hasBaBb1 = hasBaOrBb(str1)
    val hasBaBb2 = hasBaOrBb(str2)
    
    if (hasBaBb1 && hasBaBb2) {
      true
    } else if (!hasBaBb1 && !hasBaBb2) {
      val countA1 = str1.count(_ == 'a')
      val countA2 = str2.count(_ == 'a')
      (countA1 % 3) == (countA2 % 3)
    } else {
      false
    }
  }
  
  def randomReduce1(str: String, count: Int): (String, Boolean) = {
    val arr = rules1.flatMap { rule =>
      val poses = findAllSubstringPositions(str, rule.from)
      poses.map(pos => (pos, rule))
    }
    
    if (arr.isEmpty || count == 5) (str, true)
    else {
      val randomIndex = random.nextInt(arr.length)
      val (pos, rule) = arr(randomIndex)
      val newStr = str.substring(0, pos) + rule.to + str.substring(pos + rule.from.length)
      (newStr, false)
    }
  }
  
  def randomNormalize1(str1: String): String = {
    @tailrec
    def normalizeLoop(currentStr: String, countRules: Int): String = {
      val (newStr, res) = randomReduce1(currentStr, countRules)
      if (res) newStr
      else normalizeLoop(newStr, countRules + 1)
    }
    normalizeLoop(str1, 0)
  }
  
  def meta(str1: String): Boolean = {
    val str = randomNormalize1(str1)
    firstCompare(str1, str) && secondCompare(str1, str) && thirdCompare(str1, str) && fourthCompare(str1, str)
  }
  
  def testing(): Unit = {
    for (i <- 0 until 1000) {
      val testString = generateRandomString(50)  
      val reducedString = randomNormalize(testString)
      
      if (i % 100 == 0) println(i) 
      
      if (!((fuzz(testString, reducedString) || fuzz(reducedString, testString)) && meta(testString))) {
      //рухнули
        if (testString.length > reducedString.length || 
            (testString.length == reducedString.length && testString > reducedString)) {
          println(s"$testString $reducedString")
        } else {
          println(s"$reducedString $testString")
        }
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    testing()
  }
}
