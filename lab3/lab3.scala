import scala.collection.mutable.Set
import scala.util.Random

object CYKParser {
  case class Rule(lhs: String, rhs: List[String])

  def cykParse(word: String, grammar: List[Rule], startSymbols: Any): Boolean = {
    val startSymbolsList = startSymbols match {
      case s: String => List(s)
      case lst: List[_] => lst.asInstanceOf[List[String]]
      case _ => throw new IllegalArgumentException("startSymbols must be String or List[String]")
    }
    
    val n = word.length
    if (n == 0) {
      return startSymbolsList.exists(s => 
        grammar.exists(rule => rule.lhs == s && rule.rhs.isEmpty)
      )
    }

    val dp = Array.fill(n)(Array.fill(n + 1)(Set[String]()))

    for (i <- 0 until n) {
      val symbol = word.charAt(i).toString
      for (rule <- grammar) {
        if (rule.rhs.length == 1 && rule.rhs.head == symbol) {
          dp(i)(1).add(rule.lhs)
        }
      }
    }

    for (l <- 2 to n) {
      for (i <- 0 to n - l) {
        for (k <- 1 until l) {
          val leftSet = dp(i)(k)
          val rightSet = dp(i + k)(l - k)
          if (leftSet.nonEmpty && rightSet.nonEmpty) {
            for (rule <- grammar) {
              if (rule.rhs.length == 2) {
                val B = rule.rhs.head
                val C = rule.rhs(1)
                if (leftSet.contains(B) && rightSet.contains(C)) {
                  dp(i)(l).add(rule.lhs)
                }
              }
            }
          }
        }
      }
    }

    startSymbolsList.exists(start => dp(0)(n).contains(start))
  }

  def generateRandomWord(length: Int): String = {
    Random.alphanumeric.filter(c => c == 'a' || c == 'b').take(length).mkString
  }

  // Grammar 1, переходим к нфх, чтобы по соотвествующему алгоритму распарсить
  val grammar1 = List(
    Rule("S", List("T", "S3")),
    Rule("S3", List("S", "T")),
    Rule("S", List("S", "S4")),
    Rule("S4", List("B1", "S")),
    Rule("S", List("A1", "S5")),
    Rule("S5", List("A2", "A3")),
    Rule("T", List("B1", "B2")),
    Rule("T", List("T", "T1")),
    Rule("T1", List("A1", "T")),
    Rule("A1", List("a")),
    Rule("A2", List("a")),
    Rule("A3", List("a")),
    Rule("B1", List("b")),
    Rule("B2", List("b"))
  )
  val startSymbols1 = "S"

  // Grammar 2
  val grammar2 = List(
    Rule("A", List("a")),
    Rule("B", List("b")),
    Rule("AA", List("A", "A")),
    
    Rule("T03", List("T03", "X1")),
    Rule("X1", List("A", "T13")),
    Rule("T13", List("T13", "X2")),
    Rule("X2", List("A", "T13")),
    Rule("T33", List("T33", "X3")),
    Rule("X3", List("A", "T13")),
    Rule("T56", List("T56", "X4")),
    Rule("X4", List("A", "T46")),
    Rule("T46", List("T46", "X5")),
    Rule("X5", List("A", "T46")),
    Rule("T66", List("T66", "X6")),
    Rule("X6", List("A", "T46")),
    
    Rule("T03", List("B", "B")),
    Rule("T13", List("B", "B")),
    Rule("T33", List("B", "B")),
    Rule("T56", List("B", "B")),
    Rule("T46", List("B", "B")),
    Rule("T66", List("B", "B")),
    
    Rule("S06", List("T03", "Z1")),
    Rule("Z1", List("S35", "T56")),
    Rule("S06", List("T03", "Z2")),
    Rule("Z2", List("S36", "T66")),
    Rule("S36", List("T33", "Z3")),
    Rule("Z3", List("S35", "T56")),
    Rule("S36", List("T33", "Z4")),
    Rule("Z4", List("S36", "T66")),
    
    Rule("S05", List("S05", "Y1")),
    Rule("Y1", List("B", "S05")),
    Rule("S05", List("S06", "Y2")),
    Rule("Y2", List("B", "S05")),
    Rule("S06", List("S05", "Y3")),
    Rule("Y3", List("B", "S06")),
    Rule("S06", List("S06", "Y4")),
    Rule("Y4", List("B", "S06")),
    Rule("S35", List("S35", "Y5")),
    Rule("Y5", List("B", "S05")),
    Rule("S35", List("S36", "Y6")),
    Rule("Y6", List("B", "S05")),
    Rule("S36", List("S35", "Y7")),
    Rule("Y7", List("B", "S06")),
    Rule("S36", List("S36", "Y8")),
    Rule("Y8", List("B", "S06")),
    
    Rule("S05", List("A", "AA")),
    Rule("S35", List("A", "AA"))
  )
  val startSymbols2 = List("S05", "S06")

  // Grammar 3
  val grammar3 = List(
    Rule("A", List("a")),
    Rule("B", List("b")),
    Rule("AA", List("A", "A")),
    
    Rule("T01", List("T01", "X1")),
    Rule("X1", List("A", "T41")),
    Rule("T41", List("T41", "X2")),
    Rule("X2", List("A", "T41")),
    Rule("T21", List("T21", "X3")),
    Rule("X3", List("A", "T41")),
    Rule("T31", List("T31", "X4")),
    Rule("X4", List("A", "T41")),
    
    Rule("T01", List("B", "B")),
    Rule("T41", List("B", "B")),
    Rule("T21", List("B", "B")),
    Rule("T31", List("B", "B")),
    
    Rule("S01", List("T01", "Z1")),
    Rule("Z1", List("S13", "T31")),
    Rule("S01", List("T01", "Z2")),
    Rule("Z2", List("S12", "T21")),
    Rule("S31", List("T31", "Z3")),
    Rule("Z3", List("S13", "T31")),
    Rule("S31", List("T31", "Z4")),
    Rule("Z4", List("S12", "T21")),
    
    Rule("S01", List("S02", "Y1")),
    Rule("Y1", List("B", "S31")),
    Rule("S01", List("S01", "Y2")),
    Rule("Y2", List("B", "S31")),
    Rule("S02", List("S02", "Y3")),
    Rule("Y3", List("B", "S32")),
    Rule("S02", List("S01", "Y4")),
    Rule("Y4", List("B", "S32")),
    Rule("S31", List("S32", "Y5")),
    Rule("Y5", List("B", "S31")),
    Rule("S31", List("S31", "Y6")),
    Rule("Y6", List("B", "S31")),
    Rule("S32", List("S32", "Y7")),
    Rule("Y7", List("B", "S32")),
    Rule("S32", List("S31", "Y8")),
    Rule("Y8", List("B", "S32")),
    Rule("S12", List("S12", "Y9")),
    Rule("Y9", List("B", "S32")),
    Rule("S12", List("S11", "Y10")),
    Rule("Y10", List("B", "S32")),
    Rule("S13", List("S12", "Y11")),
    Rule("Y11", List("B", "S33")),
    Rule("S13", List("S11", "Y12")),
    Rule("Y12", List("B", "S33")),
    Rule("S11", List("S12", "Y13")),
    Rule("Y13", List("B", "S31")),
    Rule("S11", List("S11", "Y14")),
    Rule("Y14", List("B", "S31")),
    Rule("S33", List("S32", "Y15")),
    Rule("Y15", List("B", "S33")),
    Rule("S33", List("S31", "Y16")),
    Rule("Y16", List("B", "S33")),
    
    Rule("S02", List("A", "AA")),
    Rule("S12", List("A", "AA")),
    Rule("S32", List("A", "AA"))
  )
  val startSymbols3 = List("S01", "S02")

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to 500) {
      val word = generateRandomWord(50)
      val result1 = cykParse(word, grammar1, startSymbols1)
      val result2 = cykParse(word, grammar2, startSymbols2)
      val result3 = cykParse(word, grammar3, startSymbols3)
      val allAccept = result1 && result2 && result3
      val allReject = !result1 && !result2 && !result3
      if (!(allAccept || allReject)) println(word)
    }
  }
}