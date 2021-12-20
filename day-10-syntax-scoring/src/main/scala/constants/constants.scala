package constants

object constants {
  val chars = Map("(" -> ")", "{" -> "}", "[" -> "]", "<" -> ">")
  val closingCharSeq = Array(")", "}", "]", ">")
  val openCharScore = Map(")" -> 3, "}" -> 1197, "]" -> 57, ">" -> 25137)
  val closingCharScore = Map(")" -> 1, "}" -> 3, "]" -> 2, ">" -> 4)
}
