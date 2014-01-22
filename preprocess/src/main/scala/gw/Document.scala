package jp.naist.cl
package gw


/** indices are 1-based */
case class TokenIndex(val sentence:Int, val token:Int)
case class Token(val index:TokenIndex, val word:String, val lemma:String, val pos:String, val ne:Option[String])
case class Dependency(val head:TokenIndex, val daughter:TokenIndex, val label:String)

case class Sentence(
  val words: IndexedSeq[Token],
  val dependencies: Seq[Dependency],
  val root: TokenIndex,
  val verbs: Seq[TokenIndex] = Seq()) {

  def apply(idx:TokenIndex): Token = {
    val token = words(idx.token - 1)
    if (token.index == idx) {
      token
    } else {
      throw new IndexOutOfBoundsException(idx.toString)
    }
  }

  def daughters(idx:TokenIndex): Seq[Dependency] = dependencies.filter{_.head == idx}
}

// token span (first, last) inclusive
// in most cases head == last
case class Mention(val first: TokenIndex, val last: TokenIndex, val head: TokenIndex, val representative: Boolean = false)
case class Entity(val mentions: Seq[Mention])

class Document(
  val sentences: IndexedSeq[Sentence],
  val entities: Seq[Entity] = Seq(),
  val id: GigawordId = GigawordId.empty) {

  def apply(idx:TokenIndex): Token = sentences(idx.sentence - 1).words(idx.token - 1)

  def sentence(idx:TokenIndex): Sentence = sentences(idx.sentence - 1)

  def tokens: Iterator[Token] = sentences.iterator.flatMap{_.words}

  def verbs: Iterator[Token] = sentences.iterator.flatMap{s => s.verbs.map{s(_)}}
}


object TokenNormalizer {
  val Punctuation = """^[^a-zA-Z0-9]+$""".r
  val Number = """^([':()+-])?([.])?\d[\d \\/()'.,:-]*(s|st|nd|rd|th)?$""".r
  val MeasureExpression = """^([0-9,.]+)(-[\w-]+)$""".r
  val Brackets = """-[LR].B-""".r

  def apply(token: Token): Token = {
    token.copy(word = normalize(token.word), lemma = normalize(token.lemma))
  }

  def apply(s: String) = normalize(s)

  def normalize(s: String): String = {
    normBrackets(s).replace("\u00A0", " ") match {
      case Punctuation() => "PUNCT"
      case Number(_,_,_) => "NUMBER"
      case MeasureExpression(num, suff) => "N" ++ suff
      case s => s.toLowerCase
    }
  }

  def normBrackets(s: String) = Brackets.replaceAllIn(s, m => if (m.matched(1) == 'L') "(" else ")")
}
