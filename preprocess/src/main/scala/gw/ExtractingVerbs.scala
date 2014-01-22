package jp.naist.cl
package gw


//trait for document processors that may modify sentences
trait DocumentProcessor extends (Document => Document) {
  def apply(d: Document): Document = processDocument(d)

  def processDocument(d: Document): Document =
    new Document(d.sentences.map{processSentence(_)}, d.entities, d.id)

  def processSentence(s: Sentence): Sentence = s
}

//document processor that uses `extract` to add verbs to Sentence
class VerbExtractorProcessor(val extract: Sentence => Seq[Token]) extends DocumentProcessor {
  override def processSentence(s: Sentence) = s.copy(verbs = extract(s).map{_.index})
}


//holds ready-made verb extractors
object VerbExtractors {
  def isverb(t: Token) = t.pos(0) == 'V'

  lazy val allverbs = new VerbExtractorProcessor(s => s.words.filter{isverb(_)})
  lazy val rootverbs = new VerbExtractorProcessor(s => {val r = s(s.root); if (isverb(r)) Seq(r) else Seq()})
  lazy val recursive = new VerbExtractorProcessor(RecursiveExtractor)
}


//recursively extracts verbs
object RecursiveExtractor extends (Sentence => Seq[Token]) {
  import VerbExtractors.isverb

  def apply(s: Sentence): Seq[Token] = {
    val root = s(s.root)

    if (isverb(root)) {
      extract(s, List(root), children(s, root)).reverse
    } else {
      Seq()
    }
  }

  private def children(s: Sentence, head: Token): List[Token] = 
    s.daughters(head.index)
      .filter{d => filter(s(d.daughter), d, head)}
      .map{d => s(d.daughter)}
      .filter{_ != head}
      .distinct
      .toList

  private def extract(s: Sentence, verbs: List[Token], pending: List[Token]): List[Token] = {
    pending match {
      case List() => verbs
      case t :: rest =>
        extract(
          s,
          t :: verbs,
          rest ++ children(s, t).filter{!verbs.contains(_)})
    }
  }

  private def filter(t: Token, d: Dependency, h: Token): Boolean = {
    isverb(t) && d.label.take(3) != "aux"
  }
}
