package jp.naist.cl
package gw

import edu.jhu.agiga.{AgigaPrefs, StreamingDocumentReader, AgigaDocument}


object NormalizeTokens extends DocumentProcessor {
  override def processSentence(s: Sentence) = {
    s.copy(words = s.words.map{TokenNormalizer(_)})
  }
}


class Dataset(
  val xmlFiles: Seq[String],
  val process: Option[Document => Document] = None) {

  def documents : Iterator[Document] =
    xmlFiles
      .iterator
      .flatMap{agDocumentIterator(_)}
      .map{ag2doc(_)}
      .map{d => process.map{_(d)}.getOrElse(d)}

  private def ag2doc(ag: AgigaDocument): Document = {
    import scala.collection.JavaConversions._

    val sentences = Vector() ++ ag.getSents.map{
      s => {
        val tokens = Vector() ++ s.getTokens.map{
          t => Token(
            //the agiga java api indices are 0-based
            TokenIndex(s.getSentIdx + 1, t.getTokIdx + 1),
            t.getWord,
            t.getLemma,
            t.getPosTag,
            t.getNerTag match { case "O" => None; case s => Some(s) }
          )
        }
        val dependencies = s.getColCcprocDeps.filter{_.getType != "root"}.map{
          d => Dependency(
            tokens(d.getGovIdx).index,
            tokens(d.getDepIdx).index,
            d.getType
          )
        }.toList
        val rootidx = s.getColCcprocDeps.find{_.getType == "root"}.map{_.getDepIdx}.getOrElse(0)
        val root = tokens(rootidx).index

        Sentence(tokens, dependencies, root)
      }
    }

    val entities = ag.getCorefs.map{
      _.getMentions.map{
        m => {
          val s = sentences(m.getSentenceIdx)
          val first = s.words(m.getStartTokenIdx).index
          val last = s.words(m.getEndTokenIdx - 1).index //agiga index is one-past-end
          val head = s.words(m.getHeadTokenIdx).index
          val rep = m.isRepresentative
          Mention(first, last, head, rep)
        }
      }.toList
    }.map{Entity(_)}.toList

    val id = GigawordId(ag.getDocId)

    new Document(sentences, entities, id)
  }

  private def agDocumentIterator(xmlFile: String): Iterator[AgigaDocument] = {
    import scala.collection.JavaConversions._

    val prefs = new AgigaPrefs()
    prefs.setParse(false)
    prefs.setBasicDeps(false)

    val reader = new StreamingDocumentReader(xmlFile, prefs)
    reader.iterator.filter{_.getType == "story"}
  }
}
