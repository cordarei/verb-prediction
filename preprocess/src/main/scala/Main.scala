package jp.naist.cl

import jp.naist.cl.gw._
import jp.naist.cl.Using._

import java.io.{Writer, PrintWriter}


class Vocab(inner: Map[String, (Int, Int)]) {
  def apply(word: String): Int = inner.getOrElse(word, inner("UNK"))._1
  def word(s: String): String = if (inner.contains(s)) s else "UNK"
  def toSeq = inner.toSeq.map{case (w, (i, n)) => (w, i, n)}.sortBy(_._2)
}

object Vocab {
  def apply(words: Iterator[String], trimVocab: Int) = {
    val counts = new collection.mutable.HashMap[String, Int]()
    for (w <- words) {
      counts += w -> (counts.getOrElse(w, 0) + 1)
    }
    val unkCount = counts.view.filter{case (w, n) => n < trimVocab}.map{_._2}.sum

    val vocab = counts
      .toSeq
      .sortBy(_._2)(Ordering[Int].reverse)
      .filter{case (w, n) => n >= trimVocab}
      .zipWithIndex
      .map{case ((w, n), i) => (w, (i+1, n))}
      .toMap

    new Vocab(vocab + ("UNK" -> (vocab.size + 1, unkCount)))
  }
}


trait DocumentWriter {

  def writeDoc(pw: PrintWriter, doc: Document, iw: PrintWriter)

  def writeIndex(pw: PrintWriter, doc: Document, tok: Token, vocab: Vocab, index: String) {
    pw.println((instanceIndexParts(doc, tok, vocab) :+ index) mkString "\t")
  }

  def instanceIndexParts(doc: Document, tok: Token, vocab: Vocab) = {
    List(doc.id.toString, tok.index.sentence.toString, tok.index.token.toString, vocab.word(tok.lemma), tok.pos)
  }
}

class LdaWriter(val vocab: Vocab) extends DocumentWriter {
  var d = 1
  def writeDoc(pw: PrintWriter, doc: Document, iw: PrintWriter) {
    val tokens = doc.tokens.toSeq
    val verbs = doc.verbs.toSeq
    pw.println(s"document ${tokens.length}")
    var i = 1
    for (tok <- tokens) {
      val id = vocab(tok.lemma)
      val isverb = verbs.contains(tok)
      pw.println(s"$id $isverb")

      if (isverb) {
        writeIndex(iw, doc, tok, vocab, s"$d $i")
      }
      i = i + 1
    }

    d = d + 1
  }
}

class VerbOnlyWriter(val vocab: Vocab) extends DocumentWriter {
  var d = 1
  def writeDoc(pw: PrintWriter, doc: Document, iw: PrintWriter) {
    val verbs = doc.verbs.toSeq
    pw.println(s"document ${verbs.length}")
    var i = 1
    for (verb <- verbs) {
      pw.println(vocab(verb.lemma).toString)
      writeIndex(iw, doc, verb, vocab, s"$d $i")
      i = i + 1
    }

    d = d + 1
  }
}

class VerbArgWriter(val verbVocab: Vocab, val argVocab: Vocab) extends DocumentWriter {
  var d = 1
  def writeDoc(pw: PrintWriter, doc: Document, iw: PrintWriter) {
    val verbs = doc.verbs.toSeq
    val argLists = verbs.map{VerbArgs(doc, _)}
    val numArgs = argLists.map{_.length}.sum

    pw.println(s"document ${verbs.length} $numArgs")

    // start with "1" to match Julia's one-based array indices
    val senOffs = doc.sentences.scanLeft(1)(_ + _.words.length)

    var i = 1
    for (vb <- verbs) {
      val id = verbVocab(vb.lemma)
      val tokNum = senOffs(vb.index.sentence - 1) + vb.index.token
      pw.println(s"$id $tokNum")
      writeIndex(iw, doc, vb, verbVocab, s"$d $i")
      i = i + 1
    }

    for ((as, i) <- argLists.zipWithIndex) {
      for (a <- as) {
        val id = argVocab(a.lemma)
        val tokNum = senOffs(a.index.sentence - 1) + a.index.token
        val varNum = i + 1
        pw.println(s"$id $tokNum $varNum")
      }
    }
    d = d + 1
  }
}

object VerbArgs {
  def apply(doc: Document, verb: Token) = {
    doc.sentence(verb.index).daughters(verb.index)
      .map{dep => doc(dep.daughter)}
      .filter{_.pos(0) == 'N'}
  }
}

class VerbChainWriter(val vocab: Vocab) extends DocumentWriter {
  var d = 1
  def writeDoc(pw: PrintWriter, doc: Document, iw: PrintWriter) {
    val chains = verbChains(doc)
    val numVerbs = chains.map{_.length}.sum
    pw.println(s"document ${chains.length} $numVerbs")

    // start with "1" to match Julia's one-based array indices
    val senOffs = doc.sentences.scanLeft(1)(_ + _.words.length)
    var j = 1
    for ((ch,i) <- chains.zipWithIndex) {
      for (t <- ch) {
        val id = vocab(t.lemma)
        val tokNum = senOffs(t.index.sentence - 1) + t.index.token
        val varNum = i + 1 // +1 to match Julia index
        // <word> <token index (for sorting purposes during L2R approx.)> <index of latent variable this depends on>
        pw.println(s"$id $tokNum $varNum")
        writeIndex(iw, doc, t, vocab, s"$d $j")
        j = j + 1
      }
    }

    d = d + 1
  }

  def verbChains(doc: Document) = {
    val entities = doc.entities.map{_.mentions.map{_.head}}
    val verbs = doc.verbs.toSeq
    val entityVerbSets = entities.map{
      _.flatMap{headWordIdx => doc.sentence(headWordIdx).dependencies.filter{_.daughter == headWordIdx}}
        .map{_.head}
        .map(doc(_))
        .filter{verbs.contains}
        .toSet
    }.filter{!_.isEmpty}.distinct
    val entityVerbs = entityVerbSets.flatten
    val singleVerbs = verbs.filter{!entityVerbs.contains(_)}

    var sets = entityVerbSets
    var chains = new collection.mutable.ListBuffer[Set[Token]]()
    while (!sets.isEmpty) {
      var ent = sets.head
      sets = sets.tail
      var ints = sets.filter{!ent.intersect(_).isEmpty}
      while(!ints.isEmpty) {
        ent = ints.foldLeft(ent)(_.union(_))
        sets = sets.filter{!ints.contains(_)}
        ints = sets.filter{!ent.intersect(_).isEmpty}
      }
      chains += ent
    }

    chains.map{_.toSeq.sortBy(tok => (tok.index.sentence, tok.index.token))}.toList ++ singleVerbs.map{List(_)}
  }
}


object Opts {
  import org.rogach.scallop._

  def apply(args: Array[String]) = new ScallopConf(args) {

    val corpus = opt[String]("corpus", descr="Corpus directory", required=true)

    val summarize = toggle("summarize", default=Some(false))

    val trimVocab = opt[Int]("trimVocab", descr="Trim words with <trimVocab frequency", default=Some(2))

    val verbTypes = Set("root", "recursive", "fused", "all")
    val verbs = opt[String]("verbs", noshort=true, descr="Specify the verbs to extract", default=Some("root"), validate=verbTypes)

    val modelTypes = Set("lda", "verbonly", "verbchain", "verbargs")
    val model = opt[String]("model", descr="Specify the model to create dataset for", required=true, validate=modelTypes)

  }
}


object Main {

  def main(args: Array[String]) {
    val options = Opts(args)

    val train = makeCorpus(Path(options.corpus(), "train"), options.verbs())
    val test = makeCorpus(Path(options.corpus(), "test"), options.verbs())

    if (options.summarize()) {
      summarizeCorpus(train, test, options.verbs())
      return
    }

    options.model() match {
      case "lda" => makeLdaDataset(train, test, options.trimVocab())
      case "verbonly" => makeVerbTopicDataset(train, test, options.trimVocab())
      case "verbchain" => makeVerbChainDataset(train, test, options.trimVocab())
      case "verbargs" => makeVerbArgsDataset(train, test, options.trimVocab())
    }
  }

  def makeCorpus(dir: Path, verbs: String) = new Dataset(
    dir.files.map(_.path),
    Some(
      NormalizeTokens.andThen(verbs match {
        case "root" => VerbExtractors.rootverbs
        case "recursive" => VerbExtractors.recursive
        case "all" => VerbExtractors.allverbs
      })
    )
  )

  case class DocSummary(file: String, docid: GigawordId, sents: Int, tokens: Int, verbs: Int)

  def summarizeCorpus(train: Dataset, test: Dataset, verbs: String) {
    import Using._

    val d2file = (d:Document) => s"${d.id.section}_eng_${d.id.year}${d.id.month}.xml.gz"
    val d2sum = (d:Document) => DocSummary(d2file(d), d.id, d.sentences.length, d.tokens.length, d.verbs.length)
    val sum2s = (s:DocSummary) => s"${s.file}\t${s.docid}\t${s.sents}\t${s.tokens}\t${s.verbs}"
    val header = "XML_File_Name\tDocument_Id\tNumber_of_Sentences\tNumber_of_Tokens\tNumber_of_Verbs"

    val trainsum = train.documents map d2sum
    val testsum = test.documents map d2sum

    using(Path(s"summary.${verbs}.train").writer) { w => w.println(header); trainsum.map{sum2s}.foreach{w.println(_)} }
    using(Path(s"summary.${verbs}.test").writer) { w => w.println(header); testsum.map{sum2s}.foreach{w.println(_)} }
  }


  def writeVocab(writer: Writer, vocab: Vocab) {
    val pw = IO.wrapWriter(writer)
    for ((w, i, n) <- vocab.toSeq) {
      pw.println(s"$w\t$i\t$n")
    }
  }

  def filterDocs(docs: Iterator[Document]) = docs.filter{!_.verbs.isEmpty}

  def writeData(name: String, data: Dataset, docWriter: DocumentWriter) {
    using(Path(name).writer, Path(s"index.$name").writer) {
      (dw, iw) => {
        filterDocs(data.documents).foreach{docWriter.writeDoc(dw, _, iw)}
      }
    }
  }


  def makeLdaDataset(train: Dataset, test: Dataset, trimVocab: Int) {
    val vocab = Vocab(train.documents.flatMap{_.tokens}.map{_.lemma}, trimVocab)
    IO.withWriter("vocab") {writeVocab(_, vocab)}

    writeData("train", train, new LdaWriter(vocab))
    writeData("test", test, new LdaWriter(vocab))
  }


  def makeVerbTopicDataset(train: Dataset, test: Dataset, trimVocab: Int) {
    val vocab = Vocab(train.documents.flatMap{_.verbs}.map{_.lemma}, trimVocab)
    IO.withWriter("vocab") {writeVocab(_, vocab)}

    writeData("train", train, new VerbOnlyWriter(vocab))
    writeData("test", test, new VerbOnlyWriter(vocab))
  }


  def makeVerbChainDataset(train: Dataset, test: Dataset, trimVocab: Int) {
    val verbVocab = Vocab(train.documents.flatMap{_.verbs}.map{_.lemma}, trimVocab)
    IO.withWriter("vocab") {writeVocab(_, verbVocab)}

    writeData("train", train, new VerbChainWriter(verbVocab))
    writeData("test", test, new VerbChainWriter(verbVocab))
  }


  def makeVerbArgsDataset(train: Dataset, test: Dataset, trimVocab: Int) {
    val verbVocab = Vocab(train.documents.flatMap{_.verbs}.map{_.lemma}, trimVocab)
    IO.withWriter("verb-vocab") {writeVocab(_, verbVocab)}

    val argVocab = Vocab(train.documents.map{d => d.verbs.map{VerbArgs(d, _)}}.flatten.flatten.map{_.lemma}, trimVocab)
    IO.withWriter("arg-vocab") {writeVocab(_, argVocab)}

    writeData("train", train, new VerbArgWriter(verbVocab, argVocab))
    writeData("test", test, new VerbArgWriter(verbVocab, argVocab))
  }
}
