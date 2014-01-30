package jp.naist.cl

import jp.naist.cl.gw._

import java.io.{Writer, PrintWriter}


object Opts {
  import org.rogach.scallop._

  def apply(args: Array[String]) = new ScallopConf(args) {

    val corpus = opt[String]("corpus", descr="Corpus directory", required=true)

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

  def makeVocab(words: Iterator[String], trimVocab: Int) = {
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

    vocab + ("UNK" -> (vocab.size + 1, unkCount))
  }

  def writeVocab(writer: Writer, vocab: Map[String, (Int, Int)]) {
    val pw = IO.wrapWriter(writer)
    for ((w, (i, n)) <- vocab.toSeq.sortBy(_._2._1)) {
      pw.println(s"$w\t$i\t$n")
    }
  }

  def w2id(w: String, vocab: Map[String, (Int, Int)]) = vocab.getOrElse(w, vocab("UNK"))._1

  def makeLdaDataset(train: Dataset, test: Dataset, trimVocab: Int) {
    val vocab = makeVocab(train.documents.flatMap{_.tokens}.map{_.lemma}, trimVocab)
    IO.withWriter("vocab") {writeVocab(_, vocab)}

    val writeDoc = (pw: PrintWriter, doc: Document) => {
      val tokens = doc.tokens.toSeq
      val verbs = doc.verbs.toSeq
      pw.println(s"document ${tokens.length}")
      for (tok <- tokens) {
        val id = w2id(tok.lemma, vocab)
        val isverb = verbs.contains(tok)
        pw.println(s"$id $isverb")
      }
    }

    IO.withWriter("train") {
      w => {
        val pw = IO.wrapWriter(w)
        for (doc <- train.documents) {
          writeDoc(pw, doc)
        }
      }
    }

    IO.withWriter("test") {
      w => {
        val pw = IO.wrapWriter(w)
        for (doc <- test.documents) {
          writeDoc(pw, doc)
        }
      }
    }
  }

  def makeVerbTopicDataset(train: Dataset, test: Dataset, trimVocab: Int) {
    val vocab = makeVocab(train.documents.flatMap{_.verbs}.map{_.lemma}, trimVocab)
    IO.withWriter("vocab") {writeVocab(_, vocab)}

    val writeDoc = (pw: PrintWriter, doc: Document) => {
      val verbs = doc.verbs.toSeq
      pw.println(s"document ${verbs.length}")
      for (verb <- verbs) {
        pw.println(w2id(verb.lemma, vocab).toString)
      }
    }

    IO.withWriter("train") {
      w => {
        val pw = IO.wrapWriter(w)
        for (doc <- train.documents) {
          writeDoc(pw, doc)
        }
      }
    }

    IO.withWriter("test") {
      w => {
        val pw = IO.wrapWriter(w)
        for (doc <- test.documents) {
          writeDoc(pw, doc)
        }
      }
    }
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

  def makeVerbChainDataset(train: Dataset, test: Dataset, trimVocab: Int) {
    val verbVocab = makeVocab(train.documents.flatMap{_.verbs}.map{_.lemma}, trimVocab)
    IO.withWriter("vocab") {writeVocab(_, verbVocab)}

    val writeDoc = (pw: PrintWriter, doc: Document) => {
      val chains = verbChains(doc)
      val numVerbs = chains.map{_.length}.sum
      pw.println(s"document ${chains.length} $numVerbs")

      // start with "1" to match Julia's one-based array indices
      val senOffs = doc.sentences.scanLeft(1)(_ + _.words.length)
      for ((ch,i) <- chains.zipWithIndex) {
        for (t <- ch) {
          val id = w2id(t.lemma, verbVocab)
          val tokNum = senOffs(t.index.sentence - 1) + t.index.token
          val varNum = i + 1 // +1 to match Julia index
          // <word> <token index (for sorting purposes during L2R approx.)> <index of latent variable this depends on>
          pw.println(s"$id $tokNum $varNum")
        }
      }
    }

    IO.withWriter("train") {
      w => {
        val pw = IO.wrapWriter(w)
        for (doc <- train.documents) {
          writeDoc(pw, doc)
        }
      }
    }

    IO.withWriter("test") {
      w => {
        val pw = IO.wrapWriter(w)
        for (doc <- test.documents) {
          writeDoc(pw, doc)
        }
      }
    }
  }

  def verbArgs(doc: Document, verb: Token) = {
    doc.sentence(verb.index).daughters(verb.index)
      .map{dep => doc(dep.daughter)}
      .filter{_.pos(0) == 'N'}
  }

  def makeVerbArgsDataset(train: Dataset, test: Dataset, trimVocab: Int) {
    val verbVocab = makeVocab(train.documents.flatMap{_.verbs}.map{_.lemma}, trimVocab)
    IO.withWriter("verb-vocab") {writeVocab(_, verbVocab)}

    val argVocab = makeVocab(train.documents.map{d => d.verbs.map{verbArgs(d, _)}}.flatten.flatten.map{_.lemma}, trimVocab)
    IO.withWriter("arg-vocab") {writeVocab(_, argVocab)}

    val writeDoc = (pw: PrintWriter, doc: Document) => {
      val verbs = doc.verbs.toSeq
      val argLists = verbs.map{verbArgs(doc, _)}
      val numArgs = argLists.map{_.length}.sum

      pw.println(s"document ${verbs.length} $numArgs")

      // start with "1" to match Julia's one-based array indices
      val senOffs = doc.sentences.scanLeft(1)(_ + _.words.length)

      for (vb <- verbs) {
        val id = w2id(vb.lemma, argVocab)
        val tokNum = senOffs(vb.index.sentence - 1) + vb.index.token
        pw.println(s"$id $tokNum")
      }

      for ((as, i) <- argLists.zipWithIndex) {
        for (a <- as) {
          val id = w2id(a.lemma, argVocab)
          val tokNum = senOffs(a.index.sentence - 1) + a.index.token
          val varNum = i + 1
          pw.println(s"$id $tokNum $varNum")
        }
      }
    }

    IO.withWriter("train") {
      w => {
        val pw = IO.wrapWriter(w)
        for (doc <- train.documents) {
          writeDoc(pw, doc)
        }
      }
    }

    IO.withWriter("test") {
      w => {
        val pw = IO.wrapWriter(w)
        for (doc <- test.documents) {
          writeDoc(pw, doc)
        }
      }
    }
  }
}
