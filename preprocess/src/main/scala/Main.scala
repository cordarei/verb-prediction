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

    val modelTypes = Set("lda", "verbtopic", "verbchain", "verbargs")
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
      case "verbtopic" => makeVerbTopicDataset(train, test, options.trimVocab())
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
    val vocab = new collection.mutable.HashMap[String,(Int,Int)]()
    var id = 0
    for (w <- words) {
      if (vocab.contains(w)) {
        val (i,n) = vocab(w)
        vocab += w -> (i, n+1)
      } else {
        vocab += w -> (id, 1)
        id = id + 1
      }
    }

    vocab
      .view
      .filter{case (w, (i, n)) => n >= trimVocab}
      .map{case (w, (i, n)) => (w, i)}
      .toMap + ("UNK" -> id)
  }

  def writeVocab(writer: Writer, vocab: Map[String, Int]) {
    val pw = IO.wrapWriter(writer)
    for ((w, i) <- vocab) {
      pw.println(s"$w\t$i")
    }
  }

  def w2id(w: String, vocab: Map[String, Int]) = vocab.getOrElse(w, vocab("UNK"))

  def makeLdaDataset(train: Dataset, test: Dataset, trimVocab: Int) {
    val vocab = makeVocab(train.documents.flatMap{_.tokens}.map{_.lemma}, trimVocab)
    IO.withWriter("vocab") {writeVocab(_, vocab)}

    val writeDoc = (pw: PrintWriter, doc: Document) => {
      val tokens = doc.tokens.toSeq
      pw.println(s"document ${tokens.length}")
      for (tok <- tokens) {
        pw.println(w2id(tok.lemma, vocab).toString)
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
    val entityVerbs = entities.map{
      _.flatMap{headWordIdx => doc.sentence(headWordIdx).dependencies.filter{_.daughter == headWordIdx}}
        .map{_.head}
        .map(doc(_))
        .filter{verbs.contains}
        .toSet
    }.filter{!_.isEmpty}

    val findIntersect = (sets: Seq[Set[Token]]) => {
      sets.combinations(2).filter{case List(s1,s2) => !s1.intersect(s2).isEmpty}
    }

    var chains = entityVerbs
    var ints = findIntersect(chains)
    while(!ints.isEmpty) {
      val tomerge = ints.flatten
      val nomerge = chains.filter{!tomerge.contains(_)}

      chains = nomerge ++ ints.map{case List(s1, s2) => s1.union(s2)}
      ints = findIntersect(chains)
    }

    chains.map{_.toSeq.sortBy(tok => (tok.index.sentence, tok.index.token))}
  }

  def makeVerbChainDataset(train: Dataset, test: Dataset, trimVocab: Int) {
    val verbVocab = makeVocab(train.documents.flatMap{_.verbs}.map{_.lemma}, trimVocab)
    IO.withWriter("vocab") {writeVocab(_, verbVocab)}

    val writeDoc = (pw: PrintWriter, doc: Document) => {
      val chains = verbChains(doc)
      pw.println(s"document ${chains.length}")
      for (ch <- chains) {
        pw.println(ch.map{t => w2id(t.lemma, verbVocab)}.mkString("|"))
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

  def verbArgs(doc: Document, verb: Token):Seq[String] = {
    doc.sentence(verb.index).daughters(verb.index)
      .map{dep => doc(dep.daughter)}
      .filter{_.pos(0) == 'N'}
      .map{_.lemma}
  }

  def makeVerbArgsDataset(train: Dataset, test: Dataset, trimVocab: Int) {
    val verbVocab = makeVocab(train.documents.flatMap{_.verbs}.map{_.lemma}, trimVocab)
    IO.withWriter("verb-vocab") {writeVocab(_, verbVocab)}

    val argVocab = makeVocab(train.documents.map{d => d.verbs.map{verbArgs(d, _)}}.flatten.flatten, trimVocab)
    IO.withWriter("arg-vocab") {writeVocab(_, argVocab)}

    val writeDoc = (pw: PrintWriter, doc: Document) => {
      val verbs = doc.verbs.toSeq
      val argLists = verbs.map{verbArgs(doc, _)}

      pw.println(s"document ${verbs.length}")
      for (vb <- verbs) {
        pw.println(w2id(vb.lemma, verbVocab).toString)
      }
      for (as <- argLists) {
        pw.println(as.map{w2id(_, argVocab)}.mkString("|"))
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
