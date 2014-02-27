package jp.naist.cl

import java.io.{File, Writer, FileWriter, PrintWriter, BufferedWriter, Reader, FileReader, BufferedReader}

object Using {
  def using[A, B <: {def close(): Unit}] (closeable: B) (f: B => A): A =
    try { f(closeable) } finally { closeable.close() }

  def using[A, B <: {def close(): Unit}] (first: B, second: B) (f: (B,B) => A): A =
    try { f(first, second) } finally { first.close(); second.close() }
}

class Path private (private val f: File) {
  type Writer = PrintWriter
  type Reader = BufferedReader

  def exists = f.exists

  def path = f.getPath

  def /(child: String): Path = Path(path, child)

  def files: Seq[Path] = f.listFiles.filter(_.isFile).map{new Path(_)}.toSeq

  def writer: Writer = new PrintWriter(new BufferedWriter(new FileWriter(f)))
  def reader: Reader = new BufferedReader(new FileReader(f))

  override def toString = path
}

object Path {
  def apply() = {
    new Path(new File("."))
  }

  def apply(path: String) = {
    new Path(new File(path))
  }

  def apply(first: String, second: String, rest: String*) = {
    new Path(rest.foldLeft(new File(first, second))(new File(_, _)))
  }
}


object IO {
  import Using.using

  def withWriter[A](filename: String)(f: Writer => A): A = 
    using(new PrintWriter(new BufferedWriter(new FileWriter(filename)))) { f(_) }

  def wrapWriter(w: Writer): PrintWriter =
    if (w.isInstanceOf[PrintWriter]) w.asInstanceOf[PrintWriter] else new PrintWriter(w)

  def lines(r: Reader): Iterator[String] = new Iterator[String] {
    val br = if (r.isInstanceOf[BufferedReader]) r.asInstanceOf[BufferedReader] else new BufferedReader(r)
    var cur = br.readLine

    override def hasNext = cur != null

    override def next = {
      require(hasNext)
      val ret = cur
      cur = br.readLine
      ret
    }
  }
  def lines(p: Path): Iterator[String] = lines(p.reader)

  def mkdir(path: String) {
    new File(path).mkdirs()
  }
}
