package jp.naist.cl
package gw


case class GigawordId(section: String, year: String, month: String, day: String, id: String) {
  override def toString = s"${section}_eng_$year$month$day.$id"
}

object GigawordId {
  val empty = GigawordId("", "", "", "", "")
  val idPattern = """(\w+)_eng_(\d\d\d\d)(\d\d)(\d\d).(\d+)""".r

  def apply(s: String): GigawordId = s.toLowerCase match {
    case idPattern(section, year, month, day, id) => GigawordId(section, year, month, day, id)
    case _ => throw new IllegalArgumentException(s)
  }
}
