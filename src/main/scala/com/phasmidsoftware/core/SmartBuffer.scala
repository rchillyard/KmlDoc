package com.phasmidsoftware.core

case class SmartBuffer(sb: StringBuilder) {

  /**
   * Method to append a String (delegates to sb.append)
   *
   * @param s the String to be appended.
   */
  def append(s: String): SmartBuffer = {
    val delimAppend = s.startsWith("\n")
    if (delimAppend) trim
    sb.append(s)
    if (sb.result().contains(" \n"))
      println(s"SmartBuffer: ${sb.result()}") // XXX what's going on here?
    this
  }

  /**
   * Method to append a String (delegates to sb.append) with padding if necessary.
   *
   * @param s the String to be appended.
   * @return this SmartBuffer (mutated)
   */
  def appendPadded(s: String): SmartBuffer = {
    if (sb.nonEmpty && !delimExist && s.nonEmpty && !s.startsWith("\n")) sb.append(" ")
    append(s)
  }

  def trim: SmartBuffer = {
    SmartBuffer.trimStringBuilder(sb)
    this
  }

  def clear: String = {
    val s = result
    sb.clear()
    s
  }

  /**
   * Get the resulting String.
   *
   * NOTE: this method contains a hack which collapses output of the form &lt;tag ...&gt;&lt;/tag&gt; into &lt;tag .../&gt;
   * As such, it is useful for e.g. hotSpot which has no children.
   *
   * NOTE: the method assumes there is at most one newline character at the beginning of the string.
   *
   * @return
   */
  def result: String = {
    val str = sb.result()
    if (str.isEmpty) str
    else {
      val regex = """(\s*)<([a-zA-Z0-9 ="]+)></(\w+)>""".r
      str.substring(1) match {
        case regex(p, q, _) => str.substring(0, 1) + p + "<" + q + "/>"
        case _ => str
      }
    }
  }

  override def toString: String = sb.toString

  /**
   * Return tru if sb ends with a delimiter such as ">"
   *
   * @return
   */
  private def delimExist: Boolean = {
    val result = sb.result()
    result.endsWith(">") || result.endsWith("{")
  }

}

object SmartBuffer {
  def apply(): SmartBuffer = new SmartBuffer(new StringBuilder())

  /**
   * CONSIDER a faster way to do this
   */
  def trimStringBuilder(sb: StringBuilder): Unit = {
    while (sb.endsWith(" ")) sb.setLength(sb.length() - 1)
  }

}
