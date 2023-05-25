package com.phasmidsoftware.core

case class SmartBuffer(sb: StringBuilder) {

    /**
     * Method to append a String (delegates to sb.append)
     *
     * @param s the String to be appended.
     */
    def append(s: String): SmartBuffer = {
        sb.append(s)
        this
    }

    /**
     * Return tru if sb ends with a delimiter such as ">"
     *
     * @return
     */
    private def delim: Boolean = {
        val result = sb.result()
        result.endsWith(">") || result.endsWith("{")
    }

    /**
     * Method to append a String (delegates to sb.append) with padding if necessary.
     *
     * @param s the String to be appended.
     * @return this SmartBuffer (mutated)
     */
    def appendPadded(s: String): SmartBuffer = {
        if (sb.nonEmpty && !delim && s.nonEmpty && !s.startsWith("\n")) sb.append(" ")
        sb.append(s)
        this
    }

    def clear: String = {
        val s = result
        sb.clear()
        s
    }

    def result: String = sb.result()

    override def toString: String = sb.toString
}

object SmartBuffer {
    def apply(): SmartBuffer = new SmartBuffer(new StringBuilder())
}
