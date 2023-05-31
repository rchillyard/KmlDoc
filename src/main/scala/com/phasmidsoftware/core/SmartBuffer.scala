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
            println(s"SmartBuffer: ${sb.result()}")
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

    def result: String = sb.result()

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
