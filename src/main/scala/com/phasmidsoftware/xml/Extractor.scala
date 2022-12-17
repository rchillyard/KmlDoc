package com.phasmidsoftware.xml

import com.phasmidsoftware.core.Utilities.show
import com.phasmidsoftware.core.XmlException
import com.phasmidsoftware.xml.Extractors.{extractOptional, extractSingleton}
import org.slf4j.{Logger, LoggerFactory}
import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
import scala.xml.{Node, NodeSeq}

/**
 * Trait to define the behavior of a type which can be constructed from an XML Node.
 *
 * @tparam T the type to be constructed.
 */
trait Extractor[T] {
    self =>

    /**
     * Method to convert a Node into a Try[T].
     *
     * @param node a Node.
     * @return a Try[T].
     */
    def extract(node: Node): Try[T]

    /**
     * Method to create an Extractor[T] such that, if this Extractor[T] fails, then we invoke the (implicit) Extractor[P] instead.
     *
     * @tparam P the type of the alternative Extractor. P must provide implicit evidence of Extractor[P] and P must be a sub-class of T.
     * @return an Extractor[T].
     */
    def orElse[P <: T : Extractor](): Extractor[T] = (node: Node) => self.extract(node) orElse implicitly[Extractor[P]].mapTo[T].extract(node)

    /**
     * Method to create an Extractor[P] which instantiates a Try[T] but treats it as a Try[P] where P is a super-class of T.
     * CONSIDER why does this work when Extractor[T] is not defined with T as covariant?
     *
     * @tparam P the type of the Extractor we wish to return.
     * @return an Extractor[P].
     */
    private def mapTo[P >: T]: Extractor[P] = (node: Node) => self.extract(node)
}

/**
 * Companion object to Extractor.
 */
object Extractor {

    /**
     * Method to yield a Try[P] for a particular child or attribute of the given node.
     *
     * NOTE: Plural members should use extractChildren and not extractField.
     *
     * NOTE: ideally, this should be private but is used for testing and the private method tester is struggling.
     *
     * @param field the name of a member field:
     *              if the (text) content of the node, then the field should be "$";
     *              if a singleton child, then field is as is;
     *              if an attribute, then field should begin with "_";
     *              if an optional child, then field should begin with "maybe".
     * @param node  a Node whence field is to be extracted.
     * @tparam P the type to which Node should be converted [must be Extractor].
     * @return a Try[P].
     */
    def extractField[P: Extractor](field: String)(node: Node): Try[P] = doExtractField[P](field, node) match {
        case _ -> Success(p) => Success(p)
        case m -> Failure(x) =>
            x match {
                case _: NoSuchFieldException => Success(None.asInstanceOf[P])
                case _ =>
                    val message = s"extractField ($m): field '$field' from node:\n    {${show(node)}}"
                    logger.warn(s"$message caused by $x")
                    Failure(XmlException(message, x))
            }
    }

    /**
     * Method to extract child elements from a node.
     *
     * @param member the name of the element(s) to extract, according to the construct function (typically, this means the name of the member in a case class).
     * @param node   the node from which we want to extract.
     * @tparam P the (MultiExtractor-able) underlying type of the result.
     * @return a Try[P].
     */
    def extractChildren[P: MultiExtractor](member: String)(node: Node): Try[P] = {
        val w = translateMemberName(member)
        val nodeSeq = node \ w
        if (nodeSeq.isEmpty) logger.info(s"extractChildren: no children found for child $w (for member $member) in ${show(node)}")
        implicitly[MultiExtractor[P]].extract(nodeSeq)
    }

    /**
     * Method to create an Extractor[T] which always fails.
     *
     * @tparam T the underlying type of the result.
     * @return a failing Extractor[T].
     */
    def none[T]: Extractor[T] = (_: Node) => Failure(new NoSuchElementException)


    val translations: mutable.HashMap[String, String] = new mutable.HashMap[String, String]()

    private def translateMemberName(member: String): String =
        translations.getOrElse(member,
            member match {
                case plural(x) => x
                case _ => translations.getOrElse(member, member)
            })

    private def doExtractField[P: Extractor](field: String, node: Node) = field match {
        // NOTE special name for the (text) content of a node.
        case "$" => "$" -> extractText[P](node)
        // NOTE attributes must match names where the case class member name starts with "_"
        case attribute("xmlns") => "attribute xmlns" -> Failure(XmlException("it isn't documented by xmlns is a reserved attribute name"))
        case optionalAttribute(x) => s"optional attribute: $x" -> extractAttribute[P](node, x, optional = true)
        case attribute(x) => s"attribute: $x" -> extractAttribute[P](node, x)
        // NOTE child nodes are extracted using extractChildren, not here.
        case plural(x) => s"plural:" -> Failure(XmlException(s"extractField: incorrect usage for plural field: $x. Use extractChildren instead."))
        // NOTE optional members such that the name begins with "maybe"
        case optional(x) => s"optional: $x" -> extractOptional[P](node \ x)
        // NOTE this is the default case which is used for a singleton entity (plural entities would be extracted using extractChildren).
        case x => s"singleton: $x" -> extractSingleton[P](node \ x)
    }

    private def extractAttribute[P: Extractor](node: Node, x: String, optional: Boolean = false): Try[P] =
        (for (ns <- node.attribute(x)) yield for (n <- ns) yield implicitly[Extractor[P]].extract(n)) match {
            case Some(py :: Nil) => py
            case _ if optional => Failure(new NoSuchFieldException)
            case _ => Failure(XmlException(s"failure to retrieve unique attribute $x from node ${show(node)}"))
        }

    /**
     * Regular expression to match a plural name, viz. .....s
     */
    val plural: Regex = """(\w+)s""".r

    /**
     * Regular expression to match an attribute name, viz. _.....
     */
    val attribute: Regex = """_(\w+)""".r

    /**
     * Regular expression to match an optional attribute name, viz. &#95;&#95;.....
     * With an optional attribute, it will have a default value that does not need to be overridden.
     */
    val optionalAttribute: Regex = """__(\w+)""".r

    /**
     * Regular expression to match an optional name, viz. maybe....
     */
    val optional: Regex = """maybe(\w+)""".r

    private def extractText[P: Extractor](node: Node): Try[P] = implicitly[Extractor[P]].extract(node)

    val logger: Logger = LoggerFactory.getLogger(Extractor.getClass)
}

/**
 * Trait to define the behavior of an iterable type which can be constructed from an XML NodeSeq.
 *
 * @tparam T the (iterable) type to be constructed.
 */
trait MultiExtractor[T] {
    /**
     * Method to convert a NodeSeq into a Try[T], usually an iterable type.
     *
     * @param nodeSeq a NodeSeq.
     * @return a Try[T].
     */
    def extract(nodeSeq: NodeSeq): Try[T]
}
