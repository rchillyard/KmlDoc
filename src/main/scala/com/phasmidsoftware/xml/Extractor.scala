package com.phasmidsoftware.xml

import com.phasmidsoftware.core.Utilities.{lensFilter, renderNode, renderNodes, sequence}
import com.phasmidsoftware.core.XmlException
import com.phasmidsoftware.flog.Flog
import com.phasmidsoftware.xml.Extractors.{extractOptional, extractSingleton}
import com.phasmidsoftware.xml.NamedFunction.name
import org.slf4j.{Logger, LoggerFactory}
import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
import scala.xml.{Node, NodeSeq}

/**
 * Trait to define the behavior of a type which can be constructed from an XML Node.
 *
 * @tparam T the type to be constructed.
 */
trait Extractor[T] extends NamedFunction[Extractor[T]] {
    self =>

    /**
     * Method to convert a Node into a Try[T].
     *
     * @param node a Node.
     * @return a Try[T].
     */
    def extract(node: Node): Try[T]

    /**
     * Method to transform this Extractor[T] into an Extractor[U].
     *
     * @param f a T => U.
     * @tparam U the underlying type of the result.
     * @return an Extractor[U].
     */
    def map[U](f: T => U): Extractor[U] = (node: Node) => self.extract(node) map f

    /**
     * Method to create an Extractor[T] such that, if this Extractor[T] fails, then we invoke the (implicit) Extractor[P] instead.
     *
     * @tparam P the type of the alternative Extractor. P must provide implicit evidence of Extractor[P] and P must be a sub-class of T.
     * @return an Extractor[T].
     */
    def |[P <: T : Extractor](): Extractor[T] = (node: Node) => self.extract(node) orElse implicitly[Extractor[P]].mapTo[T].extract(node)

    /**
     * Method to create an Extractor[P] which instantiates a Try[T] but treats it as a Try[P] where P is a super-class of T.
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

    val flog: Flog = Flog[Extractors]

    import flog._

    /**
     * Method to create an Extractor[T] from a Node => Try[T] function.
     * Note that this isn't strictly necessary because of the SAM conversion mechanism which turns a Node => Try[T] function into an Extractor[T].
     *
     * @param f a Node => Try[T] function.
     * @tparam T the underlying type of the resulting Extractor.
     * @return an Extractor[T].
     */
    def apply[T](f: Node => Try[T]): Extractor[T] = new Extractor[T] {
        def extract(node: Node): Try[T] = /* "Extractor.apply(f)" !? */ f(node)
    } ^^ "Extractor.apply(f)"

    /**
     * Method to create an Extractor[T] such that the result of the extraction is always a constant, regardless of what's in the node provided.
     *
     * @param ty a Try[T].
     * @tparam T the underlying type of the result.
     * @return an Extractor[T] which always produces ty when extract is invoked on it.
     */
    def apply[T](ty: => Try[T]): Extractor[T] = Extractor(_ => ty) ^^ s"Extractor.apply($ty)"

    /**
     * Method to extract a Try[T] from the implicitly defined extractor operating on the given node.
     *
     * @param node the node on which the extractor will work.
     * @tparam T the underlying result type and which provides (implicit) evidence of an Extractor[T].
     * @return a Try[T].
     */
    def extract[T: Extractor](node: Node): Try[T] =
//        implicitly[Extractor[T]].extract(node)
        s"extract: ${name[Extractor[T]]} from ${renderNode(node)}" !? implicitly[Extractor[T]].extract(node)

    /**
     * Method to extract a Try[T] from the implicitly defined multi-extractor operating on the given nodes.
     * Usually, T is itself an Iterable type.
     *
     * @param nodeSeq the nodes on which the extractor will work.
     * @tparam T the underlying result type and which provides (implicit) evidence of a MultiExtractor[T].
     * @return a Try[T].
     */
    def extractMulti[T: MultiExtractor](nodeSeq: NodeSeq): Try[T] =
        s"extractMulti: ${name[MultiExtractor[T]]} from ${renderNodes(nodeSeq)}" !!
                implicitly[MultiExtractor[T]].extract(nodeSeq)

    /**
     * Method to extract all possible Try[T] from the implicitly defined multi-extractor operating on the given nodes.
     * Usually, T is itself an Iterable type.
     *
     * @param node the node on which the extractor will work--it will extract from all the node's children.
     * @tparam T the underlying result type and which provides (implicit) evidence of a MultiExtractor[T].
     * @return a Try[T].
     */
    def extractAll[T: MultiExtractor](node: Node): Try[T] = extractMulti(node / "_")

    /**
     * Method to yield a Try[P] for a particular child or attribute of the given node.
     *
     * NOTE: Plural members should use extractChildrenDeprecated and not extractField.
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
        case _ -> Success(p) => //s"extractField($field)(${renderNode(node)})(${name[Extractor[P]]})" !?
            Success(p)
        case m -> Failure(x) =>
            x match {
                case _: NoSuchFieldException => Success(None.asInstanceOf[P])
                case _ =>
                    val message = s"extractField(field=$field) from node (${renderNode(node)}) using (${implicitly[Extractor[P]].name}): (field type = $m)"
                    logger.warn(s"$message caused by $x")
                    Failure(XmlException(message, x))
            }
    }

    /**
     * Method to extract child elements from a node.
     * It is acceptable
     *
     * This method is deprecated because specifying member here is counter-productive.
     *
     * @param member the name of the element(s) to extract, according to the construct function (typically, this means the name of the member in a case class).
     * @param node   the node from which we want to extract.
     * @tparam P the (MultiExtractor-able) underlying type of the result.
     * @return a Try[P].
     */
    @deprecated
    def extractChildrenDeprecated[P: MultiExtractor](member: String)(node: Node): Try[P] = {
        // TODO use Flog logging
        val ts = translateMemberNames(member)
        logger.debug(s"extractChildrenDeprecated(${name[MultiExtractor[P]]})($member)(${renderNode(node)}): get $ts")
        if (ts.isEmpty) logger.warn(s"extractChildrenDeprecated: logic error: no suitable tags found for children of member $member in ${renderNode(node)}")
        val nodeSeq: Seq[Node] = for (t <- ts; w <- node / t) yield w
        if (nodeSeq.nonEmpty) {
            logger.info(s"extractChildrenDeprecated extracting ${nodeSeq.size} nodes for ($member)")
            extractMulti(nodeSeq)
        }
        else {
            logger.info(s"extractChildrenDeprecated: no children matched any of $ts in ${renderNode(node)}")
            Try(Nil.asInstanceOf[P])
        }
    }

    /**
     * Method to extract a Seq or Try[P] from a NodeSeq, by filtering on the given label.
     *
     * @param nodeSeq the nodes whence to extract.
     * @param label   the label to match.
     * @tparam P the underlying (Extractor) type of the result.
     * @return a Seq of Try[P].
     */
    def extractElementsByLabel[P: Extractor](nodeSeq: NodeSeq, label: String): Seq[Try[P]] =
        for (node <- lensFilter[Node, String](_.label)(label)(nodeSeq)) yield extract[P](node)

    /**
     * Method to create an Extractor[T] which always fails.
     *
     * @tparam T the underlying type of the result.
     * @return a failing Extractor[T].
     */
    def none[T]: Extractor[T] = Extractor(Failure(new NoSuchElementException))

    // CONSIDER removing this as it appears to be useless.
    // TODO make this immutable.
    val translations: mutable.HashMap[String, Seq[String]] = new mutable.HashMap()

    def expandTranslations(labels: Seq[String]): Seq[String] = for (label <- labels; z <- translateMemberNames(label)) yield z

    private def translateMemberNames(member: String): Seq[String] =
        translations.getOrElse(member,
            member match {
                case plural(x) => Seq(x)
                case _ => translations.getOrElse(member, Seq(member))
            })

    private def doExtractField[P: Extractor](field: String, node: Node): (String, Try[P]) =
        field match {
            // NOTE special name for the (text) content of a node.
            case "$" => "$" -> extractText[P](node)
            // NOTE attributes must match names where the case class member name starts with "_"
            case attribute("xmlns") => "attribute xmlns" -> Failure(XmlException("it isn't documented by xmlns is a reserved attribute name"))
            case optionalAttribute(x) => s"optional attribute: $x" -> extractAttribute[P](node, x, optional = true)
            case attribute(x) => s"attribute: $x" -> extractAttribute[P](node, x)
            // NOTE child nodes are extracted using extractChildrenDeprecated, not here.
            case plural(x) => s"plural:" -> Failure(XmlException(s"extractField: incorrect usage for plural field: $x. Use extractChildrenDeprecated instead."))
            // NOTE optional members such that the name begins with "maybe"
            case optional(x) =>
                val y = x.head.toLower + x.tail
                s"optional: $y" -> extractOptional[P](node / y)
            // NOTE this is the default case which is used for a singleton entity (plural entities would be extracted using extractChildrenDeprecated).
            case x => s"singleton: $x" -> extractSingleton[P](node / x)
        }

    private def extractAttribute[P: Extractor](node: Node, x: String, optional: Boolean = false): Try[P] =
        (for (ns <- node.attribute(x)) yield for (n <- ns) yield Extractor.extract[P](n)) match {
            case Some(py :: Nil) => py
            case _ if optional => Failure(new NoSuchFieldException)
            case _ => Failure(XmlException(s"failure to retrieve unique attribute $x from node ${renderNode(node)}"))
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
     *
     * TODO set the first character that is matched to lower case here.
     */
    val optional: Regex = """maybe(\w+)""".r

    private def extractText[P: Extractor](node: Node): Try[P] = Extractor.extract[P](node)

    /**
     * Unit extractor.
     */
    implicit val unitExtractor: Extractor[Unit] = Extractor(Success())

    /**
     * String extractor.
     */
    implicit object stringExtractor extends Extractor[String] {
        def extract(node: Node): Try[String] = Success(node.text)
    }

    /**
     * Int extractor.
     */
    implicit val intExtractor: Extractor[Int] = stringExtractor map (_.toInt)

    /**
     * Boolean extractor.
     */
    implicit val booleanExtractor: Extractor[Boolean] = stringExtractor map {
        case "true" | "yes" | "T" | "Y" => true
        case _ => false
    }

    /**
     * Double extractor.
     */
    implicit val doubleExtractor: Extractor[Double] = stringExtractor map (_.toDouble)

    /**
     * Long extractor.
     */
    implicit val longExtractor: Extractor[Long] = stringExtractor map (_.toLong)

    val logger: Logger = LoggerFactory.getLogger(Extractor.getClass)
}

/**
 * Trait to define the behavior of an iterable type which can be constructed from an XML NodeSeq.
 *
 * @tparam T the (iterable) type to be constructed.
 */
trait MultiExtractor[T] extends NamedFunction[MultiExtractor[T]] {
    /**
     * Method to convert a NodeSeq into a Try[T], usually an iterable type.
     * This method will typically be used on the result of: <code>node \ tag</code> where tag is a particular tag String.
     *
     * @param nodeSeq a NodeSeq.
     * @return a Try[T].
     */
    def extract(nodeSeq: NodeSeq): Try[T]
}

/**
 * Trait which extends a function of type String => Extractor[T].
 * When the apply method is invoked with a particular label, an appropriate Extractor[T] is returned.
 *
 * CONSIDER renaming this because it isn't an extractor, but a function which creates an extractor from a String.
 *
 * @tparam T the underlying type of the result of invoking apply. T may be an Iterable type.
 */
trait TagToExtractorFunc[T] extends (String => Extractor[T]) {

    /**
     * Method to yield an Extractor[T], given a label.
     *
     * @param label the label of a node or sequence of nodes we wish to extract.
     * @return an Extractor[T].
     */
    def apply(label: String): Extractor[T]
}

/**
 * Trait which extends TagToExtractorFunc for an underlying sequence type.
 *
 * @tparam T the underlying type of the resulting sequence when invoking apply.
 */
trait TagToSequenceExtractorFunc[T] extends TagToExtractorFunc[Seq[T]] {
    val tags: Seq[String]

    val pseudo: String

    def valid(w: String): Boolean = w == pseudo

    val tsm: MultiExtractor[Seq[T]]

    def extract(node: Node): Try[Seq[T]] = sequence(for (tag <- tags) yield tsm.extract(node \ tag)) map (_.flatten)
}

/**
 * Class which extends an Extractor of Seq[T].
 *
 * @param labels a set of labels (tags)
 * @param tsm    an (implicit) MultiExtractor of Seq[T].
 * @tparam T the type to be constructed.
 */
class SubclassExtractor[T](val labels: Seq[String])(implicit tsm: MultiExtractor[Seq[T]]) extends Extractor[Seq[T]] {
    def extract(node: Node): Try[Seq[T]] = sequence(for (label <- labels) yield tsm.extract(node \ label)) map (_.flatten)
}
