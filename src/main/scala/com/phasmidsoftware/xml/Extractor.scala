package com.phasmidsoftware.xml

import com.phasmidsoftware.core.FP.{sequence, sequenceForgiving}
import com.phasmidsoftware.core.Utilities.{lensFilter, renderNode}
import com.phasmidsoftware.core.{LowerCaseInitialRegex, MissingFieldException, SmartBuffer, XmlException}
import com.phasmidsoftware.flog.Flog
import com.phasmidsoftware.xml.Extractors.extractOptional
import com.phasmidsoftware.xml.NamedFunction.name
import org.slf4j.{Logger, LoggerFactory}
import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
import scala.xml.{Node, NodeSeq, PCData}

/**
 * Trait to define the behavior of an extractor (parser) which can will take an XML Node
 * and return Try[T] where T is the underlying type of the Extractor.
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
     * Method to map this Extractor[T] into an Extractor[U].
     *
     * @param f a T => U.
     * @tparam U the underlying type of the result.
     * @return an Extractor[U].
     */
    def map[U](f: T => U): Extractor[U] = (node: Node) => self.extract(node) map f

    /**
     * Method to flatMap this Extractor[T] into an Extractor[U].
     *
     * @param f a T => Try[U].
     * @tparam U the underlying type of the result.
     * @return an Extractor[U].
     */
    def flatMap[U](f: T => Try[U]): Extractor[U] = (node: Node) => self.extract(node) flatMap f

    /**
     * Method to create an Extractor[T] such that, if this Extractor[T] fails, then we invoke the (implicit) Extractor[P] instead.
     *
     * TESTME
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

    // NOTE: this is needed if you enable logging (by uncommenting) in extract and extractMulti methods (below).
//    import flog._

    /**
     * Method to create an Extractor[T] from a Node => Try[T] function.
     * Note that this isn't strictly necessary because of the SAM conversion mechanism which turns a Node => Try[T] function into an Extractor[T].
     *
     * @param extractorFunc a Node => Try[T] function.
     * @tparam T the underlying type of the resulting Extractor.
     * @return an Extractor[T].
     */
    def apply[T](extractorFunc: Node => Try[T]): Extractor[T] = (node: Node) => extractorFunc(node)

    /**
     * Method to create an Extractor[T] such that the result of the extraction is always a constant, regardless of what's in the node provided.
     *
     * @param ty a Try[T].
     * @tparam T the underlying type of the result.
     * @return an Extractor[T] which always produces ty when extract is invoked on it.
     */
    def apply[T](ty: => Try[T]): Extractor[T] = Extractor(_ => ty) ^^ s"Extractor.apply($ty)"

    /**
     * Method to create a lazy Extractor[T] from an explicit Extractor[T] which is call-by-name.
     * The purpose of this method is to break the infinite recursion caused when implicit values are defined recursively.
     * See the Play JSON library method in JsPath called lazyRead.
     *
     * TESTME
     *
     * @param te an Extractor[T].
     * @tparam T the underlying type of the input and output Extractors.
     * @return an Extractor[T].
     */
    def createLazy[T](te: => Extractor[T]): Extractor[T] = (node: Node) => te.extract(node)

    /**
     * Method to extract a Try[T] from the implicitly defined extractor operating on the given node.
     *
     * @param node the node on which the extractor will work.
     * @tparam T the underlying result type.
     *           Required: implicit evidence of an Extractor[T].
     * @return a Try[T].
     */
    def extract[T: Extractor](node: Node): Try[T] =
        implicitly[Extractor[T]].extract(node)

    /**
     * Method to extract a Try[T] from the implicitly defined multi-extractor operating on the given nodes.
     * Usually, T is itself an Iterable type.
     *
     * @param nodeSeq the nodes on which the extractor will work.
     * @tparam T the underlying result type.
     *           Required: implicit evidence of a MultiExtractor[T].
     * @return a Try[T].
     */
    def extractMulti[T: MultiExtractor](nodeSeq: NodeSeq): Try[T] =
//        s"extractMulti: ${name[MultiExtractor[T]]} from ${renderNodes(nodeSeq)}" !!
        implicitly[MultiExtractor[T]].extract(nodeSeq)

    /**
     * Method to extract all possible Try[T] from the implicitly defined multi-extractor operating on the given nodes.
     * Usually, T is itself an Iterable type.
     *
     * The difference between this method and extractMulti is that a Node is passed in, rather than a NodeSeq.
     *
     * @param node the node on which the extractor will work--it will extract from all the node's children.
     * @tparam T the underlying result type.
     *           Required: implicit evidence of a MultiExtractor[T].
     * @return a Try[T].
     */
    def extractAll[T: MultiExtractor](node: Node): Try[T] = extractMulti(node / "_")

    /**
     * Method to extract a singleton from a NodeSeq.
     *
     * @param nodeSeq a sequence of Nodes.
     * @tparam P the underlying type of the result.
     *           Required: implicit evidence of an Extractor[P].
     * @return a Try[P].
     */
    def extractSingleton[P: Extractor](nodeSeq: NodeSeq): Try[P] =
        extractSequence[P](nodeSeq) match {
            case Success(Nil) =>
                Failure(XmlException(s"extractSingleton: empty"))
            case Success(p :: Nil) => Success(p)
            // TESTME
            case Success(ps) => Failure(XmlException(s"extractSingleton: ambiguous values: $ps"))
            case Failure(x) => Failure(x)
        }

    /**
     * Method which tries to extract a sequence of objects from a NodeSeq.
     * The difference between this method and extractMulti is in the declaration of the parametric type and its
     * implicit evidence.
     * The difference between this method and extractSingleton is in the result type.
     *
     * @param nodeSeq a NodeSeq.
     * @tparam P the (Extractor) type to which each individual Node should be converted.
     *           Required: implicit evidence of type Extractor[P].
     * @return a Try of Seq[P].
     */
    def extractSequence[P: Extractor](nodeSeq: NodeSeq): Try[Seq[P]] =
        sequence(for (node <- nodeSeq) yield Extractor.extract[P](node))

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
     * @tparam P the type to which Node should be converted.
     *           Required: implicit evidence of type Extractor[P].
     * @return a Try[P].
     */
    def fieldExtractor[P: Extractor](field: String): Extractor[P] = Extractor(node => doExtractField[P](field, node) match {
        case _ -> Success(p) => Success(p)
        case m -> Failure(x) => x match {
            case _: NoSuchFieldException => Success(None.asInstanceOf[P])
            case _ =>
                val message = s"fieldExtractor(field=$field) from node (${renderNode(node)}) using (${implicitly[Extractor[P]].name}): (field type = $m)"
                Failure(MissingFieldException(message, m, x))
        }
    }
    )

    /**
     * Method to extract child elements from a node.
     *
     * CONSIDER rewriting this method: it has ended up being a huge a hack and needs work!!
     * NOTE: There are four different ways to get a successful result:
     * (1) node / member yields a non-empty result which is passed into extractMulti;
     * (2) TagProperties.mustMatch(member) in which case the result will be empty;
     * (3) extractAll(node) yields a successful non-empty result, which is returned;
     * (4) extractAll(node) yields a successful empty result, in which case ChildNames.translate(member) is used to match children.
     *
     * @param member the name of the element(s) to extract, according to the construct function (typically, this means the name of the member in a case class).
     * @param node   the node from which we want to extract.
     * @tparam P the underlying type of the result.
     *           Required: implicit evidence of type MultiExtractor[P].
     * @return a Try[P].
     */
    def extractChildren[P: MultiExtractor](member: String)(node: Node): Try[P] = {
        val explicitChildren: NodeSeq = node / member
        if (explicitChildren.nonEmpty || TagProperties.mustMatch(member))
            extractMulti(explicitChildren)
        else
            extractAll(node) match {
                case Success(Nil) =>
                    // CONSIDER use Flog logging
                    val ts = ChildNames.translate(member)
                    logger.debug(s"extractChildren(${name[MultiExtractor[P]]})($member)(${renderNode(node)}): get $ts")
                    if (ts.isEmpty) logger.warn(s"extractChildren: logic error: no suitable tags found for children of member $member in ${renderNode(node)}")
                    val nodeSeq: Seq[Node] = for (t <- ts; w <- node / t) yield w
                    if (nodeSeq.nonEmpty) {
                        logger.info(s"extractChildren extracting ${nodeSeq.size} nodes for ($member)")
                        extractMulti(nodeSeq)
                    }
                    else {
                        logger.warn(s"extractChildren: no children matched any of $ts in ${renderNode(node)}")
                        Try(Nil.asInstanceOf[P])
                    }
                case Success(x) =>
                    logger.info(s"extractChildren extracted $x using extractAll")
                    Success(x)
                case Failure(x) =>
                    Failure(x)
            }
    }

    /**
     * Method to extract a Seq or Try[P] from a NodeSeq, by filtering on the given label.
     *
     * @param nodeSeq the nodes whence to extract.
     * @param label   the label to match.
     * @tparam P the underlying type of the result.
     *           Required: implicit evidence of type Extractor[P].
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

    /**
     * Method to determine, from the name of a property, whether it's an attribute or an element.
     *
     * @param name the member (property) name.
     * @return Some(true) if it's an optional attribute; Some(false) if it's a required attribute; else None.
     */
    def inferAttributeType(name: String): Option[Boolean] = name match {
        case optionalAttribute(_) => Some(true)
        case attribute(_) => Some(false)
        case _ => None
    }

    private def doExtractField[P: Extractor](field: String, node: Node): (String, Try[P]) =
        field match {
            // NOTE special name for the (text) content of a node.
            case "$" => "$" -> extractText[P](node)
            // NOTE attributes must match names where the case class member name starts with "_"
            case attribute("xmlns") => "attribute xmlns" -> Failure(XmlException("it isn't documented by xmlns is a reserved attribute name"))
            case optionalAttribute(x) => s"optional attribute: $x" -> extractAttribute[P](node, x, optional = true)
            case attribute(x) => s"attribute: $x" -> extractAttribute[P](node, x)
            // NOTE child nodes are extracted using extractChildren, not here, but if the plural-sounding name is present in node, then we are OK
            case plural(x) if (node \ field).isEmpty => // NOTE: TESTME: this mechanism is to allow for field names to end in "s" without being plural (such as OuterBoundaryIs).
                s"plural:" -> Failure(XmlException(s"extractField: incorrect usage for plural field: $x. Use extractChildren instead."))
            // NOTE optional members such that the name begins with "maybe"
            case optional(x) => s"optional: $x" -> extractOptional[P](node / x)
            // NOTE this is the default case which is used for a singleton entity (plural entities would be extracted using extractChildren).
            // FIXME why would be be looking for a singleton LinearRing in a node which is an extrude node?
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
     * NOTE that the initial character of the result will have been set to lower case.
     */
    val optional: Regex = new LowerCaseInitialRegex("""maybe(\w+)""")

    private def extractText[P: Extractor](node: Node): Try[P] = Extractor.extract[P](node)

    /**
     * Unit extractor.
     */
    implicit val unitExtractor: Extractor[Unit] = Extractor(Success())

    /**
     * CharSequence extractor.
     */
    implicit object charSequenceExtractor extends Extractor[CharSequence] {
        def extract(node: Node): Try[CharSequence] = node match {
            case x: xml.Text => Success(x.data)
            case CDATA(x) => Success(x)
            case _ => node.child.toSeq match {
                case Seq(x) => Success(x.text)
                case x => Failure(XmlException(s"charSequenceExtractor: cannot decode text node: $node: $x"))
            }
        }
    }

    /**
     * Int extractor.
     */
    implicit val intExtractor: Extractor[Int] = charSequenceExtractor flatMap {
        case w: String => Success(w.toInt)
        case x => Failure(XmlException(s"cannot convert $x to an Int"))
    }

    /**
     * Boolean extractor.
     */
    implicit val booleanExtractor: Extractor[Boolean] = charSequenceExtractor flatMap {
        case "true" | "yes" | "T" | "Y" => Success(true)
        case _: String => Success(false)
        case x => Failure(XmlException(s"cannot convert $x to a Boolean"))
    }

    /**
     * Double extractor.
     */
    implicit val doubleExtractor: Extractor[Double] = charSequenceExtractor flatMap {
        case w: String => Success(w.toDouble)
        case x => Failure(XmlException(s"cannot convert $x to a Double"))
    }

    /**
     * Long extractor.
     */
    implicit val longExtractor: Extractor[Long] = charSequenceExtractor flatMap {
        case w: String => Success(w.toLong)
        case x => Failure(XmlException(s"cannot convert $x to a Long"))
    }

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
 * Companion object to MultiExtractor.
 */
object MultiExtractor {

    /**
     * Method to create an MultiExtractor[T] from a NodeSeq => Try[T] function.
     * Note that this isn't strictly necessary because of the SAM conversion mechanism which turns a Node => Try[T] function into an Extractor[T].
     *
     * TESTME
     *
     * @param f a NodeSeq => Try[T] function.
     * @tparam T the underlying type of the resulting MultiExtractor.
     * @return a MultiExtractor[T].
     */
    def apply[T](f: NodeSeq => Try[T]): MultiExtractor[T] = (nodeSeq: NodeSeq) => f(nodeSeq)

    /**
     * Method to create a lazy MultiExtractor[T] from an explicit MultiExtractor[T] which is call-by-name.
     * The purpose of this method is to break the infinite recursion caused when implicit values are defined
     * recursively.
     * See the Play JSON library method in JsPath called lazyRead.
     *
     * @param tm a MultiExtractor[T].
     * @tparam T the underlying type of the MultiExtractor required.
     * @return a MultiExtractor[T].
     */
    def createLazy[T](tm: => MultiExtractor[T]): MultiExtractor[T] = (nodes: NodeSeq) => tm.extract(nodes)
}

/**
 * MultiExtractorBase class to deal with minimum and maximum numbers of elements.
 * This is not used for situations where different subtypes are grouped together (for example, Folder can contain any number of "Features,"
 * which can be of type Container, Document, Folder, or Placemark.
 *
 * @tparam P element type of the MultiExtractor to be returned.
 *           requires implicit evidence of Extractor[P].
 */
case class MultiExtractorBase[P: Extractor](range: Range) extends MultiExtractor[Seq[P]] {
    def extract(nodeSeq: NodeSeq): Try[Seq[P]] =
        sequenceForgiving(logWarning)(nodeSeq map Extractor.extract[P]) match {
            case x@Success(ps) if range.contains(ps.size) => x
            case Success(ps) => Failure(XmlException(s"MultiExtractorBase.extract: the number (${ps.size}) of elements extracted is not in the required range: $range"))
            case x@Failure(_) => x
        }

    private def logWarning(x: Throwable): Unit = x match {
        case MissingFieldException(_, "singleton", _) if range.start <= 0 => // NOTE: OK -- no need to log anything.
        case XmlException(message, x) => logger.warn("MultiExtractorBase: $message" + x.getLocalizedMessage)
    }
}

object MultiExtractorBase {
    /**
     * All integers greater than zero (the "counting numbers" or Z+).
     */
    val Positive: Range.Inclusive = 1 to Int.MaxValue

    /**
     * All non-negative integers (Z0+).
     */
    val NonNegative: Range.Inclusive = 0 to Int.MaxValue

    /**
     * Exactly One.
     */
    val ExactlyOne: Range.Inclusive = 1 to 1

    /**
     * Either Zero or One.
     */
    val AtMostOne: Range.Inclusive = 0 to 1
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
}

/**
 * Class which extends an Extractor of Seq[T].
 *
 * NOTE: used by subclassExtractor1 method (itself unused).
 *
 * @param labels a set of labels (tags)
 * @param tsm    an (implicit) MultiExtractor of Seq[T].
 * @tparam T the type to be constructed.
 */
class SubclassExtractor[T](val labels: Seq[String])(implicit tsm: MultiExtractor[Seq[T]]) extends Extractor[Seq[T]] {
    /**
     * TESTME in particular regarding the use of sequenceForgiving
     *
     * @param node a Node.
     * @return a Try[T].
     */
    def extract(node: Node): Try[Seq[T]] = {
        val f: Throwable => Unit = x => Extractor.logger.warn(x.getLocalizedMessage)
        sequenceForgiving(f)(for (label <- labels) yield tsm.extract(node \ label)) map (_.flatten)
    }
}

/**
 * Object to manage translation of a name to a Seq of names.
 *
 * CONSIDER removing this altogether because there is another mechanism (Feature.multiExtractor) which does something similar.
 *
 */
object ChildNames {
    // CONSIDER make this immutable.
    val map: mutable.HashMap[String, Seq[String]] = new mutable.HashMap()

    def addTranslation(key: String, value: Seq[String]): Unit = map += key -> value

    def translate(member: String): Seq[String] =
        map.getOrElse(member,
            member match {
                case Extractor.plural(x) => Seq(x)
                case _ => map.getOrElse(member, Seq(member))
            })
}

object TagProperties {
    def addMustMatch(tag: String): Unit = mustMatchList += tag

    def mustMatch(tag: String): Boolean = mustMatchList.contains(tag)

    private val mustMatchList = mutable.Set[String]()
}

/**
 * Case class to represent a CDATA node.
 *
 * @param content the payload of the CDATA node (will contain <, >, & characters).
 * @param pre     the prefix (probably a newline).
 * @param post    the postfix (probably a newline).
 */
case class CDATA(content: String, pre: String, post: String) extends CharSequence {
    def toXML: Try[String] = Success(s"""$pre<![CDATA[$content]]>$post""")

    def length(): Int = content.length()

    def charAt(index: Int): Char = content.charAt(index)

    def subSequence(start: Int, end: Int): CharSequence = content.subSequence(start, end)

    override def toString: String = s"$pre$content$post"
}

/**
 * Companion object to CDATA.
 */
object CDATA {

    def apply(x: String, pre: String, post: String): CDATA = new CDATA(x, trimSpace(pre), trimSpace(post))

    def apply(x: String): CDATA = apply(x, "", "")

    def wrapped(x: String): CDATA = apply(x, "\n", "\n")

    private def trimSpace(w: String): String = {
        val sb = new StringBuilder(w)
        SmartBuffer.trimStringBuilder(sb)
        sb.toString()
    }

    def unapply(node: Node): Option[CDATA] = node.child match {
        case Seq(pre, PCData(x), post) => Some(CDATA(x, pre.text, post.text))
        case Seq(PCData(x)) => Some(CDATA(x))
        case _ => None
    }
}