package gov.irs.factgraph

import fs2.{ Fallible, Stream }
import fs2.data.xml.*
import fs2.data.xml.dom.*
import fs2.data.xml.scalaXml.*
import gov.irs.factgraph.compnodes.CollectionItemNode
import gov.irs.factgraph.compnodes.CollectionNode
import gov.irs.factgraph.compnodes.EnumNode
import gov.irs.factgraph.compnodes.MultiEnumNode
import gov.irs.factgraph.compnodes.RootNode
import gov.irs.factgraph.definitions.{ FactDictionaryConfigElement, FactDictionaryConfigTrait }
import gov.irs.factgraph.definitions.fact.FactConfigElement
import gov.irs.factgraph.definitions.meta.MetaConfigTrait
import scala.collection.mutable
import scala.scalajs.js.annotation.JSExport
import scala.util.matching.Regex
import scala.xml.NodeSeq

class FactDictionary:
  private val UUID_REGEX: Regex = "(?i)#[0-9A-F]{8}-[0-9A-F]{4}-[1-5][0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}".r

  private val definitions: mutable.Map[Path, FactDefinition] = mutable.Map()
  private val definitionsAsNodes: mutable.Map[Path, NodeSeq] = mutable.Map()
  private var frozen: Boolean = false
  private var meta: MetaConfigTrait = Meta.empty()

  def getPaths(): Iterable[Path] =
    definitions.keys

  def freeze(): Unit =
    for {
      (_, definition) <- definitions
    } definition.meta
    if (meta == Meta.empty())
      throw new UnsupportedOperationException(
        "Must provide meta information to FactDictionary",
      )
    frozen = true

  @JSExport
  def getDefinition(path: String): FactDefinition | Null =
    apply(path: String)

  def apply(path: Path): Option[FactDefinition] = definitions.get(path)

  def apply(path: String): FactDefinition | Null =
    resolveDirectPath(path)
      .orElse(resolveWildcardPath(path))
      .orElse(resolveCollectionAliasPath(path))
      .orNull

  private def resolveDirectPath(path: String): Option[FactDefinition] =
    definitions.get(Path(path))

  // Try to match a definition after removing the UUIDs
  private def resolveWildcardPath(path: String): Option[FactDefinition] =
    definitions.get(Path(UUID_REGEX.replaceAllIn(path, "*")))

  // A path whose first segment is a fact that stands for another collection, or for one item of
  // one. Both kinds are matched on rather than cast: the first segment of an undefined path can be a
  // fact of any type at all, and an `asInstanceOf` here turned that into a ClassCastException from
  // inside a lookup whose contract is to return null.
  private def resolveCollectionAliasPath(path: String): Option[FactDefinition] =
    path.stripPrefix("/").split("/").toList match
      case firstSegment :: subPath =>
        definitions.get(Path(s"/$firstSegment")).flatMap { aliasDef =>
          aliasDef.value match
            // `/primaryFiler` is one item of `/filers`, so `/primaryFiler/x` is `/filers/*/x`. The
            // whole remainder first, then its head alone, which is what resolves a path into a
            // typed value's own members: `/primaryFiler/tin/isSSN` answers with the `tin` fact.
            case node: CollectionItemNode =>
              for
                collection <- node.getAlias()
                resolved <- subPath.headOption.flatMap(head =>
                  definitions
                    .get(Path(s"$collection/*/${subPath.mkString("/")}"))
                    .orElse(definitions.get(Path(s"$collection/*/$head"))),
                )
              yield resolved
            // `/alaskaPfd1099s` is a `<Filter>` over `/form1099Miscs`, so `/alaskaPfd1099s/*/x` is
            // `/form1099Miscs/*/x`. The alias replaces the first segment and the rest is carried
            // over, wildcard included.
            case node: CollectionNode =>
              for
                collection <- node.getAlias()
                resolved <- definitions.get(Path((collection.toString :: subPath).mkString("/")))
              yield resolved
            case _ => None
        }
      case _ => None

  def getDefinitionsAsNodes(): mutable.Map[Path, NodeSeq] = definitionsAsNodes

  @JSExport
  def getMeta(): MetaConfigTrait = meta

  @JSExport("getOptionsPathForEnum")
  def getOptionsPathForEnum(enumPath: String): Option[String] =
    val factDef = this(enumPath)
    factDef.value match
      case value: EnumNode      => Some(value.enumOptionsPath.toString)
      case value: MultiEnumNode => Some(value.enumOptionsPath.toString)
      case _                    => None

  protected[factgraph] def addDefinition(definition: FactDefinition): Unit =
    if (frozen)
      throw new UnsupportedOperationException(
        "cannot add definitions to a frozen FactDictionary",
      )

    definitions.addOne(definition.asTuple)

  protected[factgraph] def addDefinitionAsNodes(path: Path, rawXml: NodeSeq): Unit =
    if (frozen)
      throw new UnsupportedOperationException(
        "cannot add definitions to a frozen FactDictionary",
      )
    definitionsAsNodes.addOne(path, rawXml)

  protected[factgraph] def addMeta(metaConfigTrait: MetaConfigTrait): Unit =
    if (frozen)
      throw new UnsupportedOperationException(
        "Meta configuration must be added before freezing the dictionary",
      )
    meta = metaConfigTrait

trait DefaultFactDictConfig {
  val meta = Meta("1.0")

  def apply(): FactDictionary =
    val dictionary = new FactDictionary()
    FactDefinition(RootNode(), Path.Root, Seq.empty, NodeSeq.Empty, dictionary)
    dictionary

  @JSExport
  def fromConfig(e: FactDictionaryConfigTrait): FactDictionary =
    val dictionary = this()
    Meta.fromConfig(e.meta, dictionary)
    e.facts.map(FactDefinition.fromConfig(_)(using dictionary))
    dictionary.freeze()
    dictionary

  @JSExport
  def importFromXml(xmlString: String): FactDictionary = {
    // We're using a different parser because XML.loadString requires the JVM
    val evts = Stream
      .emits(xmlString)
      .through(events[Fallible, Char]())
      .through(documents)

    val moduleXml = evts.compile.toList match {
      case Right(x) => x
      case Left(e)  => throw e
    }

    fromXml(moduleXml.head)
  }

  def fromXml(factDictionaryModule: scala.xml.NodeSeq): FactDictionary = {
    val facts = factDictionaryModule \\ "Fact"
    val factConfigs = facts.map(FactConfigElement.fromXml)
    val config = FactDictionaryConfigElement(meta, factConfigs)
    fromConfig(config)
  }
}

object FactDictionary extends DefaultFactDictConfig

object FactDictionaryForTests extends DefaultFactDictConfig {
  override val meta: Meta = Meta("1.0", true)
}
