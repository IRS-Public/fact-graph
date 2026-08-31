package gov.irs.factgraph.compnodes

import gov.irs.factgraph.*
import gov.irs.factgraph.definitions.fact.WritableConfigTrait
import gov.irs.factgraph.types.{ Collection, CollectionItem }
import scala.scalajs.js.annotation.JSExport

final case class CollectionNode(
    expr: Expression[Collection],
    alias: Option[Path],
) extends CompNode:
  type Value = Collection
  override def ValueClass = classOf[Collection]

  /** The collection this one stands for, when it is a `<Filter>` or a `<Dependency>` on another. */
  @JSExport
  def getAlias() = alias

  override private[compnodes] def switch(
      cases: List[(BooleanNode, CompNode)],
  ): CompNode =
    val aliasesMatch = cases.forall(
      _._2.asInstanceOf[CollectionNode].alias == alias,
    )

    if (!aliasesMatch)
      throw new UnsupportedOperationException(
        "collections in a <Switch> must reference the same collection",
      )

    CollectionNode(
      Expression.Switch(
        cases
          .asInstanceOf[List[(BooleanNode, CollectionNode)]]
          .map((b, a) => (b.expr, a.expr)),
      ),
      alias,
    )

  override private[compnodes] def dependency(path: Path): CompNode =
    val newAlias = alias match
      case Some(_) => alias
      case None    => Some(path)

    CollectionNode(Expression.Dependency(path), newAlias)

  override private[factgraph] def fromExpression(
      expr: Expression[Collection],
  ): CompNode =
    throw new UnsupportedOperationException(
      "cannot create a Collection from an expression",
    )

  override def extract(key: PathItem): Option[CompNode] =
    key match
      case PathItem.Member(id) =>
        Some(
          CollectionItemNode(
            Expression.Constant(Some(CollectionItem(id))),
            None,
          ),
        )
      case PathItem.Unknown =>
        Some(CollectionItemNode(Expression.Constant(None), None))
      case _ => None

object CollectionNode extends WritableNodeFactory:
  override val Key: String = "Collection"

  override def fromWritableConfig(
      e: WritableConfigTrait,
  )(using Factual)(using FactDictionary): CompNode =
    new CollectionNode(
      Expression.Writable(classOf[Collection]),
      None,
    )
