package gov.irs.factgraph.compnodes

import gov.irs.factgraph.{ Expression, FactDictionary, Factual }
import gov.irs.factgraph.definitions.fact.CompNodeConfigTrait
import gov.irs.factgraph.operators.UnaryOperator
import gov.irs.factgraph.types.{ Dollar, Rational }
import scala.math.BigDecimal.RoundingMode

object Ceiling extends CompNodeFactory:
  override val Key: String = "Ceiling"

  def apply(node: CompNode): CompNode =
    node match {
      case node: RationalNode => IntNode(Expression.Unary(node.expr, RationalCeilingOperator()))
      case node: IntNode      => IntNode(Expression.Unary(node.expr, IntegerCeilingOperator()))
      case node: DollarNode   => DollarNode(Expression.Unary(node.expr, DollarCeilingOperator()))
      case _ => throw new UnsupportedOperationException(s"cannot execute Ceiling on a ${node.getClass.getName}")
    }

  override def fromDerivedConfig(e: CompNodeConfigTrait)(using Factual)(using FactDictionary): CompNode =
    CompNode.getConfigChildNode(e) match
      case x: RationalNode => this(x)
      case x: IntNode      => this(x)
      case x: DollarNode   => this(x)
      case _               => throw new UnsupportedOperationException(s"invalid child type: ${e.typeName}")

final private class RationalCeilingOperator extends UnaryOperator[Int, Rational]:
  override protected def operation(x: Rational): Int = (BigDecimal(x.numerator) / BigDecimal(x.denominator))
    .setScale(0, RoundingMode.CEILING)
    .intValue()

final private class IntegerCeilingOperator extends UnaryOperator[Int, Int]:
  override protected def operation(x: Int): Int = x

final private class DollarCeilingOperator extends UnaryOperator[Dollar, Dollar]:
  override protected def operation(x: Dollar): Dollar = {
    val rounded = x.asInstanceOf[BigDecimal].setScale(0, RoundingMode.CEILING)
    Dollar(rounded)
  }
