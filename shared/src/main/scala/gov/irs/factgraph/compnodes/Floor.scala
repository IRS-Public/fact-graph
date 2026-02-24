package gov.irs.factgraph.compnodes

import gov.irs.factgraph.{ Expression, FactDictionary, Factual }
import gov.irs.factgraph.definitions.fact.CompNodeConfigTrait
import gov.irs.factgraph.operators.UnaryOperator
import gov.irs.factgraph.types.{ Dollar, Rational }
import scala.math.BigDecimal.RoundingMode

object Floor extends CompNodeFactory:
  override val Key: String = "Floor"

  def apply(node: CompNode): CompNode =
    node match
      case node: RationalNode => IntNode(Expression.Unary(node.expr, RationalFloorOperator()))
      case node: IntNode      => IntNode(Expression.Unary(node.expr, IntegerFloorOperator()))
      case node: DollarNode   => DollarNode(Expression.Unary(node.expr, DollarFloorOperator()))
      case _ => throw new UnsupportedOperationException(s"cannot execute Floor on a ${node.getClass.getName}")

  override def fromDerivedConfig(e: CompNodeConfigTrait)(using Factual)(using FactDictionary): CompNode =
    CompNode.getConfigChildNode(e) match
      case x: RationalNode => this(x)
      case x: IntNode      => this(x)
      case x: DollarNode   => this(x)
      case _               => throw new UnsupportedOperationException(s"invalid child type: ${e.typeName}")

final private class RationalFloorOperator extends UnaryOperator[Int, Rational]:
  override protected def operation(x: Rational): Int = (x.numerator.toFloat / x.denominator).floor.toInt

final private class IntegerFloorOperator extends UnaryOperator[Int, Int]:
  override protected def operation(x: Int): Int = x

final private class DollarFloorOperator extends UnaryOperator[Dollar, Dollar]:
  override protected def operation(x: Dollar): Dollar = {
    val rounded = x.asInstanceOf[BigDecimal].setScale(0, RoundingMode.FLOOR)
    Dollar(rounded)
  }
