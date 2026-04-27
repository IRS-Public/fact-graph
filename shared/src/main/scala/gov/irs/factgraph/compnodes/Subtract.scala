package gov.irs.factgraph.compnodes

import gov.irs.factgraph.{ Expression, FactDictionary, Factual }
import gov.irs.factgraph.definitions.fact.CompNodeConfigTrait
import gov.irs.factgraph.operators.{ BinaryOperator, ReduceOperator }
import gov.irs.factgraph.types.{ *, given }
import gov.irs.factgraph.util.Seq.itemsHaveSameRuntimeClass
import scala.annotation.unused

object Subtract extends CompNodeFactory:
  override val Key: String = "Subtract"

  def apply(nodes: Seq[CompNode]): CompNode =
    if (itemsHaveSameRuntimeClass(nodes))
      reduceSubtract(nodes)
    else
      nodes.reduceLeft(binarySubtract)

  override def fromDerivedConfig(e: CompNodeConfigTrait)(using
      Factual,
  )(using
      FactDictionary,
  ): CompNode =
    val minuend = CompNode.getConfigChildNode(e, "Minuend")
    val subtrahends = CompNode.getConfigChildNodes(e, "Subtrahends")
    this(minuend +: subtrahends)

  private def reduceSubtract(nodes: Seq[CompNode]): CompNode =
    nodes.head match
      case node: IntNode =>
        IntNode(
          Expression.Reduce(
            nodes.asInstanceOf[List[IntNode]].map(_.expr),
            summon[SubtractReduceOperator[Int]],
          ),
        )
      case node: DollarNode =>
        DollarNode(
          Expression.Reduce(
            nodes.asInstanceOf[List[DollarNode]].map(_.expr),
            summon[SubtractReduceOperator[Dollar]],
          ),
        )
      case node: RationalNode =>
        RationalNode(
          Expression.Reduce(
            nodes.asInstanceOf[List[RationalNode]].map(_.expr),
            summon[SubtractReduceOperator[Rational]],
          ),
        )
      case node: DayNode => nodes.reduceLeft(binarySubtract)
      case _             =>
        throw new UnsupportedOperationException(
          s"cannot Subtract a ${nodes.head.getClass.getName}",
        )

  private def binarySubtract(lhs: CompNode, rhs: CompNode): CompNode =
    (lhs, rhs).match
      case (lhs: IntNode, rhs: IntNode) =>
        IntNode(
          Expression.Binary(
            lhs.expr,
            rhs.expr,
            summon[SubtractBinaryOperator[Int, Int, Int]],
          ),
        )
      case (lhs: DollarNode, rhs: DollarNode) =>
        DollarNode(
          Expression.Binary(
            lhs.expr,
            rhs.expr,
            summon[SubtractBinaryOperator[Dollar, Dollar, Dollar]],
          ),
        )
      case (lhs: RationalNode, rhs: RationalNode) =>
        RationalNode(
          Expression.Binary(
            lhs.expr,
            rhs.expr,
            summon[SubtractBinaryOperator[Rational, Rational, Rational]],
          ),
        )
      case (lhs: IntNode, rhs: DollarNode) =>
        DollarNode(
          Expression.Binary(
            lhs.expr,
            rhs.expr,
            summon[SubtractBinaryOperator[Dollar, Int, Dollar]],
          ),
        )
      case (lhs: DollarNode, rhs: IntNode) =>
        DollarNode(
          Expression.Binary(
            lhs.expr,
            rhs.expr,
            summon[SubtractBinaryOperator[Dollar, Dollar, Int]],
          ),
        )
      case (lhs: IntNode, rhs: RationalNode) =>
        RationalNode(
          Expression.Binary(
            lhs.expr,
            rhs.expr,
            summon[SubtractBinaryOperator[Rational, Int, Rational]],
          ),
        )
      case (lhs: RationalNode, rhs: IntNode) =>
        RationalNode(
          Expression.Binary(
            lhs.expr,
            rhs.expr,
            summon[SubtractBinaryOperator[Rational, Rational, Int]],
          ),
        )
      case (lhs: RationalNode, rhs: DollarNode) =>
        DollarNode(
          Expression.Binary(
            lhs.expr,
            rhs.expr,
            summon[SubtractBinaryOperator[Dollar, Rational, Dollar]],
          ),
        )
      case (lhs: DollarNode, rhs: RationalNode) =>
        DollarNode(
          Expression.Binary(
            lhs.expr,
            rhs.expr,
            summon[SubtractBinaryOperator[Dollar, Dollar, Rational]],
          ),
        )
      case (lhs: DayNode, rhs: DaysNode) =>
        DayNode(
          Expression.Binary(
            lhs.expr,
            rhs.expr,
            summon[SubtractBinaryOperator[Day, Day, Days]],
          ),
        )
      case (lhs: DayNode, rhs: DayNode) =>
        IntNode(
          Expression.Binary(
            lhs.expr,
            rhs.expr,
            summon[SubtractBinaryOperator[Int, Day, Day]],
          ),
        )
      case _ =>
        throw new UnsupportedOperationException(
          s"cannot Subtract a ${lhs.getClass.getName} and a ${rhs.getClass.getName}",
        )

final private class SubtractReduceOperator[A: Numeric] extends ReduceOperator[A]:
  override protected def reduce(x: A, y: A): A = Numeric[A].minus(x, y)

@unused
private object SubtractReduceOperator:
  implicit val intOperator: SubtractReduceOperator[Int] =
    SubtractReduceOperator[Int]
  implicit val dollarOperator: SubtractReduceOperator[Dollar] =
    SubtractReduceOperator[Dollar]
  implicit val rationalOperator: SubtractReduceOperator[Rational] =
    SubtractReduceOperator[Rational]
  // implicit def numericOperator[A: Numeric]: SubtractReduceOperator[A] =
  //   SubtractReduceOperator[A]

private trait SubtractBinaryOperator[A, L, R] extends BinaryOperator[A, L, R]

@unused
private object SubtractBinaryOperator:
  implicit val intIntOperator: SubtractBinaryOperator[Int, Int, Int] =
    (lhs: Int, rhs: Int) => lhs - rhs
  implicit val dollarDollarOperator: SubtractBinaryOperator[Dollar, Dollar, Dollar] =
    (lhs: Dollar, rhs: Dollar) => lhs - rhs
  implicit val dayDaysOperator: SubtractBinaryOperator[Day, Day, Days] =
    (lhs: Day, rhs: Days) => lhs - rhs
  implicit val dayDayOperator: SubtractBinaryOperator[Int, Day, Day] =
    (lhs: Day, rhs: Day) => lhs - rhs
  implicit val rationalRationalOperator: SubtractBinaryOperator[Rational, Rational, Rational] =
    (lhs: Rational, rhs: Rational) => lhs - rhs
  implicit val dollarIntOperator: SubtractBinaryOperator[Dollar, Dollar, Int] =
    (lhs: Dollar, rhs: Int) => Numeric[Dollar].minus(lhs, rhs)
  implicit val intDollarOperator: SubtractBinaryOperator[Dollar, Int, Dollar] =
    (lhs: Int, rhs: Dollar) => Numeric[Dollar].minus(lhs, rhs)
  implicit val rationalIntOperator: SubtractBinaryOperator[Rational, Rational, Int] =
    (lhs: Rational, rhs: Int) => Numeric[Rational].minus(lhs, rhs)
  implicit val intRationalOperator: SubtractBinaryOperator[Rational, Int, Rational] =
    (lhs: Int, rhs: Rational) => Numeric[Rational].minus(lhs, rhs)
  implicit val dollarRationalOperator: SubtractBinaryOperator[Dollar, Dollar, Rational] =
    (lhs: Dollar, rhs: Rational) => Numeric[Dollar].minus(lhs, rhs)
  implicit val rationalDollarOperator: SubtractBinaryOperator[Dollar, Rational, Dollar] =
    (lhs: Rational, rhs: Dollar) => Numeric[Dollar].minus(lhs, rhs)
