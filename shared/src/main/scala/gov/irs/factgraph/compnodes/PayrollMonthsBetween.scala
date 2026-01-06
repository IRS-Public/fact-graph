package gov.irs.factgraph.compnodes

import gov.irs.factgraph.{ Expression, FactDictionary, Factual }
import gov.irs.factgraph.definitions.fact.CompNodeConfigTrait
import gov.irs.factgraph.operators.BinaryOperator
import gov.irs.factgraph.types.*
import java.time.temporal.ChronoUnit

object PayrollMonthsBetween extends CompNodeFactory:
  override val Key: String = "PayrollMonthsBetween"

  private val operator = PayrollMonthsBetweenBinaryOperator()

  def apply(startDate: DayNode, endDate: DayNode): IntNode =
    IntNode(
      Expression.Binary(
        startDate.expr,
        endDate.expr,
        operator,
      ),
    )

  override def fromDerivedConfig(e: CompNodeConfigTrait)(using
      Factual,
  )(using
      FactDictionary,
  ): CompNode =
    val startDate = CompNode.getConfigChildNode(e, "StartDate").asInstanceOf[DayNode]
    val endDate = CompNode.getConfigChildNode(e, "EndDate").asInstanceOf[DayNode]
    this(startDate, endDate)

  final private class PayrollMonthsBetweenBinaryOperator extends BinaryOperator[Int, Day, Day]:
    override protected def operation(startDate: Day, endDate: Day): Int =
      ChronoUnit.MONTHS
        .between(startDate.date, endDate.date)
        .toInt + 1 // We add one because we don't want to undercount
