package gov.irs.factgraph.types
import java.time.temporal.ChronoUnit
import java.time.temporal.TemporalAdjusters
import scala.annotation.targetName
import scala.beans.BeanProperty
import scala.scalajs.js.annotation.{ JSExport, JSExportAll, JSExportTopLevel }
import upickle.default._

implicit val localDateReadWrite: ReadWriter[java.time.LocalDate] =
  readwriter[String]
    .bimap[java.time.LocalDate](_.toString, java.time.LocalDate.parse(_))

@JSExportTopLevel("Day")
final case class Day(@BeanProperty date: java.time.LocalDate) derives ReadWriter:
  import Day.DayIsComparable._

  def <(that: Day): Boolean = lt(this, that)

  def >(that: Day): Boolean = gt(this, that)

  def <=(that: Day): Boolean = lteq(this, that)

  def >=(that: Day): Boolean = gteq(this, that)

  def -(y: Days): Day = this.minusDays(y)

  def -(y: Day): Int = ChronoUnit.DAYS.between(y.date, this.date).toInt

  def +(y: Days): Day = this.plusDays(y)

  def minusDays(y: Days): Day = Day(this.date.minusDays(y.longValue))

  def plusDays(y: Days): Day = Day(this.date.plusDays(y.longValue))

  def addPayrollMonths(months: Int): Day =
    val isLastDayOfMonth = this.date.`with`(TemporalAdjusters.lastDayOfMonth()).isEqual(this.date)
    if (isLastDayOfMonth) {
      Day(this.date.plusMonths(months.toLong).`with`(TemporalAdjusters.lastDayOfMonth()))
    } else {
      Day(this.date.plusMonths(months.toLong))
    }

  @JSExport
  def year: Int = date.getYear()
  @JSExport
  def month: Int = date.getMonth().getValue
  @JSExport
  def day: Int = date.getDayOfMonth()
  @JSExport
  def ordinal: Int = date.getDayOfYear()

  override def toString: String = date.toString

object Day:
  def apply(s: String): Day = this(java.time.LocalDate.parse(s))

  def apply(s: Option[String]): Option[Day] =
    s match
      case Some(dateStr) =>
        try Some(apply(dateStr))
        catch case _: java.time.format.DateTimeParseException => None
      case None => None

  implicit object DayIsComparable extends Ordering[Day]:
    def compare(
        x: gov.irs.factgraph.types.Day,
        y: gov.irs.factgraph.types.Day,
    ): Int = x.date.compareTo(y.date)
