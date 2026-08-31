package gov.irs.factgraph.types
import gov.irs.factgraph.validation.{ ValidationFailure, ValidationFailureReason }
import java.lang.Enum
import scala.beans.BeanProperty
import scala.compiletime.ops.boolean
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportAll
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.matching.Regex
import upickle.default.ReadWriter

@JSExportAll
enum UserFriendlyIpPinFailureReason:
  case InvalidIpPin
  case InvalidAllZerosIpPin

@JSExportAll
enum IpPinFailureReason extends Enum[IpPinFailureReason], ValidationFailureReason:
  type UserFriendlyReason = UserFriendlyIpPinFailureReason
  case InvalidIpPin
  case InvalidAllZerosIpPin

  def toUserFriendlyReason() = this match
    case InvalidIpPin         => UserFriendlyIpPinFailureReason.InvalidIpPin
    case InvalidAllZerosIpPin =>
      UserFriendlyIpPinFailureReason.InvalidAllZerosIpPin

@JSExportAll
final case class IpPinValidationFailure(
    private val message: String = "",
    private val cause: Throwable = None.orNull,
    val validationMessage: IpPinFailureReason,
) extends IllegalArgumentException(message, cause),
      ValidationFailure[IpPinFailureReason];

@JSExportTopLevel("IpPin")
@JSExportAll
final case class IpPin(
    @BeanProperty pin: String,
) derives ReadWriter:

  precondition(
    pin.length == 6,
    "PIN of wrong length",
    IpPinFailureReason.InvalidIpPin,
  )
  precondition(
    List(pin).forall(arg => arg.forall(_.isDigit)),
    "Numbers only",
    IpPinFailureReason.InvalidIpPin,
  )
  precondition(
    !(pin equals "000000"),
    "No PIN with all zeroes",
    IpPinFailureReason.InvalidAllZerosIpPin,
  )

  override def toString(): String = s"${pin}"

  private[this] def precondition(
      test: Boolean,
      message: String,
      validationMessage: IpPinFailureReason,
  ): Unit =
    if (!test)
      throw IpPinValidationFailure(message, None.orNull, validationMessage)

object IpPin:
  private val Pattern: Regex = """^([0-9]{6})$""".r

  def apply(s: String): IpPin = this.parseString(s) match
    case Some(pin) => pin
    case None      =>
      throw IpPinValidationFailure(
        "PINs must be 6 digits long",
        None.orNull,
        IpPinFailureReason.InvalidIpPin,
      )

  @JSExport
  def parseString(s: String): Option[IpPin] =
    val cleanInput = s.replaceAll("[^0-9]", "")
    cleanInput match
      // single-argument `apply` above is this method's
      // caller, so `this(pin)` calls back into `parseString` and never terminates. That made a
      // graph holding a IpPin impossible to deserialize
      case Pattern(pin) => Some(new IpPin(pin))
      case _            => None
