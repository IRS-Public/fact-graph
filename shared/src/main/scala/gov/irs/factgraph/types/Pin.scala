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
enum UserFriendlyPinFailureReason:
  case InvalidPin
  case InvalidAllZerosPin

@JSExportAll
enum PinFailureReason extends Enum[PinFailureReason], ValidationFailureReason:
  type UserFriendlyReason = UserFriendlyPinFailureReason
  case InvalidPin
  case InvalidAllZerosPin

  def toUserFriendlyReason() = this match
    case InvalidPin         => UserFriendlyPinFailureReason.InvalidPin
    case InvalidAllZerosPin => UserFriendlyPinFailureReason.InvalidAllZerosPin

@JSExportAll
final case class PinValidationFailure(
    private val message: String = "",
    private val cause: Throwable = None.orNull,
    val validationMessage: PinFailureReason,
) extends IllegalArgumentException(message, cause),
      ValidationFailure[PinFailureReason];

@JSExportTopLevel("Pin")
@JSExportAll
final case class Pin(
    @BeanProperty pin: String,
) derives ReadWriter:

  precondition(
    pin.length == 5,
    "PIN of wrong length",
    PinFailureReason.InvalidPin,
  )
  precondition(
    List(pin).forall(arg => arg.forall(_.isDigit)),
    "Numbers only",
    PinFailureReason.InvalidPin,
  )
  precondition(
    !(pin equals "00000"),
    "No PIN with all zeroes",
    PinFailureReason.InvalidAllZerosPin,
  )

  override def toString(): String = s"${pin}"

  private[this] def precondition(
      test: Boolean,
      message: String,
      validationMessage: PinFailureReason,
  ): Unit =
    if (!test)
      throw PinValidationFailure(message, None.orNull, validationMessage)

object Pin:
  private val Pattern: Regex = """^(?!0{5})([0-9]{5})$""".r

  def apply(s: String): Pin = this.parseString(s) match
    case Some(pin) => pin
    case None      =>
      throw PinValidationFailure(
        "PINs must be 5 digits long",
        None.orNull,
        PinFailureReason.InvalidPin,
      )

  @JSExport
  def parseString(s: String): Option[Pin] =
    val cleanInput = s.replaceAll("[^0-9]", "")
    cleanInput match
      // single-argument `apply` above is this method's
      // caller, so `this(pin)` calls back into `parseString` and never terminates. That made a
      // graph holding a Pin impossible to deserialize
      case Pattern(pin) => Some(new Pin(pin))
      case _            => None
