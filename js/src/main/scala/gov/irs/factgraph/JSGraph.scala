package gov.irs.factgraph
import gov.irs.factgraph.compnodes.*
import gov.irs.factgraph.limits.LimitViolation
import gov.irs.factgraph.monads.{ MaybeVector, Result }
import gov.irs.factgraph.persisters.*
import gov.irs.factgraph.types.{ DayFactory, DollarFactory, EnumFactory, WritableType }
import gov.irs.factgraph.ErrorType.{ LimitError, UnsupportedTypeError, ValidationError }
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.JSConverters.*
import scala.util.Try

enum ErrorType:
  case LimitError
  case ValidationError
  case UnsupportedTypeError

// Consider extending this class so that the errorType can actually use ErrorType
// It's currently a string so that it can be easily checked for in JS
class SetReturnValue(val errorType: String, val errorName: String, val expectedValue: String) extends js.Object

@JSExportTopLevel("Graph")
@JSExportAll
class JSGraph(
    override val dictionary: FactDictionary,
    override val persister: Persister,
) extends Graph(dictionary, persister):

  val INVALID_BOOLEAN_ERROR = "InvalidBoolean"
  val INVALID_INT_ERROR = "InvalidInt"

  def toStringDictionary(): js.Dictionary[String] =
    // This is a debug function to allow for quick inspection
    // of the graph
    this.persister.toStringMap().toJSDictionary

  // Get a fact definition from the fact dictionary
  // In scala this is graph.apply
  def getFact(path: String) =
    root.apply(Path(path)) match
      case MaybeVector.Single(x) =>
        x match
          case Result.Complete(v)    => v
          case Result.Placeholder(v) => v
          case Result.Incomplete     => null

      case MaybeVector.Multiple(vect, c) =>
        throw new UnsupportedOperationException(
          s"getFact returned multiple results for path $path, which is unsupported",
        )

  // In HTML, form value are always strings
  // This method simplifies the interface for facts so that the consumer of the fact graph only
  // has to supply a string: the fact graph will convert it to the appropriate type based on the
  // definition, or throw an exception if that type is incorrect.
  def set(path: String, value: String): SetReturnValue = {
    val definition = this.dictionary.getDefinition(path)

    // I would like to do this in a far cleaner way but until I excise the factory pattern this will have to do.
    // The core problems I'm running into are that the factory pattern is only implemented for some nodes, and even if
    // it was implemented for all of them, the ValidationMessage is implemented with a generic type, not an interface,
    // so you can't actually write code that reads a ValidationMessage (as far as I can tell) without knowing the type
    // at compile time. That's why all the nodes have to unpacked with their specific types known.
    //
    // Still, this way of doing errors is a good interface for the JS, and we can refactor the implementation later.
    val typedValue: WritableType = definition.value match
      case _: BooleanNode =>
        try value.toBoolean
        catch case _: Throwable => return SetReturnValue(ValidationError.toString, INVALID_BOOLEAN_ERROR, null)
      case _: IntNode =>
        try value.toInt
        catch case _: Throwable => return SetReturnValue(ValidationError.toString, INVALID_INT_ERROR, null)
      case a: EnumNode =>
        val maybeEnum = EnumFactory(value, a.enumOptionsPath)
        if (maybeEnum.isLeft) {
          val errorName = maybeEnum.left.validationMessage.toUserFriendlyReason().toString
          return SetReturnValue(ValidationError.toString, errorName, null)
        }
        maybeEnum.right
      case _: DollarNode =>
        val maybeDollar = DollarFactory(value)
        if (maybeDollar.isLeft) {
          val errorName = maybeDollar.left.validationMessage.toUserFriendlyReason().toString
          return SetReturnValue(ValidationError.toString, errorName, null)
        }
        maybeDollar.right
      case _: DayNode =>
        val maybeDay = DayFactory(value)
        if (maybeDay.isLeft) {
          val errorName = maybeDay.left.validationMessage.toUserFriendlyReason().toString
          return SetReturnValue(ValidationError.toString, errorName, null)
        }
        maybeDay.right
      // Every String writable's constraint is expressed as a
      // `<Limit>` on the fact, and limits are checked below by `this.set(path, typedValue)`, which
      // reports them as LimitError exactly as Min/Max on a Dollar are reported. Running the value
      // through StringFactory here would instead second-guess the dictionary against a hardcoded
      // list of specific patterns it knows nothing about.
      case _: StringNode => value
      case _             => return SetReturnValue(UnsupportedTypeError.toString, null, null)

    // Surface limit violations
    val rawSave = this.set(path, typedValue)
    val limitViolations = rawSave._2.map(f => LimitViolationWrapper.fromLimitViolation(f))
    if (limitViolations.nonEmpty) {
      val limitViolation = limitViolations.head
      return SetReturnValue(LimitError.toString, limitViolation.limitName, limitViolation.limit)
    }

    SetReturnValue(null, null, null)
  }

  def paths(): js.Array[String] =
    this.dictionary
      .getPaths()
      .map(path => path.toString)
      .toJSArray

  def getCollectionIds(collectionPath: String): js.Array[String] = {
    val pathWithWildcard = collectionPath + "/*"
    this
      .getCollectionPaths(pathWithWildcard)
      .map(path => path.replace(collectionPath + "/#", ""))
      .toJSArray
  }

  @JSExport("toJSON")
  def toJson(indent: Int = -1): String =
    this.persister.toJson(indent)

  def explainAndSolve(path: String): js.Array[js.Array[String]] =
    val rawExpl = this.explain(path)
    return rawExpl.solves.map(l => l.map(p => p.toString).toJSArray).toJSArray

  @JSExport("checkPersister")
  def jsCheckPersister(): js.Array[PersisterSyncIssueWrapper] =
    val raw = this.checkPersister();
    return raw
      .map(f => PersisterSyncIssueWrapper.fromPersisterSyncIssue(f))
      .toJSArray

@JSExportTopLevel("GraphFactory")
object JSGraph:
  @JSExport("apply")
  def apply(dictionary: FactDictionary): JSGraph = new JSGraph(dictionary, InMemoryPersister())

  @JSExport("fromJSON")
  def fromJSON(dictionary: FactDictionary, serializedFactGraph: String): JSGraph = {
    val persister = InMemoryPersister(serializedFactGraph)
    new JSGraph(dictionary, persister)
  }

final class LimitViolationWrapper(
    var limitName: String,
    var factPath: String,
    val level: String,
    val limit: String,
    val actual: String,
) extends js.Object

object LimitViolationWrapper {
  def fromLimitViolation(lv: LimitViolation) =
    new LimitViolationWrapper(
      lv.limitName,
      lv.factPath,
      lv.LimitLevel.toString(),
      lv.limit,
      lv.actual,
    )
}

final class PersisterSyncIssueWrapper(
    val path: String,
    val message: String,
) extends js.Object

object PersisterSyncIssueWrapper {
  def fromPersisterSyncIssue(issue: PersisterSyncIssue) =
    new PersisterSyncIssueWrapper(
      issue.path,
      issue.message,
    )
}
