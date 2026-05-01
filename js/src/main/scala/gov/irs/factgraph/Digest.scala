package gov.irs.factgraph

import gov.irs.factgraph.definitions.fact.{
  CompNodeConfigDigestWrapper,
  CompNodeDigestWrapper,
  FactConfigElement,
  WritableConfigElementDigestWrapper,
}
import scala.scalajs.js
import scala.scalajs.js.annotation.{ JSExport, JSExportTopLevel }

// We aren't actively using DigestNodes right now so these override options
// should be double checked if we start using this
@JSExportTopLevel("DigestNodeWrapper")
class DigestNodeWrapper(
    val path: String,
    val writable: WritableConfigElementDigestWrapper | Null,
    val derived: CompNodeConfigDigestWrapper | Null,
    val placeholder: CompNodeConfigDigestWrapper | Null,
    val overrideCondition: CompNodeConfigDigestWrapper | Null,
    val overrideDefault: CompNodeConfigDigestWrapper | Null,
) extends js.Object:
  /** A digest-node is the JSON serialization of a Fact, as produced by the direct-file Java application, from the XML
    * fact dictionary. This wrapper allows us to map digest JSON nodes into FactConfigElements, handling the necessary
    * type conversion and null safety matches
    */
  def writableOption = this.writable match
    case null => None
    case node =>
      Some(WritableConfigElementDigestWrapper.toNative(node))

  def derivedOption = this.derived match
    case null => None
    case _    => Some(CompNodeDigestWrapper.toNative(this.derived))

  def placeholderOption = this.placeholder match
    case null => None
    case _    => Some(CompNodeDigestWrapper.toNative(this.placeholder))

  def overrideConditionOption = this.overrideCondition match
    case null => None
    case _    => Some(CompNodeDigestWrapper.toNative(this.overrideCondition))

  def overrideDefaultOption = this.overrideDefault match
    case null => None
    case _    => Some(CompNodeDigestWrapper.toNative(this.overrideDefault))

@JSExportTopLevel("DigestNodeWrapperFactory")
object DigestNodeWrapper:
  @JSExport
  def toNative(wrapper: DigestNodeWrapper) = FactConfigElement(
    wrapper.path,
    wrapper.writableOption,
    wrapper.derivedOption,
    wrapper.placeholderOption,
    wrapper.overrideConditionOption,
    wrapper.overrideDefaultOption,
  )
