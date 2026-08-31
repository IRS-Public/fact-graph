package gov.irs.factgraph

import gov.irs.factgraph.definitions.fact.{
  CommonOptionConfigTraits,
  CompNodeConfigElement,
  CompNodeConfigTrait,
  FactConfigElement,
  LimitConfigTrait,
  LimitLevel,
  WritableConfigElement,
}
import org.scalatest.funspec.AnyFunSpec

/** `JSGraph.set` is the only way a browser writes a fact: `fg-set.js` hands it the raw form value as a string and it
  * picks the conversion from the fact definition. A node type missing from that match is not a type error at build time
  * — it is a question the user cannot answer, reported as the generic "something went wrong". String was that node type
  * until this spec existed.
  */
class JSGraphSpec extends AnyFunSpec:

  private def matchLimit(pattern: String) = new LimitConfigTrait:
    override def operation: String = "Match"
    override def level: LimitLevel = LimitLevel.Error
    override def node: CompNodeConfigTrait =
      CompNodeConfigElement("String", Seq.empty, CommonOptionConfigTraits.value(pattern))

  private def graphWith(writable: WritableConfigElement): JSGraph =
    val dictionary = FactDictionary()
    FactDefinition.fromConfig(FactConfigElement("/test", Some(writable), None, None))(using dictionary)
    new JSGraph(dictionary, persisters.InMemoryPersister())

  describe("JSGraph.set") {
    describe("on a String writable") {
      it("accepts a value, rather than reporting UnsupportedTypeError") {
        val graph = graphWith(WritableConfigElement("String", Seq.empty, Seq.empty, None))
        val res = graph.set("/test", "20814")
        assert(res.errorType == null)
        assert(graph.get("/test").value.contains("20814"))
      }

      it("reports a Match limit violation as a LimitError naming the limit") {
        val writable = WritableConfigElement("String", Seq.empty, Seq(matchLimit("^\\d{5}(-\\d{4})?$")), None)
        val graph = graphWith(writable)

        assert(graph.set("/test", "20814").errorType == null)
        assert(graph.set("/test", "20814-1234").errorType == null)

        val res = graph.set("/test", "not a zip")
        assert(res.errorType == "LimitError")
        assert(res.errorName == "Match")
      }
    }

    describe("on a Dollar writable") {
      it("still reports an unparseable value as a ValidationError") {
        val graph = graphWith(WritableConfigElement("Dollar", Seq.empty, Seq.empty, None))
        assert(graph.set("/test", "not a dollar").errorType == "ValidationError")
      }
    }
  }
