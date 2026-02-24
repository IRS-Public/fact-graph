package gov.irs.factgraph.compnodes

import gov.irs.factgraph.definitions.fact.*
import gov.irs.factgraph.monads.Result
import gov.irs.factgraph.FactDictionary
import org.scalatest.funspec.AnyFunSpec

class CeilingSpec extends AnyFunSpec:
  describe("Ceiling") {
    it("Rounds a rational to the next-highest integer") {
      val node = CompNode.fromDerivedConfig(
        new CompNodeConfigElement(
          "Ceiling",
          Seq(CompNodeConfigElement("Rational", Seq.empty, CommonOptionConfigTraits.value("1/3"))),
        ),
      )
      assert(node.get(0) == Result.Complete(1))
    }

    it("Does not change integers") {
      val integerNode = CompNode.fromDerivedConfig(
        new CompNodeConfigElement(
          "Ceiling",
          Seq(CompNodeConfigElement("Int", Seq.empty, CommonOptionConfigTraits.value("1"))),
        ),
      )
      assert(integerNode.get(0) == Result.Complete(1))
    }

    it("Rounds negative rationals to the nearest, higher, Int") {
      val node = CompNode
        .fromDerivedConfig(
          new CompNodeConfigElement(
            "Ceiling",
            Seq(CompNodeConfigElement("Rational", Seq.empty, CommonOptionConfigTraits.value("-1/2"))),
          ),
        )
      assert(node.get(0) == Result.Complete(0))
    }

    it("Does not change negative integers") {
      val integerNode = CompNode
        .fromDerivedConfig(
          new CompNodeConfigElement(
            "Ceiling",
            Seq(CompNodeConfigElement("Int", Seq.empty, CommonOptionConfigTraits.value("-1"))),
          ),
        )

      assert(integerNode.get(0) == Result.Complete(-1))
    }

    it("Rounds dollars values to the next-highest dollar") {
      val node = CompNode.fromDerivedConfig(
        new CompNodeConfigElement(
          "Ceiling",
          Seq(CompNodeConfigElement("Dollar", Seq.empty, CommonOptionConfigTraits.value("10.2"))),
        ),
      )

      assert(node.get(0) == Result.Complete(11.0))
    }

    it("Rounds negative dollars values to the next-highest dollar") {
      val node = CompNode.fromDerivedConfig(
        new CompNodeConfigElement(
          "Ceiling",
          Seq(CompNodeConfigElement("Dollar", Seq.empty, CommonOptionConfigTraits.value("-10.2"))),
        ),
      )

      assert(node.get(0) == Result.Complete(-10.0))
    }

  }
