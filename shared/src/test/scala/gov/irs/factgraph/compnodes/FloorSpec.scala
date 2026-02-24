package gov.irs.factgraph.compnodes

import gov.irs.factgraph.definitions.fact.*
import gov.irs.factgraph.monads.Result
import gov.irs.factgraph.FactDictionary
import org.scalatest.funspec.AnyFunSpec

class FloorSpec extends AnyFunSpec:
  describe("Floor") {
    it("Floors a rational to the next-lowest integer") {
      val node = CompNode
        .fromDerivedConfig(
          new CompNodeConfigElement(
            "Floor",
            Seq(CompNodeConfigElement("Rational", Seq.empty, CommonOptionConfigTraits.value("1/3"))),
          ),
        )
    }
    it("Does not change integers") {
      val integerNode = CompNode
        .fromDerivedConfig(
          new CompNodeConfigElement(
            "Floor",
            Seq(CompNodeConfigElement("Int", Seq.empty, CommonOptionConfigTraits.value("1"))),
          ),
        )

      assert(integerNode.get(0) == Result.Complete(1))
    }

    it("Floors a negative rational to the next-lowest integer") {
      val node = CompNode
        .fromDerivedConfig(
          new CompNodeConfigElement(
            "Floor",
            Seq(CompNodeConfigElement("Rational", Seq.empty, CommonOptionConfigTraits.value("-1/2"))),
          ),
        )
      assert(node.get(0) == Result.Complete(-1))
    }

    it("Does not change negative integers") {
      val integerNode = CompNode
        .fromDerivedConfig(
          new CompNodeConfigElement(
            "Floor",
            Seq(CompNodeConfigElement("Int", Seq.empty, CommonOptionConfigTraits.value("-1"))),
          ),
        )
      assert(integerNode.get(0) == Result.Complete(-1))
    }

    it("Rounds down dollars values") {
      val node = CompNode.fromDerivedConfig(
        new CompNodeConfigElement(
          "Floor",
          Seq(CompNodeConfigElement("Dollar", Seq.empty, CommonOptionConfigTraits.value("10.2"))),
        ),
      )

      assert(node.get(0) == Result.Complete(10.0))
    }

    it("Rounds negative dollars values to the next-lowest dollar") {
      val node = CompNode.fromDerivedConfig(
        new CompNodeConfigElement(
          "Floor",
          Seq(CompNodeConfigElement("Dollar", Seq.empty, CommonOptionConfigTraits.value("-10.2"))),
        ),
      )

      assert(node.get(0) == Result.Complete(-11.0))
    }
  }
