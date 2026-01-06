package gov.irs.factgraph.compnodes

import gov.irs.factgraph.definitions.fact.*
import gov.irs.factgraph.monads.Result
import gov.irs.factgraph.types.Day
import gov.irs.factgraph.FactDictionary
import org.scalatest.funspec.AnyFunSpec

class PayrollMonthsBetweenSpec extends AnyFunSpec:
  describe("PayrollMonthsBetween") {
    it("handles basic date calculations") {
      val node = CompNode.fromDerivedConfig(
        new CompNodeConfigElement(
          "PayrollMonthsBetween",
          Seq(
            new CompNodeConfigElement(
              "StartDate",
              Seq(
                new CompNodeConfigElement(
                  "Day",
                  Seq.empty,
                  CommonOptionConfigTraits.value("2025-01-15"),
                ),
              ),
            ),
            new CompNodeConfigElement(
              "EndDate",
              Seq(
                new CompNodeConfigElement(
                  "Day",
                  Seq.empty,
                  CommonOptionConfigTraits.value("2025-02-28"),
                ),
              ),
            ),
          ),
        ),
      )

      assert(node.get(0) == Result.Complete(2))
    }

    it("handles end of month oddities") {
      val node = CompNode.fromDerivedConfig(
        new CompNodeConfigElement(
          "PayrollMonthsBetween",
          Seq(
            new CompNodeConfigElement(
              "StartDate",
              Seq(
                new CompNodeConfigElement(
                  "Day",
                  Seq.empty,
                  CommonOptionConfigTraits.value("2025-01-31"),
                ),
              ),
            ),
            new CompNodeConfigElement(
              "EndDate",
              Seq(
                new CompNodeConfigElement(
                  "Day",
                  Seq.empty,
                  CommonOptionConfigTraits.value("2025-02-28"),
                ),
              ),
            ),
          ),
        ),
      )

      assert(node.get(0) == Result.Complete(1))
    }

    it("handles when starting month is earlier") {
      val node = CompNode.fromDerivedConfig(
        new CompNodeConfigElement(
          "PayrollMonthsBetween",
          Seq(
            new CompNodeConfigElement(
              "StartDate",
              Seq(
                new CompNodeConfigElement(
                  "Day",
                  Seq.empty,
                  CommonOptionConfigTraits.value("2025-02-28"),
                ),
              ),
            ),
            new CompNodeConfigElement(
              "EndDate",
              Seq(
                new CompNodeConfigElement(
                  "Day",
                  Seq.empty,
                  CommonOptionConfigTraits.value("2025-05-31"),
                ),
              ),
            ),
          ),
        ),
      )

      assert(node.get(0) == Result.Complete(4))
    }
  }
