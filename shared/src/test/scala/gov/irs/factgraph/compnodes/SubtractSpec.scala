package gov.irs.factgraph.compnodes

import gov.irs.factgraph.definitions.fact.*
import gov.irs.factgraph.monads.Result
import gov.irs.factgraph.types.*
import gov.irs.factgraph.FactDictionary
import org.scalatest.funspec.AnyFunSpec

class SubtractSpec extends AnyFunSpec:
  describe("Subtract") {
    it("subtracts Ints") {
      val node = CompNode.fromDerivedConfig(
        new CompNodeConfigElement(
          "Subtract",
          Seq(
            new CompNodeConfigElement(
              "Minuend",
              Seq(
                new CompNodeConfigElement(
                  "Int",
                  Seq.empty,
                  CommonOptionConfigTraits.value("12"),
                ),
              ),
            ),
            new CompNodeConfigElement(
              "Subtrahends",
              Seq(
                new CompNodeConfigElement(
                  "Int",
                  Seq.empty,
                  CommonOptionConfigTraits.value("1"),
                ),
                new CompNodeConfigElement(
                  "Int",
                  Seq.empty,
                  CommonOptionConfigTraits.value("2"),
                ),
                new CompNodeConfigElement(
                  "Int",
                  Seq.empty,
                  CommonOptionConfigTraits.value("3"),
                ),
              ),
            ),
          ),
        ),
      )

      assert(node.get(0) == Result.Complete(6))
    }

    it("subtracts Rationals") {
      val node = CompNode.fromDerivedConfig(
        new CompNodeConfigElement(
          "Subtract",
          Seq(
            new CompNodeConfigElement(
              "Minuend",
              Seq(
                new CompNodeConfigElement(
                  "Rational",
                  Seq.empty,
                  CommonOptionConfigTraits.value("2/1"),
                ),
              ),
            ),
            new CompNodeConfigElement(
              "Subtrahends",
              Seq(
                new CompNodeConfigElement(
                  "Rational",
                  Seq.empty,
                  CommonOptionConfigTraits.value("1/2"),
                ),
                new CompNodeConfigElement(
                  "Rational",
                  Seq.empty,
                  CommonOptionConfigTraits.value("2/3"),
                ),
                new CompNodeConfigElement(
                  "Rational",
                  Seq.empty,
                  CommonOptionConfigTraits.value("3/4"),
                ),
              ),
            ),
          ),
        ),
      )

      assert(node.get(0) == Result.Complete(Rational("1/12")))
    }

    it("subtracts Dollars") {
      val node = CompNode.fromDerivedConfig(
        new CompNodeConfigElement(
          "Subtract",
          Seq(
            new CompNodeConfigElement(
              "Minuend",
              Seq(
                new CompNodeConfigElement(
                  "Dollar",
                  Seq.empty,
                  CommonOptionConfigTraits.value("15.00"),
                ),
              ),
            ),
            new CompNodeConfigElement(
              "Subtrahends",
              Seq(
                new CompNodeConfigElement(
                  "Dollar",
                  Seq.empty,
                  CommonOptionConfigTraits.value("1.23"),
                ),
                new CompNodeConfigElement(
                  "Dollar",
                  Seq.empty,
                  CommonOptionConfigTraits.value("4.56"),
                ),
                new CompNodeConfigElement(
                  "Dollar",
                  Seq.empty,
                  CommonOptionConfigTraits.value("7.89"),
                ),
              ),
            ),
          ),
        ),
      )

      assert(node.get(0) == Result.Complete(Dollar("1.32")))
    }

    it("subtracts a long sequence of mixed types") {
      val node = CompNode.fromDerivedConfig(
        new CompNodeConfigElement(
          "Subtract",
          Seq(
            new CompNodeConfigElement(
              "Minuend",
              Seq(
                new CompNodeConfigElement(
                  "Int",
                  Seq.empty,
                  CommonOptionConfigTraits.value("40"),
                ),
              ),
            ),
            new CompNodeConfigElement(
              "Subtrahends",
              Seq(
                new CompNodeConfigElement(
                  "Int",
                  Seq.empty,
                  CommonOptionConfigTraits.value("0"),
                ),
                new CompNodeConfigElement(
                  "Int",
                  Seq.empty,
                  CommonOptionConfigTraits.value("1"),
                ),
                new CompNodeConfigElement(
                  "Rational",
                  Seq.empty,
                  CommonOptionConfigTraits.value("0/1"),
                ),
                new CompNodeConfigElement(
                  "Rational",
                  Seq.empty,
                  CommonOptionConfigTraits.value("2/3"),
                ),
                new CompNodeConfigElement(
                  "Int",
                  Seq.empty,
                  CommonOptionConfigTraits.value("4"),
                ),
                new CompNodeConfigElement(
                  "Dollar",
                  Seq.empty,
                  CommonOptionConfigTraits.value("5.67"),
                ),
                new CompNodeConfigElement(
                  "Rational",
                  Seq.empty,
                  CommonOptionConfigTraits.value("8/9"),
                ),
                new CompNodeConfigElement(
                  "Dollar",
                  Seq.empty,
                  CommonOptionConfigTraits.value("10.11"),
                ),
                new CompNodeConfigElement(
                  "Int",
                  Seq.empty,
                  CommonOptionConfigTraits.value("12"),
                ),
              ),
            ),
          ),
        ),
      )

      assert(node.get(0) == Result.Complete(Dollar("5.66")))
    }

    describe("when subtracting an Int and a Rational") {
      it("returns a Rational") {
        val node = CompNode.fromDerivedConfig(
          new CompNodeConfigElement(
            "Subtract",
            Seq(
              new CompNodeConfigElement(
                "Minuend",
                Seq(
                  new CompNodeConfigElement(
                    "Int",
                    Seq.empty,
                    CommonOptionConfigTraits.value("1"),
                  ),
                ),
              ),
              new CompNodeConfigElement(
                "Subtrahends",
                Seq(
                  CompNodeConfigElement(
                    "Rational",
                    Seq.empty,
                    CommonOptionConfigTraits.value("2/3"),
                  ),
                ),
              ),
            ),
          ),
        )

        assert(node.get(0) == Result.Complete(Rational("1/3")))
      }
    }

    describe("when subtracting an Int and a Dollar") {
      it("returns a Dollar") {
        val node = CompNode.fromDerivedConfig(
          new CompNodeConfigElement(
            "Subtract",
            Seq(
              new CompNodeConfigElement(
                "Minuend",
                Seq(
                  new CompNodeConfigElement(
                    "Int",
                    Seq.empty,
                    CommonOptionConfigTraits.value("5"),
                  ),
                ),
              ),
              new CompNodeConfigElement(
                "Subtrahends",
                Seq(
                  new CompNodeConfigElement(
                    "Dollar",
                    Seq.empty,
                    CommonOptionConfigTraits.value("2.34"),
                  ),
                ),
              ),
            ),
          ),
        )

        assert(node.get(0) == Result.Complete(Dollar("2.66")))
      }
    }

    describe("when subtracting a Rational and a Dollar") {
      it("returns a Dollar") {
        val node = CompNode.fromDerivedConfig(
          new CompNodeConfigElement(
            "Subtract",
            Seq(
              new CompNodeConfigElement(
                "Minuend",
                Seq(
                  new CompNodeConfigElement(
                    "Rational",
                    Seq.empty,
                    CommonOptionConfigTraits.value("5/4"),
                  ),
                ),
              ),
              new CompNodeConfigElement(
                "Subtrahends",
                Seq(
                  new CompNodeConfigElement(
                    "Dollar",
                    Seq.empty,
                    CommonOptionConfigTraits.value("1.23"),
                  ),
                ),
              ),
            ),
          ),
        )

        assert(node.get(0) == Result.Complete(Dollar("0.02")))
      }
    }
    describe("when subtracting Days and a Day") {
      it("returns a Day") {
        val node = CompNode.fromDerivedConfig(
          new CompNodeConfigElement(
            "Subtract",
            Seq(
              new CompNodeConfigElement(
                "Minuend",
                Seq(
                  new CompNodeConfigElement(
                    "Day",
                    Seq.empty,
                    CommonOptionConfigTraits.value("2024-01-10"),
                  ),
                ),
              ),
              new CompNodeConfigElement(
                "Subtrahends",
                Seq(
                  new CompNodeConfigElement(
                    "Days",
                    Seq.empty,
                    CommonOptionConfigTraits.value("1"),
                  ),
                ),
              ),
            ),
          ),
        )

        assert(node.get(0) == Result.Complete(Day("2024-01-09")))
      }
    }
    describe("when subtracting Day and a Day") {
      it("returns an Int") {
        val node = CompNode.fromDerivedConfig(
          new CompNodeConfigElement(
            "Subtract",
            Seq(
              new CompNodeConfigElement(
                "Minuend",
                Seq(
                  new CompNodeConfigElement(
                    "Day",
                    Seq.empty,
                    CommonOptionConfigTraits.value("2024-01-20"),
                  ),
                ),
              ),
              new CompNodeConfigElement(
                "Subtrahends",
                Seq(
                  new CompNodeConfigElement(
                    "Day",
                    Seq.empty,
                    CommonOptionConfigTraits.value("2024-01-15"),
                  ),
                ),
              ),
            ),
          ),
        )

        assert(node.get(0) == Result.Complete(5))
      }
    }

    describe("when the inputs are not numbers") {
      it("throws an exception") {
        assertThrows[UnsupportedOperationException] {
          CompNode.fromDerivedConfig(
            new CompNodeConfigElement(
              "Subtract",
              Seq(
                new CompNodeConfigElement(
                  "Minuend",
                  Seq(
                    new CompNodeConfigElement(
                      "String",
                      Seq.empty,
                      CommonOptionConfigTraits.value("Hello"),
                    ),
                  ),
                ),
                new CompNodeConfigElement(
                  "Subtrahends",
                  Seq(
                    new CompNodeConfigElement(
                      "String",
                      Seq.empty,
                      CommonOptionConfigTraits.value("World"),
                    ),
                  ),
                ),
              ),
            ),
          )
        }
      }
    }

    describe("when one of the inputs is not a number") {
      it("throws an exception") {
        assertThrows[UnsupportedOperationException] {
          CompNode.fromDerivedConfig(
            new CompNodeConfigElement(
              "Subtract",
              Seq(
                new CompNodeConfigElement(
                  "Minuend",
                  Seq(
                    new CompNodeConfigElement(
                      "Int",
                      Seq.empty,
                      CommonOptionConfigTraits.value("3"),
                    ),
                  ),
                ),
                new CompNodeConfigElement(
                  "Subtrahends",
                  Seq(
                    new CompNodeConfigElement(
                      "String",
                      Seq.empty,
                      CommonOptionConfigTraits.value("Stooges"),
                    ),
                  ),
                ),
              ),
            ),
          )
        }
      }
    }
  }
