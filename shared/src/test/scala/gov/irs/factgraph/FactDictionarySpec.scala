package gov.irs.factgraph

import gov.irs.factgraph.compnodes.BooleanNode
import gov.irs.factgraph.compnodes.CollectionItemNode
import gov.irs.factgraph.compnodes.RootNode
import gov.irs.factgraph.definitions.*
import gov.irs.factgraph.definitions.fact.{
  CommonOptionConfigTraits,
  CompNodeConfigElement,
  FactConfigElement,
  FactConfigTrait,
}
import gov.irs.factgraph.definitions.fact.WritableConfigElement
import gov.irs.factgraph.definitions.meta.{ EnumDeclarationTrait, MetaConfigTrait }
import gov.irs.factgraph.types.Collection
import java.util.UUID
import org.scalatest.funspec.AnyFunSpec

class FactDictionarySpec extends AnyFunSpec:
  describe("FactDictionary") {
    describe("$apply") {
      it("always creates a root fact") {
        val dictionary = FactDictionary()
        assert(dictionary(Path.Root).get.value.isInstanceOf[RootNode])
      }
    }

    describe(".freeze") {
      it("prevents new definitions from being added after frozen") {
        assertThrows[UnsupportedOperationException] {
          val dictionary = FactDictionary()
          dictionary.freeze()
          FactDefinition.fromConfig(
            FactConfigElement(
              "/test",
              None,
              Some(
                CompNodeConfigElement(
                  "Int",
                  Seq.empty,
                  CommonOptionConfigTraits.value("42"),
                ),
              ),
              None,
            ),
          )(using dictionary)
        }
      }
    }

    describe(".getOptionsPathForEnum") {
      it("returns None when called on a non-enum fact") {
        val dictionary = FactDictionary();

        FactDefinition.fromConfig(
          FactConfigElement(
            "/test",
            None,
            Some(
              CompNodeConfigElement(
                "Int",
                Seq.empty,
                CommonOptionConfigTraits.value("42"),
              ),
            ),
            None,
          ),
        )(using dictionary)

        assert(dictionary.getOptionsPathForEnum("/test") == None)
      }

      it("returns an option path when called on an enum") {
        val dictionary = FactDictionary();
        val OPTIONS_PATH = "/options-path"

        FactDefinition.fromConfig(
          FactConfigElement(
            "/enum-path",
            None,
            Some(
              new CompNodeConfigElement(
                "Enum",
                Seq.empty,
                CommonOptionConfigTraits.create(
                  Seq(
                    (
                      CommonOptionConfigTraits.ENUM_OPTIONS_PATH,
                      OPTIONS_PATH,
                    ),
                    (CommonOptionConfigTraits.VALUE, "C"),
                  ),
                ),
              ),
            ),
            None,
          ),
        )(using dictionary)

        assert(
          dictionary.getOptionsPathForEnum("/enum-path").get == OPTIONS_PATH,
        )
      }
    }

    describe("addMeta") {
      it("adds a meta if you directly call directly") {
        val dictionary = FactDictionary()
        val initialMeta = dictionary.getMeta()
        dictionary.addMeta(
          new MetaConfigTrait:
            override def version: String = "1",
        )
        assert(initialMeta.version != dictionary.getMeta().version)
        assert(dictionary.getMeta().version == "1")
        assert(initialMeta.version == "Invalid")
      }

      it("adds a meta configuration to the fact graph") {
        val dictionary = FactDictionary()
        val initialMeta = dictionary.getMeta()
        val factDictionaryConfigTrait = new FactDictionaryConfigTrait:
          override def facts: Iterable[FactConfigTrait] = Seq.empty
          override def meta: MetaConfigTrait = new MetaConfigTrait:
            override def version: String = "1"
        // this calls addMeta internally
        Meta.fromConfig(factDictionaryConfigTrait.meta, dictionary)
        assert(initialMeta.version != dictionary.getMeta().version)
        assert(dictionary.getMeta().version == "1")
        assert(initialMeta.version == "Invalid")
      }

      it("it must be added to the dictionary before it is frozen") {
        assertThrows[UnsupportedOperationException] {
          val dictionary = FactDictionary()
          dictionary.freeze()
        }
      }

      it("it cannot be added into a frozen dictionary") {
        assertThrows[UnsupportedOperationException] {
          val dictionary = FactDictionary()
          val factDictionaryConfigTrait = new FactDictionaryConfigTrait:
            override def facts: Iterable[FactConfigTrait] = Seq.empty
            override def meta: MetaConfigTrait = new MetaConfigTrait:
              override def version: String = "1"
          Meta.fromConfig(factDictionaryConfigTrait.meta, dictionary)
          dictionary.freeze()
          val factDictionaryConfigTrait2 = new FactDictionaryConfigTrait:
            override def facts: Iterable[FactConfigTrait] = Seq.empty
            override def meta: MetaConfigTrait = new MetaConfigTrait:
              override def version: String = "2"
          Meta.fromConfig(factDictionaryConfigTrait2.meta, dictionary)
        }
      }
    }

    describe("getDefinition") {

      it("returns an object definition for a path") {
        val dictionary = FactDictionary()
        val definition = FactDefinition.fromConfig(
          FactConfigElement(
            "/test",
            None,
            Some(
              CompNodeConfigElement(
                "Int",
                Seq.empty,
                CommonOptionConfigTraits.value("42"),
              ),
            ),
            None,
          ),
        )(using dictionary)
        assert(dictionary.getDefinition("/test") !== null)
        assert(dictionary.getDefinition("/test") === definition)
      }

      it("returns null when no definition is found for the path") {
        val dictionary = FactDictionary()
        assert(dictionary.getDefinition("/nonexistent-path") == null)
      }

      val dictionary = FactDictionary();
      FactDefinition.fromConfig(
        FactConfigElement(
          "/collection",
          Some(
            new WritableConfigElement("Collection"),
          ),
          None,
          None,
        ),
      )(using dictionary)
      FactDefinition.fromConfig(
        FactConfigElement(
          "/collection/*/bool",
          None,
          Some(
            new CompNodeConfigElement("True"),
          ),
          None,
        ),
      )(using dictionary)
      FactDefinition.fromConfig(
        FactConfigElement(
          "/collectionAlias",
          None,
          Some(
            new CompNodeConfigElement(
              "IndexOf",
              Seq(
                new CompNodeConfigElement(
                  "Collection",
                  Seq(
                    new CompNodeConfigElement(
                      "Dependency",
                      Seq.empty,
                      "/collection",
                    ),
                  ),
                ),
                new CompNodeConfigElement(
                  "Index",
                  Seq(
                    CompNodeConfigElement(
                      "Int",
                      Seq(),
                      CommonOptionConfigTraits.value("0"),
                    ),
                  ),
                ),
              ),
            ),
          ),
          None,
        ),
      )(using dictionary)

      val graph = Graph(dictionary)
      val uuid1: UUID = UUID.fromString("4a772c04-5445-4623-b216-edca2c79698a")

      for {
        result <- graph(Path("/collection"))
        fact <- result
      } fact.set(Collection(Vector(uuid1)))

      graph.save()

      it("returns the definition for a path with a collection alias") {
        assert(dictionary.getDefinition("/collectionAlias/bool") !== null)
      }
    }
  }
