package gov.irs.factgraph.types

import org.scalatest.funspec.AnyFunSpec

class IpPinSpec extends AnyFunSpec:
  describe("IpPin") {
    describe("preconditions for the ippin fields") {
      it("throws an error because ippin is the wrong length") {
        try
          new IpPin("123")
        catch {
          case e: IpPinValidationFailure =>
            assert(e.validationMessage.toString() == "InvalidIpPin")
        }
      }
      it("throws an error because ippin has invalid characters") {
        try
          new IpPin("abcded")
        catch {
          case e: IpPinValidationFailure =>
            assert(e.validationMessage.toString() == "InvalidIpPin")
        }
      }
      it("throws an error because pin has all zeroes") {
        try
          new IpPin("000000")
        catch {
          case e: IpPinValidationFailure =>
            assert(e.validationMessage.toString() == "InvalidAllZerosIpPin")
        }
      }
    }
    describe("with a 6 digit ippin 0-9") {
      it("succeeds") {
        new IpPin("012345")
      }
    }
    describe("round-tripping through the persister") {
      it("reads back what it writes") {
        val written = upickle.default.write(IpPin("012345"))
        assert(upickle.default.read[IpPin](written) == new IpPin("012345"))
      }
      it("parses a string without recursing") {
        assert(IpPin.parseString("012345").contains(new IpPin("012345")))
      }
    }
  }
