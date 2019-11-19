package chisel3.tests

import org.scalatest._

import chisel3._
import chisel3.tester._
import chisel3.experimental.BundleLiterals._

class BundleLiteralsSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  class DoubleElements extends Bundle {
    val a = UInt(8.W)
    val b = UInt(8.W)
  }

  class NestedBundle extends Bundle {
    val c = UInt(8.W)
    val d = new DoubleElements
  }

  it should "poke Bundle literals" in {
    test(new Module {
      val io = IO(new Bundle {
        val in = Input(new DoubleElements)

        val aOut = Output(UInt(8.W))
        val bOut = Output(UInt(8.W))
      })
      io.aOut := io.in.a
      io.bOut := io.in.b
    }) { c =>
      c.io.in.poke(chiselTypeOf(c.io.in).Lit(_.a -> 0.U, _.b -> 1.U))
      c.io.aOut.expect(0.U)
      c.io.bOut.expect(1.U)

      c.io.in.poke(chiselTypeOf(c.io.in).Lit(_.a -> 2.U, _.b -> 5.U))
      c.io.aOut.expect(2.U)
      c.io.bOut.expect(5.U)
    }
  }

  it should "expect Bundle literals" in {
    test(new PassthroughModule(new DoubleElements)) { c =>
      c.in.poke(chiselTypeOf(c.in).Lit(_.a -> 0.U, _.b -> 1.U))
      c.out.expect(chiselTypeOf(c.in).Lit(_.a -> 0.U, _.b -> 1.U))
      c.in.poke(chiselTypeOf(c.in).Lit(_.a -> 2.U, _.b -> 5.U))
      c.out.expect(chiselTypeOf(c.in).Lit(_.a -> 2.U, _.b -> 5.U))
    }
  }

  it should "fail on expect mismatch" in {
    assertThrows[exceptions.TestFailedException] {
      test(new PassthroughModule(new DoubleElements)) { c =>
        c.in.poke(chiselTypeOf(c.in).Lit(_.a -> 0.U, _.b -> 1.U))
        c.out.expect(chiselTypeOf(c.in).Lit(_.a -> 0.U, _.b -> 2.U))
      }
    }
  }

  it should "return a Bundle literal when peeking" in {
    test(new PassthroughModule(new DoubleElements)) { c =>
      c.in.poke(chiselTypeOf(c.in).Lit(_.a -> 0.U, _.b -> 1.U))
      val output = c.out.peek()
      output.a.litValue should be (0)
      output.b.litValue should be (1)
    }
  }

  it should "roundtrip Bundle literals" in {
    test(new PassthroughModule(new DoubleElements)) { c =>
      c.in.poke(chiselTypeOf(c.in).Lit(_.a -> 0.U, _.b -> 1.U))
      c.in.poke(c.out.peek())
      c.out.expect(chiselTypeOf(c.in).Lit(_.a -> 0.U, _.b -> 1.U))
    }
  }

  it should "provide helpful exception when a bundle field is missing in poke" in {
    val caught = intercept[LiteralValueException] {
      test(new PassthroughModule(new DoubleElements)) { c =>
        c.in.poke(chiselTypeOf(c.in).Lit(_.a -> 0.U))
      }
    }
    caught.getMessage.contains("Bundle field in.b was not given a value during poke") should be (true)
  }

  it should "provide helpful exception when a poked UInt value is not a literal" in {
    val caught = intercept[LiteralValueException] {
      test(new PassthroughModule(new DoubleElements)) { c =>
        c.in.a.poke(UInt(8.W))
      }
    }
    caught.getMessage.contains("UInt in.a was not given a literal value during poke") should be (true)
  }

  it should "round trip nested Bundle literals, showing two ways of declaring sub-bundle" in {
    test(new PassthroughModule(new NestedBundle)) { dut =>

      dut.in.poke(chiselTypeOf(dut.in).Lit(_.c -> 7.U, _.d -> (chiselTypeOf(dut.in.d).Lit(_.a -> 0.U, _.b -> 1.U))))
      dut.in.poke(dut.out.peek())
      dut.in.expect(chiselTypeOf(dut.in).Lit(_.c -> 7.U, _.d -> (chiselTypeOf(dut.in.d).Lit(_.a -> 0.U, _.b -> 1.U))))

      dut.in.poke(chiselTypeOf(dut.in).Lit(_.c -> 7.U, _.d -> (new DoubleElements).Lit(_.a -> 0.U, _.b -> 1.U)))
      dut.in.poke(dut.out.peek())
//      dut.in.expect(chiselTypeOf(dut.in).Lit(_.c -> 7.U, _.d -> (new DoubleElements).Lit(_.a -> 0.U, _.b -> 1.U)))
      dut.in.expect(chiselTypeOf(dut.in).Lit(_.c -> 7.U, _.d -> (new DoubleElements).Lit(_.a -> 0.U, _.b -> 1.U)))
    }
  }
}
