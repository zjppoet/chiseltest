package chisel3.tests

import org.scalatest._

import chisel3._
import chisel3.tester._

class NestedBundleElaborationTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "elaboration depending on where bundle is declared"

  class DoubleElements extends Bundle {
    val a = UInt(8.W)
    val b = UInt(8.W)
  }

  class NestedBundle extends Bundle {
    val c = UInt(8.W)
    val d = new DoubleElements
  }

  it should "elaborates" in {
    test(new PassthroughModule(new NestedBundle)) { _ =>
    }
  }

  it should "fails to elaborate" in {
    class OuterBundle extends Bundle {
      val c = UInt(8.W)
      val d = new DoubleElements
    }

    test(new PassthroughModule(new OuterBundle)) { _ =>
    }
  }
}
