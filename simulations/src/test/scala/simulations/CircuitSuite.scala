package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

  test("orGate") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("orGate2") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("demux") {
    val in, control0,control1 = new Wire
    val out0, out1, out2, out3 = new Wire

    val controls = List(control1, control0);
    val outs = List(out3, out2, out1, out0);

    demux(in, controls, outs);

    for (c <- controls) c.setSignal(false)
    run

    for (o <- outs) assert(o.getSignal == false, "out should be false")

    in.setSignal(true)

    control0.setSignal(false)
    control1.setSignal(false)
    run
    assert(out0.getSignal == true, "out0 == true")
    assert(out1.getSignal == false, "out1 == false")
    assert(out2.getSignal == false, "out2 == false")
    assert(out3.getSignal == false, "out3 == false")


    control0.setSignal(true)
    control1.setSignal(false)
    run
    assert(out0.getSignal == false, "out0 == false")
    assert(out1.getSignal == true, "out1 == true")
    assert(out2.getSignal == false, "out2 == false")
    assert(out3.getSignal == false, "out3 == false")

    control0.setSignal(false)
    control1.setSignal(true)
    run
    assert(out0.getSignal == false, "out0 == false")
    assert(out1.getSignal == false, "out1 == false")
    assert(out2.getSignal == true, "out2 == true")
    assert(out3.getSignal == false, "out3 == false")

    control0.setSignal(true)
    control1.setSignal(true)
    run
    assert(out0.getSignal == false, "out0 == false")
    assert(out1.getSignal == false, "out1 == false")
    assert(out2.getSignal == false, "out2 == false")
    assert(out3.getSignal == true, "out3 == true")
  }

}
