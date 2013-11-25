package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction: Action = () => {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction: Action = () => {
      val a1Signal = a1.getSignal
      val a2Signal = a2.getSignal
      afterDelay(OrGateDelay){ output.setSignal(a1Signal | a2Signal) }
    }
    a1.addAction(orAction)
    a2.addAction(orAction)
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    // this relies on !(!a1 & !a2) <=> !(!(a|b)) <=> a|b
    val invertedA1 = new Wire
    inverter(a1, invertedA1)

    val invertedA2 = new Wire
    inverter(a2, invertedA2)

    val invertedA1AndInvertedA2 = new Wire
    andGate(invertedA1, invertedA2, invertedA1AndInvertedA2)

    inverter(invertedA1AndInvertedA2, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    c match {
      case Nil => {
        val w = new Wire
        inverter(in,w)
        inverter(w, out.head)
      }

      case c::cs => {
        val out1 = new Wire
        andGate(in, c, out1)

        val invertedC = new Wire
        val out2 = new Wire
        inverter(c, invertedC)
        andGate(in,invertedC, out2)

        demux(out1, cs, out.take(out.length / 2))
        demux(out2, cs, out.drop(out.length / 2))
      }
    }
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
