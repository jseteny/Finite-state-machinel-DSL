package hu.matan

object DfaDsal {

  def main(args: Array[String]): Unit = {


    val dfa = newDfa { dfa =>

      dfa states {
        Seq(S0, S1, S2, S3)
      }
      dfa finalStates {
        Seq(S2)
      }

      dfa transitions { transition =>
        transition on '0' from S0 to S1
        transition on '1' from S0 to S3
      }
    } startFrom S0 withInput "010101011110110110000"

    val hasInputAccepted = dfa.run

  }

  object S0 extends State

  object S1 extends State

  object S2 extends State

  object S3 extends State

  class DfaBuilder(dfa: Dfa) {
    private var states: Seq[State] = Seq()
    private var finalStates: Seq[State] = Seq()
    private var transitions: Seq[Transition] = Seq()

    def states(statesFunc: => Seq[State]): Unit = states = statesFunc

    def finalStates(statesFunc: => Seq[State]): Unit = finalStates = statesFunc

    def transitions(transitionsFunc: TransitionBuilder => Unit): Dfa = {
      val tb = new TransitionBuilder
      transitionsFunc(tb)
      transitions = tb.transitions
      dfa
    }
  }

  class Dfa {
    def startFrom(s: State): Dfa = this

    def withInput(in: String): Dfa = this

    def run: Boolean = ???
  }

  trait State {

  }

  class Transition {

  }

  class TransitionBuilder {
    var transitions: Seq[Transition] = Seq()

    def on(in: Char): TransitionBuilder = this

    def from(s: State): TransitionBuilder = this

    def to(s: State): Unit = {
      transitions = transitions :+ new Transition
    }
  }

  def newDfa(dfaFunc: DfaBuilder => Dfa): Dfa = {
    dfaFunc(new DfaBuilder(new Dfa))
  }
}
