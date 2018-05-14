package hu.matan

// https://mostafa-asg.github.io/post/writing-internal-dsl-in-scala/

object DfaDsl {

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
        transition on '0' from S1 to S2
        transition on '1' from S1 to S1
        transition on '0' from S2 to S2
        transition on '1' from S2 to S1
        transition on '0' from S3 to S3
        transition on '1' from S3 to S3
      }
    } startFrom S0 withInput "010101011110110110000"

    val hasInputAccepted = dfa.run

    println(s"'${dfa.input}' is " + (
      if (hasInputAccepted) "accepted" else "not accepted")
    )
  }

  sealed trait State

  final case object S0 extends State

  final case object S1 extends State

  final case object S2 extends State

  final case object S3 extends State


  class DfaBuilder() {
    var states: Seq[State] = Seq()
    var finalStates: Seq[State] = Seq()
    var transitions: Seq[Transition] = Seq()

    def states(statesFunc: => Seq[State]): Unit = states = statesFunc

    def finalStates(statesFunc: => Seq[State]): Unit = finalStates = statesFunc

    def transitions(transitionsFunc: TransitionBuilder => Unit): Dfa = {
      val tb = new TransitionBuilder
      transitionsFunc(tb)
      transitions = tb.transitions

      buildDfa
    }

    private def buildDfa = new Dfa(states, finalStates, transitions)
  }

  class Dfa(states: Seq[State], finalStates: Seq[State], allTransitions: Seq[Transition]) {

    def startFrom(s: State) = new startFrom(s)

    class startFrom(startState: State) {

      def withInput(in: String) = new withInput(in)

      class withInput(val input: String) {

        def run: Boolean = {

          def loop(from: State, input: Char, futureInput: List[Char], remainingTransitions: Seq[Transition]): Boolean =
            remainingTransitions match {

              case Transition(`input`, `from`, to) :: _ =>
                if (futureInput.isEmpty)
                  finalStates contains to
                else
                  loop(to, futureInput.head, futureInput.tail, allTransitions)

              case Transition(_, _, _) :: Nil =>
                throw new Exception(
                  s"You must provide transition function for input '$input' when state is $from"
                )

              case _ =>
                loop(from, input, futureInput, remainingTransitions.tail)
            }


          val list = input.toList
          loop(startState, list.head, list.tail, allTransitions)
        }
      }

    }

  }

  case class Transition(on: Char, from: State, to: State)

  class TransitionBuilder {
    var transitions: Seq[Transition] = Seq()

    def on(in: Char) = new from(in)

    class from(on: Char) {
      def from(s: State) = new to(s)

      class to(from: State) {
        def to(to: State): Unit = {
          transitions = transitions :+ Transition(on, from, to)
        }
      }

    }

  }

  def newDfa(dfaFunc: DfaBuilder => Dfa): Dfa = {
    dfaFunc(new DfaBuilder())
  }
}
