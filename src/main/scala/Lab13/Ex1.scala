package Lab13
import com.cra.figaro.language._

import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored.beliefpropagation._

import com.cra.figaro.algorithm.sampling._

object Ex1{
	def main(args: Array[String]){

		abstract class State {
			val confident: Element[Boolean]
			def possession: Element[Boolean] = If(confident, Flip(0.7), Flip(0.3))
			}

		class InitialState() extends State {
			val confident = Flip(0.4)
		}

		class NextState(current: State) extends State {
			val confident = If(current.confident, Flip(0.6), Flip(0.3))
		}

		
		def stateSequence(n: Int): List[State] = {
			if (n == 0) List(new InitialState())
			else {
				val last :: rest = stateSequence(n - 1)
				new NextState(last) :: last :: rest
			}
		}
		
		def run(algorithm: OneTimeMPE, obsSeq: List[Boolean]) {
			val stateSeq = stateSequence(obsSeq.length)
			for { i <- 0 until obsSeq.length } {
				stateSeq(i).possession.observe(obsSeq(obsSeq.length - 1 - i))
			}
			algorithm.start()
			for { i <- 0 until obsSeq.length } {
				print(algorithm.mostLikelyValue(stateSeq(i).confident) + " " + obsSeq(i))
				println()
			}
			println()
			algorithm.kill()
		}
		
		val steps = 10
		val obsSeq = List.fill(steps)(scala.util.Random.nextBoolean())
		println()
		println("Variable Elimination")
		run(MPEVariableElimination(), obsSeq)
		println("Belief Propagation")
		run(MPEBeliefPropagation(10),obsSeq)
		println("Metropolis Hastings Annealer")
		run(MetropolisHastingsAnnealer(100000, ProposalScheme.default, Schedule.default(1.0)), obsSeq) 
		
		
		}
		
		
	}