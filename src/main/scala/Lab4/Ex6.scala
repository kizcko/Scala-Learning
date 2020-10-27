package Lab4 

import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.library.atomic.discrete._

object Ex6 {
    def main(args: Array[String]) {
        def doubles = {
      val die1 = FromRange(1, 7)
      val die2 = FromRange(1, 7)
      die1 === die2
    }

    val jail = doubles && doubles && doubles

    def doublesX3={
        val die3 = FromRange(1, 7)
        val die4 = FromRange(1, 7)
        val die5 = FromRange(1, 7)
        val die6 = FromRange(1, 7)
        val die7 = FromRange(1, 7)
        val die8 = FromRange(1, 7)
        (die3 === die4) && (die5 === die6) && (die7 === die8) && (die3 !== die5) && (die5 !== die7) && (die3 !== die7)
    }


    val jail2 = doublesX3
    println(VariableElimination.probability(jail, true))
    println(VariableElimination.probability(jail2, true))


	}
}