package Lab7

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.{discrete,continuous}
import com.cra.figaro.library.collection.FixedSizeArray
import com.cra.figaro.algorithm.sampling.Importance


object golf{
	def main(args: Array[String]) {

		val golfHoles = new FixedSizeArray(18, i =>discrete.Uniform(3, 4, 5));
		val skill = continuous.Uniform(0.0, 8.0 / 13.0)

		val score = golfHoles.chain((par: Int) => 
        Chain(skill, (s:Double) => Select(
              s / 8 -> (par - 2),
              s / 2 -> (par - 1),
              s -> par,
              4.0 / 5.0 * (1.0 - 13.0 * s / 8.0) -> (par+1),
              1.0 / 5.0 * (1.0 - 13.0 * s / 8.0) -> (par+2)  
            )
          )
       ) .foldLeft(0)(_+_)


	var alg = Importance(10000, score)
    alg.start()
    alg.stop()
	val probMaiMareCa80 = alg.probability(score, (s:Int) => s >= 80)
	alg.kill()

	skill.setCondition((i: Double) => i >= 0.3)
	alg.start()
    alg.stop()
	val probSkillMaiMareCa03 = alg.probability(score, (s:Int) => s >= 80)
	alg.kill()

	println("Probabilitatea pentru scor >=80 : " + probMaiMareCa80)
	println("Probabilitatea pentru skill >= 0.3 : " + probSkillMaiMareCa03)


	}
}

