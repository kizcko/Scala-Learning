package Lab10
import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.factored._
import scala.collection.immutable.Range




object Ex1{
  def main(args: Array[String]){
		val states=10
        val investmentFraction: Array[Element[Double]]=Array.fill(1)(Constant(10.0))
        val profitIncrease: Array[Element[Double]]=Array.fill(1)(Constant(8.0))
		val capital: Array[Element[Double]]=Array.fill(states)(Constant(0.0))
		val investment: Array[Element[Double]]=Array.fill(states)(Constant(0.0))
		val profit: Array[Element[Double]]=Array.fill(states)(Constant(0.0))

		for{ i <- 1 until states} {
            investment(i) = capital(i-1)/investmentFraction(0)
			val gt400 = capital(i - 1)/profitIncrease(0)
			val lt400 = capital(i - 1)/(profitIncrease(0) + 4.0)
            profit(i) = If(investment(i) > 400.0 , gt400, lt400)
			capital(i) = capital(i-1) + profit(i) - investment(i)
        }
		capital(1).addConstraint((n:Int)=>if(n>=) 10; else 4)

		}
}