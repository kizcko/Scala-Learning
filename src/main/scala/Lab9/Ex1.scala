package Lab9
import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.factored._
import scala.collection.immutable.Range



object Ex1{
  def main(args: Array[String]){
	  	val r = new scala.util.Random
		val nrCapitole=10
		val studied: Array[Element[Boolean]]=Array.fill(nrCapitole)(Constant(false))
		val booleanMark: Array[Element[Boolean]]=Array.fill(nrCapitole)(Constant(false))
		val booleanPassed: Array[Element[Boolean]]=Array.fill(nrCapitole)(Constant(false))

		val intMark: Array[Element[Int]]=Array.fill(nrCapitole)(Constant(0))
		val intPassed: Array[Element[Boolean]]=Array.fill(nrCapitole)(Constant(false))

		studied(0)=Flip(0.5)
		for{ i <- 1 until nrCapitole} { studied(i) = If(studied(i-1),Flip(0.6),Flip(0.3)) }
		for{ i <- 0 until nrCapitole} { booleanMark(i) = If(studied(i),Flip(0.8),Flip(0.2)) } 
		for{ i <- 0 until nrCapitole} { booleanPassed(i) = If(booleanMark(i),Flip(1.0),Flip(0.0)) } 
	
		for{ i <- 0 until nrCapitole} {
					val lt5 = 1 + r.nextInt(4)
					val gte5 = 5 + r.nextInt(6)
					intMark(i) = If(studied(i),gte5,lt5) } 

		for{ i <- 0 until nrCapitole} { intPassed(i) = Chain(intMark(i), (nr: Int)=>
        													if (nr>=5) Constant(true)
        			 										else Constant(false) )
				} 
	
		booleanMark(1).observe(true)
		booleanMark(2).observe(true)
		booleanMark(3).observe(true)
		println("Probability to pass boolean test 10" + VariableElimination.probability(booleanMark(9),true))
		println("Probability to pass boolean test 10" + VariableElimination.probability(booleanPassed(9),true))


		intMark(1).addConstraint((n:Int)=>if(n>=5) 10; else 4)
		intMark(2).addConstraint((n:Int)=>if(n>=5) 10; else 4)
		intMark(3).addConstraint((n:Int)=>if(n>=5) 10; else 4)
		println("Probability to pass int test 10" + VariableElimination.probability(intPassed(9),true))

		}
}