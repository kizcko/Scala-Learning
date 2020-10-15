package Lab3

import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.sampling._


object Covid {

 


 def main(args: Array[String]) {
     val febra = Flip(0.06)
     val tuse = Flip(0.04)

     val Covid = CPD(febra, tuse,
        (false, false) -> Flip(0.02),
        (false, true) -> Flip(0.30),
        (true, false) -> Flip(0.40),
        (true, true) -> Flip(0.80)
    )

    val Izoleta = CPD(Covid,false -> Flip(0.3),true -> Flip(0.7))

    

    Izoleta.observe(true)

    val alg = Importance(100000,febra, tuse,Covid)
    alg.start()

    println("Probabilitate de febra: " + alg.probability(febra,true))
    println("Probabilitate de tuse: " + alg.probability(tuse,true))
    println("Probabilitate de asimptomatic: " + alg.probability(Covid,false))
    
    
    
    alg.kill
 }
} 