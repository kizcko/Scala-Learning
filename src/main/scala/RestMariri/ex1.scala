package partial

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored._


object Ex1 {
    def main(args: Array[String]) {
    val ninsoare = Flip(0.6)  //probabilitatea sa ninga
    val zile = 7 
    val saptamana: Array[Element[Symbol]] = Array.fill(zile)(ninsoare) //zilele din saptamana cu probabilitatea de a ninge adaugata
    val zileInCareANins = 0 
    
    for { zile <- 1 until zile } //calculam numarul de zile in care a nins 
    {
        val zileInCareANins = 
            if (zile == True) zileInCareANins = zileInCareANins + 1
    }
    val calitatea = //aflam calitatea saptamanii
            if(zileInCareANins > 5  ) Uniform ("prea multa ninsoare") //daca numarul de zile este mai mare ca 5
            else if(zileInCareANins <= 2  ) Uniform ("prea putina ninsoare") //daca numarul de zile este cel mult 2
            else Uniform ("normala")    ////daca numarul de zile este normal

    
     println(VariableElimination.probability(calitatea, "normala"))
    }
}