package partial

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored._


object Ex1 {
        def main(args: Array[String]) {
            //1
            val alarmSet = Flip(0.9)  //probabilitatea sa fie setata alarma daca cu o rata de 10% uiti sa o setezi
            val trezit = Chain(alarmSet,(b: Boolean) => //in functie de alarmSet daca este setata sau nu determinam probabilitatea ca el sa se trezeasca
									if (b) Flip(0.9) //probabilitatea sa se trezeasca daca alarma este setata
									else Flip(0.1))  //probabilitatea sa se trezeasca daca alarma nu este setata
            val autobuz = Flip(0.8) //Probabilitatea sa ajunga autobuzul

            val serviciu = CPD(trezit, autobuz, //pentru fiecare caz returneaza true daca a ajuns la timp la serviciu cu probabilitatea din Flip
                                (false, false) -> Flip(0.9),
                                (false, true) -> Flip(0.2),
                                (true, false) -> Flip(0.3),
                                (true, true) -> Flip(0.1))


            //2 a)
            trezit.observe(false) //a adormit
            println(VariableElimination.probability(serviciu, true)) //sa ajunga la serviciu chiar daca a adormit  0.3399999999999999

            //2 b)
            trezit.unobserve()
            serviciu.observe(true) //ai ajuns la serviciu
            println(VariableElimination.probability(alarmSet,true)) //alarma nu a fost setata  0.8181818181818181

            //2 c)
            serviciu.unobserve()
            //care e probabilitatea sa te trezesti prea tarziu?
            //nu trebuie pus observe deoarece chiar daca a setat alarma este o probabilitate sa nu se trezeasca
            println(VariableElimination.probability(trezit,true)) //0.8200000000000001

            //3
            //Este o probabilitate mai mare sa se trezeasca daca alarma este setata. Se poate afla prin rularea mai multor instante in care aflam daca s-a trezit sau nu 

        }


}