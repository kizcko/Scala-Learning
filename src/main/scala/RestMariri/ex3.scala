import com.cra.figaro.language.{Select, Element}
import com.cra.figaro.library.compound.{CPD, RichCPD, OneOf}
import com.cra.figaro.algorithm.factored.{VariableElimination, MPEVariableElimination}
import com.cra.figaro.library.atomic.discrete.Uniform

object Test2{
    //a
    abstract class State{ 
        val ascuns: Element[String] 
        def stare = CPD(ascuns, //in functie de starea ascunsa primeste variabila observata
                            "A" -> Constant("buna"), 
                            "B" -> Constant("nu prea buna"),
                            "C" -> Constant("bolnav"),
                            "D" -> Constant("decedat"))
    }
    //b
    class InitialState(n: String) extends State{ 
        val ascuns = Uniform(n)
    }
    //c
    class NextState(curent: State) extends State{
        val ascuns = CPD(curent, ascuns, //pentru fiecare stare ascunsa selecteaza o stare noua cu probabilitatile indicate
                         "A" -> Select(0.721 -> "A", 0.202 ->"B", 0.067->"C", 0.1 ->"D"),
                         "B" -> Select(0.581 ->"B", 0.407->"C", 0.012 ->"D"),
                         "C" -> Select(0.75->"C", 0.25 ->"D"),
                         "D" -> Select(1 ->"D"))
    }
    //d

    class stateSequence(n: Int): List(State) {   //o functie ce construieste o secventa de stari ale sistemului(fiecare stare reprezinta o stare ascunsa)
        if (n == 0)
            List(new InitialState("A")) //Primeste ca parametru prima stare A
        else{
            val last :: rest = stateSequence (n-1) //completeaza recursiv restul starilor
            new NextState(last) :: last :: rest
        }
    }


    def main(args: Array[String]){
        val states = 10 //numarul de stari ale sistemului
        val stateSeq = stateSequence(states) // se creaza secventa de stari
        
        //e
        //Trebuie sa interam prin fiecare zi si sa aplicam MPEVariableElimination


        //f
        stateSeq(6).stare.observe("nu prea buna")  //in starea a 6a nu era prea buna
        
        //println(VariableElimination.probability(stateSeq(10).ascuns))
        
        val algorithm = MPEVariableElimination()
        algorithm.start()
        println(algorithm.mostLikelyValue(stateSeq(10).ascuns)) //probabilitatea din starea 10 daca am pus observe pe starea 6
        algorithm.kill()


        //g
        stateSeq(7).stare.observe("bolnav")
        //Trebuie sa iteram prin fiecare stare acum sin stateSeq

        //nu am avut timp sa fac functia de cautare prin toate starile
    }

}