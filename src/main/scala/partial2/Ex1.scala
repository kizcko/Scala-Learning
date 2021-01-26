package partial2
import com.cra.figaro.language.{Select, Apply, Constant, Element, Chain, Universe}
import com.cra.figaro.library.compound.{If, CPD, RichCPD, OneOf, *, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.algorithm.filtering.ParticleFilter

object Test2{
    //val ora = 0
    //var starea: Array[Element[Double]] = Array.fill(ora)(Constant(0.0))
    //var decizia: Array[Element[Double]] = Array.fill(ora)(Constant(0.0))


    val initial = Universe.createNew() //facem un nou Universe
    Constant("start")("stare", initial) //initializam cu start
    

    def transition(stare: String, obs:String): (Element[(String, String)]) = {  //Starea de tranzitie pentru fiecare stare
      val stare = Constant(stare)
      val obs = Constant(obs)
      
      val newStare = CPD(stare
                         "start" -> Select(0.5 -> "insorit", 0.3 ->"inorat", 0.2->"ploios"), //daca este starea de start
                         "insorit" -> Select(0.6 -> "insorit", 0.3 ->"inorat", 0.1->"ploios"), //daca este starea de insorit
                         "inorat" -> Select(0.5 -> "inorat", 0.15 ->"insorit", 0.35->"ploios"), //daca este starea de inorat
                         "ploios" -> Select(0.45 -> "ploios", 0.15 ->"insorit", 0.4->"Inorat")) //daca este starea de ploios

      val newObs = Chain(newStare, (newStare: String) =>
        if (newStare == "insorit") Select(0.15 -> "iau_umbrela", 0.85 -> "nu_iau_umbrela");  //in functie de fiecare stare selectam observatia
        else if (newStare == "inorat") Select(0.65 -> "iau_umbrela", 0.35 -> "nu_iau_umbrela");
        else Select(0.75 -> "iau_umbrela", 0.25 -> "nu_iau_umbrela"))

      ^^(newStare, newObs) //punem in Element
      
    }


    def nextUniverse(previous: Universe): Universe = {  //facem iterativ pentru urmatoarea stare
      val next = Universe.createNew()
      val previousStare = previous.get[String]("stare")

      val newState = Chain(previousStare, transition _)
      Apply(newState, (s: (String, String)) => s._1)("stare", next)
      Apply(newState, (s: (String, String)) => s._2)("obs", next)
      next
    }
    
    val alg = ParticleFilter(initial, nextUniverse, 10000) //pentru urmatoarele 5 ore monitorizam
    alg.start()
    for {time <- 1 to 5} {
      alg.advanceTime()
      println("Time " + time + ": ")
      println("expected stare = " + alg.currentExpectation("stare", (p: String) => p))
      println("expected observation = " + alg.currentExpectation("observation", (p: String) => p))
    }
}