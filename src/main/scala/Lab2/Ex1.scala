package Lab2 

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._

//Abstract Class
abstract class Persoana
{ 
    var nume : String
    var prenume : String
    
    
}

class Student extends Persoana
{ 
    var an : Int
    var materii : Array[(String, Integer)]

    def setNota( materie: String, nota: Integer ) : Int = {
      for ((a, b) <- materii) {
        if (a==materie){
            
        }
    }
    }

    def getNota( materie: String ) : Int = {
      for ((a, b) <- materii) {
        if (a==materie){
            println(b)
        }
    }
    }

    def addMaterie(materie: String, nota: Integer){

    }

}

class Profesor extends Persoana
{
    var materie : String
}

object Ex1 {
	def main(args: Array[String]) {
		var materii = Array("PMP","SI","A3D","Python","ML","AI")
	}
}