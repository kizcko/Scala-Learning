package partial

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored._


object Ex1 {
         def main(args: Array[String]) {
             
             var variatia = Select(0.5 -> 20, 0.5 ->30)
             val nivelTemperatura = RichCPD(variatia,
                                OneOf(20) -> Constant('scazut),
                                OneOf(25) -> Constant('mediu),
                                OneOf(30) ->Constant('mare))

         
         }


}