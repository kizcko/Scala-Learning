package Lab12
import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.experimental.normalproposals.Normal
import scala.collection._

object Ex1 extends App with scalax.chart.module.Charting {
   
        val x0 = Apply(Normal(0.75, 0.3), (d: Double) => d.max(0).min(1))
        val y0 = Apply(Normal(0.4, 0.2), (d: Double) => d.max(0).min(1))
        val x = Flip(x0)
        val y = Flip(y0)
        val z = If(x === y, Flip(0.8), Flip(0.2))

        z.observe(false)

        val veAnswer = VariableElimination.probability(y,true)
        println(veAnswer)

val data = for { i <- 1000 to 10000 by 1000 } yield {    
var totalSquaredError = 0.0
    for { j <- 1 to 100 } {      
        val imp = Importance(i, y)      
        imp.start()      
        val impAnswer = imp.probability(y, true)      
        val diff = veAnswer - impAnswer      
        totalSquaredError = totalSquaredError + diff * diff    
        }    
    val rmse = math.sqrt(totalSquaredError / 100)
    println(i + " samples:rmse = " + rmse)
    (i,rmse)      
    }
    val chart = XYLineChart(data)
    chart.show()
    chart.saveAsPNG("D:/tmp/chart1.png")



val data2 =   for { i <- 10000 to 100000 by 10000 } yield {    
    var totalSquaredError = 0.0    
    for { j <- 1 to 100 } {      
        Universe.createNew()      
        val x = Flip(0.8)      
        val y = Flip(0.6)      
        val z = If(x === y, Flip(0.9), Flip(0.1))      
        z.observe(false)      
        val mh = MetropolisHastings(i, ProposalScheme.default, y)
        mh.start()      
        val mhAnswer = mh.probability(y, true)      
        val diff = veAnswer - mhAnswer      
        totalSquaredError = totalSquaredError + diff * diff    
        }    
    val rmse = math.sqrt(totalSquaredError / 100)    
    println(i + " samples:rmse = " + rmse)
    (i,rmse)
    }
    val chart2 = XYLineChart(data2)
    chart2.show()
    chart2.saveAsPNG("/Users/macbook/Desktop/chart2.png")



 val x1 = Flip(0.999)
 val y1 = Flip(0.99)
 val z1 = If(x1 === y1, Flip(0.9999), Flip(0.001))

z1.observe(false)

 val veAnswer1 = VariableElimination.probability(y1,true)
 println(veAnswer1)

   
 val totalSquaredError1 = 0.0
        val imp = Importance(1000000, y1)      
        imp.start()      
        val impAnswer = imp.probability(y1, true)      
        val diff = veAnswer - impAnswer      
        val totalSquaredError4 = totalSquaredError1 + diff * diff    
            
    val rmse1 = math.sqrt(totalSquaredError4 / 100)
    println(1000000 + " samples:rmse= " + rmse1)
    

  
     val totalSquaredError3 = 0.0    
        Universe.createNew()      
        val x2 = Flip(0.999)      
        val y2 = Flip(0.99)      
        val z2 = If(x2 === y2, Flip(0.9999), Flip(0.0001))      
        z2.observe(false)      
        val mh = MetropolisHastings(10000000, ProposalScheme.default, y)
        mh.start()      
        val mhAnswer = mh.probability(y, true)      
        val diff2 = veAnswer1 - mhAnswer      
        val totalSquaredError2 = totalSquaredError3 + diff2 * diff2    
            
     val rmse2 = math.sqrt(totalSquaredError2 / 100)    
    println(1000000 + " samples:rmse= " + rmse2)
    
}
