package Lab6

import com.cra.figaro.library.atomic.discrete.{Binomial}
import com.cra.figaro.language.Chain
import com.cra.figaro.library.compound.{RichCPD, OneOf, *,If}
import com.cra.figaro.language.{Flip, Constant, Apply,Element}
import com.cra.figaro.algorithm.sampling.Importance

object tennis{
 
  	def tennis(
	 	probP1ServeWin: Double, probP1Winner: Double, probP1Error: Double,
	 	probP2ServeWin: Double, probP2Winner: Double, probP2Error: Double):
		Element[Boolean] = {

		//Ex6
	 	def rally(firstShot: Boolean, player1: Boolean): Element[Boolean] = { //primeste ca parametru 2 variabile pentru a sti daca este prima lovitura (se serveste) si daca este player1
	 		val pWinner =
	 			if (firstShot && player1) probP1ServeWin //daca se serveste si e P1, probabilitatea este cea ca P1 sa castige din servire
	 			else if (firstShot && !player1) probP2ServeWin //daca se serveste si e P2, probabilitatea este cea ca P2 sa castige din servire
	 			else if (player1) probP1Winner //daca loveste P1, probabilitatea este cea ca P1 sa castige cand nu serveste
	 			else probP2Winner //daca loveste P2, probabilitatea este cea ca P2 sa castige cand nu serveste
	 	
	 			val pError = if (player1) probP1Error else probP2Error //probabilitatea de eroare este cea a jucatorului care loveste acum
				val winner = Flip(pWinner) //face flip peste probabilitatile de pWinner
	 			val error = Flip(pError) //face flip peste probabilitatile de pError
	 	
	 			If(winner, Constant(player1), //daca a castigat P1, returneaza true pentru el si false pentru P2
	 				If(error, Constant(!player1), //daca a dat eroare, returneaza false pentru el si true pentru P2
	 					rally(false, !player1))) //se apeleaza pentru servirea oponentului
	 	}
		//Ex5

		
		def game(p1Serves: Boolean, p1Points: Element[Int], p2Points: Element[Int]): //primeste ca parametru daca P1 serveste si punctele pt fiecare player
		
		
		Element[Boolean] = {
			val p1WinsPoint = rally(true, p1Serves)
			val newP1Points =
				Apply(p1WinsPoint, p1Points, (wins: Boolean, points: Int) => if (wins) points + 1 else points) //daca a castigat P1 i se adauga un punct, daca a pierdut ramane aceeasi valoare
		
			val newP2Points =
				Apply(p1WinsPoint, p2Points, (wins: Boolean, points: Int) =>
					if (wins) points else points + 1) //daca nu a castigat P1, se adauga un punct la P2

			val p1WinsGame =
				Apply(newP1Points, newP2Points, (p1: Int, p2: Int) =>
					p1 >= 4 && p1 - p2 >= 2)//daca P1 are cel putin 4 puncte si e cu cel putin 2 puncte inaintea oponentului, P1 a castigat un joc

			val p2WinsGame =
				Apply(newP2Points, newP1Points, (p2: Int, p1: Int) =>
					p2 >= 4 && p2 - p1 >= 2)//daca P2 are cel putin 4 puncte si e cu cel putin 2 puncte inaintea oponentului, P2 a castigat un joc

			val gameOver = p1WinsGame || p2WinsGame//daca oricare din cei 2 jucatori a castigat un joc, s-a terminat un joc
		
			If(gameOver, p1WinsGame, game(p1Serves, newP1Points, newP2Points))//daca s-a terminat un joc, returneaza ca P1 a castigat, altfel repeta metoda
		}


		//Ex4
		def play(p1Serves: Boolean, p1Sets: Element[Int], p2Sets: Element[Int], p1Games: Element[Int], p2Games: Element[Int]):
		
		
		Element[Boolean] = {
			val p1WinsGame = game(p1Serves, Constant(0), Constant(0)) //castiguri pentru P1
			val newP1Games =  
				Apply(p1WinsGame, p1Games, p2Games, (wins: Boolean, p1: Int, p2: Int) =>
					if (wins) {
					if (p1 >= 5) 0 else p1 + 1//daca a castigat P1 si are cel putin 5 jocuri castigate, numarul de jocuri castigate de el devine 0, altfel creste cu 1
				} else 
					{ if (p2 >= 5) 0 else p1}) //altfel daca P2 are cel putin 5 jocuri castigate,numarul de jocuri castigate de P1 devine 0

			val newP2Games = 
				Apply(p1WinsGame, p1Games, p2Games, (wins: Boolean, p1: Int, p2: Int) =>
					if (wins) {
					if (p1 >= 5) 0 else p2//daca a castigat P1 si are cel putin 5 jocuri castigate, numarul de jocuri castigate de P2 devine 0
				} else 
					{ if (p2 >= 5) 0 else p2 + 1})//altfel daca P2 are cel putin 5 jocuri castigate, numarul de jocuri castigate de P2 devine 0, altfel creste cu 1

			val newP1Sets =
				Apply(p1WinsGame, p1Games, p1Sets, (wins: Boolean, games: Int, sets: Int) =>
					if (wins && games == 5) sets + 1 else sets) //daca a castigat P1 si are cel putin 5 jocuri castigate, numarul lui de castiguri creste cu 1

			val newP2Sets =
				Apply(p1WinsGame, p2Games, p2Sets, (wins: Boolean, games: Int, sets: Int) =>
					if (!wins && games == 5) sets + 1 else sets) //daca nu a castigat P1 si P2 are cel putin 5 jocuri castigate, numarul lui de castiguri creste cu 1

			val matchOver = //cand se termina jocul
				Apply(newP1Sets, newP2Sets, (p1: Int, p2: Int) => p1 >= 2 || p2 >= 2)//daca oricare dintre jucatori are cel putin 2 seturi castigate meciul s-a terminat
				
				If(matchOver,
					Apply(newP1Sets, (sets: Int) => sets >= 2),//daca meciul s-a terminat returneaza daca P1 are cel putin 2 seturi
					play(!p1Serves, newP1Sets, newP2Sets, newP1Games, newP2Games))//daca nu s-a terminat meciul, continua
		}

		play(true, Constant(0), Constant(0), Constant(0), Constant(0))
	}

	def main(args: Array[String]) {
 		val tennisMatch = tennis(0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
 		val algorithm = Importance(1000, tennisMatch)

 		algorithm.start()
 		algorithm.stop()
 		val player1result = algorithm.probability(tennisMatch, true)
 		val player2result = algorithm.probability(tennisMatch, false)
		
		algorithm.kill()
		println("Probabilitatea ca P1 sa castige sunt: " + player1result)
		println("Probabilitatea ca P2 sa castige sunt: " + player2result)
 	}
}