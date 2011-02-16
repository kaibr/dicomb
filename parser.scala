package dicomb

import scala.util.parsing.combinator._

object parser extends JavaTokenParsers {
	
	def atomicprog = ( 	"id" ^^ {_ => Id()} 
								|  	"c" ^^ {_ => Rule("c")}
								|  	"w1" ^^ {_ => Rule("w1")}
								|  	"w2" ^^ {_ => Rule("w2")}
								|  	"i" ^^ {_ => Rule("i")}
								|  "e" ^^ {_ => Rule("e")}
								) 
	
	// def prog : Parser[Prog]= (	atomicprog ~ rep("." ~ prog) ^^ {(p: Prog, q: Prog) => Seq(p,q)}					)

  def main(args: Array[String]): Unit = {

	    val input = "idx"
		println("Input: "+input+"\nResult: ")
		println(parseAll(atomicprog,input))
	}
}