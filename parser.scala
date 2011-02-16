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
	
	def prog = (atomicprog ~ "." ~ atomicprog) ^^ {case p ~ "." ~ q => Seq(p,q)}					

  def main(args: Array[String]): Unit = {

	    val input = "id"
		println("Input: "+input+"\nResult: ")
		println(parseAll(atomicprog,input))
		val input2 = "id.c"
		println("Input: "+input2+"\nResult: ")
		println(parseAll(prog,input2))
		
	}
}