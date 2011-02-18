package dicomb

import scala.util.parsing.combinator._

object parser extends JavaTokenParsers {
	
	def atomicprog: Parser[Prog]  = ( 	
									"id" ^^ {_ => Id()} 
								|  	"c" ^^ {_ => Rule("c")}
								|  	"w1" ^^ {_ => Rule("w1")}
								|  	"w2" ^^ {_ => Rule("w2")}
								|  	"i" ^^ {_ => Rule("i")}
								|  "e" ^^ {_ => Rule("e")}
								) 
								
	def conjprog: Parser[Prog]  = 	"("~>prog~"^"~prog<~")" ^^  {case p ~ "^" ~ q => Conj(p,q)}
	def impprog: Parser[Prog]  =  "("~>prog~"->"~prog<~")" ^^ {case p ~ "->" ~ q => Imp(p,q)}					
	
	def nonseqprog: Parser[Prog] = atomicprog | conjprog | impprog
	
	def prog: Parser[Prog]  = rep1sep(nonseqprog, ".") ^^ {_.reduceRight ((x:Prog,y:Prog) => Seq(x,y))}
	

  def main(args: Array[String]): Unit = {

	    val inputs = List("id", "id.id.id", "c.w1.w2", "(w1 -> w2)", "(w1 ^ w2)","id.(id^id).(w1->w1.w2)")
		inputs foreach {x => println("Input: "+x+"       "+parseAll(prog,x))}
		
	}
}