package dicomb

import scala.util.parsing.combinator._

object parser extends JavaTokenParsers {
	
	def atomicprog: Parser[Prog]  = ( 	
									"id" ^^ {_ => Id()} 
								|  	"c" ^^ {_ => Rule("c")}
								|  	"w1" ^^ {_ => Rule("w1")}
								|  	"w2" ^^ {_ => Rule("w2")}
								|  	"i" ^^ {_ => Rule("i")}
								|  "e" ^^ {_ => Rule("e")} )
								
								
	def conjprog: Parser[Prog]  = 	"("~>prog~"^"~prog<~")" ^^  {case p ~ "^" ~ q => Conj(p,q)}
	def impprog: Parser[Prog]  =  "("~>prog~"->"~prog<~")" ^^ {case p ~ "->" ~ q => Imp(p,q)}					
	
	def nonseqprog: Parser[Prog] = atomicprog | conjprog | impprog
	
	def prog: Parser[Prog]  = rep1sep(nonseqprog, ".") ^^ {_.reduceRight ((x:Prog,y:Prog) => Seq(x,y))}
	
	def value: Parser[Value] = (
		  wholeNumber ^^ (x => Integer(x.toInt))
		| "("~>value~","~value<~")" ^^ {case (x~","~y) => Pair(x,y)}
		| "{"~>value~","~prog~","~prog<~"}" ^^ {case (x~","~y~","~z) => Closure(x,y,z)}  )

  def main(args: Array[String]): Unit = {

	    val programs = List("id", "id.id.id", "c.w1.w2", "(w1 -> w2)", "(w1 ^ w2)","id.(id^id).(w1->w1.w2)")
		programs foreach {x => println("Input: "+x+"       "+(parseAll(prog,x).get.toStrucString))}
	    
	    val values = List("7","(7,9)","{(7,9),id,id}")
	    values foreach {x => println("Input: "+x+"       "+parseAll(value,x))}
		
	}
}