
package dicomb;

sealed abstract class Prog {
	
	def prettyprint : String =  this match {
		case  Rule(name: String) => name
		case Id() => "id"
		case Seq(p: Prog, q: Prog) => p.prettyprint +"."+ q.prettyprint
		case Conj(p: Prog, q: Prog) =>  "("+p.prettyprint+"^"+q.prettyprint+")"
		case Imp(p: Prog, q: Prog) =>  "("+p.prettyprint+"->"+q.prettyprint+")"
	} 
}

case class Rule(name: String) extends Prog {}
case class Id() extends Prog {}
case class Seq(p: Prog, q: Prog) extends Prog {}
case class Conj(p: Prog, q: Prog) extends Prog {}
case class Imp(p: Prog, q: Prog) extends Prog {}





