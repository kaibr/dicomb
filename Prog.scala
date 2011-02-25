
package dicomb;

sealed abstract class Prog {
	
	def toStrucString : String = this match {
		case Rule(name) => name
		case Id() => "id"
		case Seq(p,q) => "("+p.toStrucString+"."+q.toStrucString+")"
		case Conj(p,q) => "("+p.toStrucString+"^"+q.toStrucString+")"
		case Imp(p,q) => "("+p.toStrucString+"->"+q.toStrucString+")"
	}
}


case class Rule(name: String) extends Prog {
  override def toString = name	
}
case class Id() extends Prog {
	override def toString = "id"
}
case class Seq(p: Prog, q: Prog) extends Prog {
	override def toString = p +"."+ q
}
case class Conj(p: Prog, q: Prog) extends Prog {
  override def toString = 	"("+p+"^"+q+")"
}
case class Imp(p: Prog, q: Prog) extends Prog {
	override def toString = 	"("+p+"->"+q+")"
}





