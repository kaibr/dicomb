
package dicomb;

sealed abstract class Prog {}

case class Rule(name: String) extends Prog {
  override def toString = name
}
case class Id() extends Prog {
  override def toString = "id"
}

case class Seq(p: Prog, q: Prog) extends Prog {
  override def toString = p+"."+q
}

case class Conj(p: Prog, q: Prog) extends Prog {
  override def toString = "("+p+"^"+q+")"
}

case class Imp(p: Prog, q: Prog) extends Prog {
  override def toString = "("+p+"->"+q+")"
}





