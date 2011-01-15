
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


sealed abstract class Type {}

case class Tyvar(name: String) extends Type {
  override def toString = name
}

case class Tycon(name: String, param:List[Type]) extends Type {
  override def toString = name + param.mkString("[",",","]")
}

case class TyConj(p: Type, q: Type) extends Type {
  override def toString = "("+p+"^"+q+")"
}


case class TyImp(p: Type, q: Type) extends Type {
  override def toString = "("+p+"->"+q+")"
}


case class FullType(in: Type, out: Type)  {
  override def toString = in + " ---> " + out
}



