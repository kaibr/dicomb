
sealed abstract class Type {}

case class Tyvar(name: String) extends Type {
  override def toString: String = name
}

case class Tycon(name: String, param:List[Type]) extends Type {
  override def toString: String = name + param.mkString("[",",","]")
}

case class TyConj(p: Type, q: Type) extends Type {
  override def toString: String = "("+p+"^"+q+")"
}

case class TyImp(p: Type, q: Type) extends Type {
  override def toString: String = "("+p+"->"+q+")"
}
