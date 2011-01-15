package dicomb;

sealed abstract class Value {}

case class Integer(i:Int) extends Value {
	override def toString = i.toString
}
case class Pair(left: Value, right: Value) extends Value {
	override def toString = "("+left+","+right+")"
}

// Closure corresponds to a term v.i.(p1 -> p2)
case class Closure(v:Value,p1:Prog,p2:Prog) extends Value {
	override def toString = "{"+v+","+p1+","+p2+"}"
}

