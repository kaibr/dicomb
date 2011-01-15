package dicomb;

sealed abstract class Value {}

<<<<<<< HEAD
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
=======
case class Integer(i:Int) extends Value {}
case class Pair(left: Value, right: Value) extends Value {}
case class Closure(v:Value,p1:Prog,p2:Prog) extends Value {}
>>>>>>> d6b5b22be1a2a9d795035f9e879f928fa4b718ee

