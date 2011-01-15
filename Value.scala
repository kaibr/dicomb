package dicomb;

sealed abstract class Value {}

case class Integer(i:Int) extends Value {}
case class Pair(left: Value, right: Value) extends Value {}
case class Closure(v:Value,p1:Prog,p2:Prog) extends Value {}

