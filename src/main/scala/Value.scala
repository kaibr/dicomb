
sealed abstract class Value {
    def pretty: String = this match {
        case Integer(i) => i.toString
        case Pair(p, q) => "(" + p.pretty + "," + q.pretty + ")"
        case Closure(v, p1, p2) => "{" + v.pretty + "," + p1.pretty + "," + p2.pretty + "}"
    }
}

case class Integer(i: Int) extends Value {
}

case class Pair(left: Value, right: Value) extends Value {
}

// Closure corresponds to a term v.i.(p1 -> p2)
case class Closure(v: Value, p1: Prog, p2: Prog) extends Value {
}

