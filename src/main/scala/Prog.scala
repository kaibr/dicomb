
sealed abstract class Prog {

    def pretty: String = this match {
        case Rule(name) => name
        case Id() => "id"
        case Seq(p, q) => "" + p.pretty + "." + q.pretty + ""
        case Conj(p, q) => "(" + p.pretty + "^" + q.pretty + ")"
        case Imp(p, q) => "(" + p.pretty + "->" + q.pretty + ")"
    }
}


case class Rule(name: String) extends Prog {
    //  override def toString: String = name
}

case class Id() extends Prog {
    //	override def toString = "id"
}

case class Seq(p: Prog, q: Prog) extends Prog {
    //override def toString: String = p +"."+ q
}

case class Conj(p: Prog, q: Prog) extends Prog {
    //override def toString: String = 	"("+p+"^"+q+")"
}

case class Imp(p: Prog, q: Prog) extends Prog {
    //override def toString: String = 	"("+p+"->"+q+")"
}





