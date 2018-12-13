
object interpreter {

    type State = (Value, Prog)

    case class ExecException(s: String) extends Exception(s) {}

    def execute(s: State): Value = s match {

        case (v, Id())
        => v

        case (v, Seq(p1, p2))
        => execute(execute(v, p1), p2)

        case (Pair(v1, v2), Conj(p1, p2))
        => Pair(execute(v1, p1), execute(v2, p2))

        case (Closure(v, p3, p4), Imp(p1, p2))
        => Closure(v, Seq(p1, p3), Seq(p4, p2))

        case (v, Rule("c"))
        => Pair(v, v)

        case (Pair(v1, v2), Rule("w1"))
        => v1

        case (Pair(v1, v2), Rule("w2"))
        => v2

        case (v, Rule("i"))
        => Closure(v, Id(), Id())

        case (Pair(Closure(v1, p1, p2), v2), Rule("e"))
        => execute(Pair(v1, execute(v2, p1)), p2)

        case _ => throw ExecException("cannot execute " + s)

    }

    def main(args: Array[String]): Unit = {


        val states1 = List(
            (Pair(Pair(Integer(4), Integer(5)), Integer(6)), Conj(Rule("w1"), Rule("c")))
        )

        val states = List(
            ("((4,5),6)", "(w1^c)"),
            ("(1,2)", "c.(w2^w1)"), // commutativity
            ("((1,2),3)", "(w1 ^ id)"),
            ("((1,2),3)", "c . (w1.w1 ^ (w2 ^ id))") //associativity
        )

        def parse: (String, String) => (Value, Prog) = {
            case (v: String, p: String) => (
                parser.parseAll(parser.value, v).get,
                parser.parseAll(parser.prog, p).get
            )
        }

        println("Original State     /     State after Execution\n")
        states foreach { x => println(x + "   /   " + execute(parse.tupled(x)).pretty ) }


    }

}
