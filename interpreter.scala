
package dicomb;


object interpreter {

type State = (Value,Prog)

case class ExecException(s: String) extends Exception(s) {}

def execute(s:State):Value = s match
{
	case (v,  Seq(p1,p2))                                   
	=> execute(execute(v,p1),p2)
	
	case (Pair(v1,v2),  Conj(p1,p2))
	=> Pair(execute(v1,p1),execute(v2,p2))
	
	case (Closure(v,p3,p4),  Imp(p1,p2))
	=> Closure(v,Seq(p1,p3),Seq(p4,p2))
	
	case (v,  Rule("c"))
	=> Pair(v,v)
	
	case (Pair(v1,v2),  Rule("w1"))
	=> v1
	
	case (Pair(v1,v2),  Rule("w2"))
	=> v2
	
	case (v,  Rule("i"))
	=> Closure(v,Id(),Id())
	
	case (Pair(Closure(v1,p1,p2),v2),  Rule("e"))	
	=> execute(Pair(v1,execute(v2,p1)),p2)

	case _ => throw new ExecException("cannot execute " + s)
	
}
	
  def main(args: Array[String]): Unit = {
	
	val myprog = Conj(Rule("w1"),Rule("c"))
	val myval = Pair(Pair(Integer(4),Integer(5)),Integer(6))
	val mystate = (myval,myprog)
	println("An example state: "+mystate)
	val newval = execute(mystate)
	println("After execution: "+newval)
}

}