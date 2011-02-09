
package dicomb;

object typeInfer {

		private var n:Int = 0

	def freshTyvar():Type = {n=n+1; Tyvar("a"+n)}

	abstract class Subst extends Function1[Type,Type] {
		
		def lookup(x:Tyvar):Type
		
		def apply(t: Type): Type = t match {
			case Tyvar(a) => 
				val u = lookup(Tyvar(a)) ; 
				if (t == u) t else apply(u)  
				// this is non-standard 
				// allows us to avoid applying substitutions too much during type inference
			case TyConj(p,q) => TyConj(apply(p), apply(q))
			case TyImp(p,q) => TyImp(apply(p), apply(q))
			case Tycon(name,param) => Tycon(name, param map apply)
		}

		def extend(x:Tyvar, t: Type) = new Subst{
			def lookup(y:Tyvar):Type = 
				if (x==y) t else Subst.this.lookup(y)
			override def toString = 
				x +":="+ t + "," + Subst.this
		}
		
	}
	object emptySubst extends Subst {
		def lookup(t: Tyvar):Type = t
		override def toString = "<emptySubst>"
	}

	
	def tyvars(t: Type): List[Tyvar] = t match {
		case Tyvar(a) => List(Tyvar(a))
		case TyImp(p,q) => tyvars(p) union tyvars(q)
		case TyConj(p,q) => tyvars(p) union tyvars(q)
		case Tycon(_,params) => (List[Tyvar]() /: params) ((tvs, t) => tvs union tyvars(t))
	}
	
	// finds mgu of s(a) and s(b)
	def mgu(t: Type, u: Type, s: Subst): Subst = 
		(s(t),s(u)) match {
			case (Tyvar(a),Tyvar(b)) if (a == b) => s
			case (Tyvar(a),_) if !(tyvars(u) contains a) => s.extend(Tyvar(a),u)
			case (_,Tyvar(b)) if !(tyvars(t) contains b) => s.extend(Tyvar(b),u)
			case (TyImp(t1,t2),TyImp(u1,u2)) => mgu(t1,u1,mgu(t2,u2,s))
			case (TyConj(t1,t2),TyConj(u1,u2)) => mgu(t1,u1,mgu(t2,u2,s))
			case(Tycon(n1,p1),Tycon(n2,p2)) if (n1==n2) => 
				(s /: (p1 zip p2)) ( (x, y) => mgu(y._1,y._2,x) )
			case (_,_) => throw new TypeError("cannot unify " + s(t) + " with " + s(u))
	}

	case class TypeError(s: String) extends Exception(s) {}

	// yields a substitution extending subst which, 
	// when applied to types in and out, 
	// yields most general input and output types for term   
	
	var current: Prog = null
	
	def tp(term: Prog, in: Type, out: Type, subst: Subst) : Subst = 
	{
		current = term
		term match {
			case Id() => mgu(in,out,subst)
			case Rule("w1") => 
				val a = freshTyvar
				mgu(in,TyConj(out,a),subst)
			case Rule("w2") => 
				val a = freshTyvar
				mgu(in,TyConj(a,out),subst)
			case Rule("c") =>
				mgu(out,TyConj(in,in),subst)
			case Rule("i") =>
				val a = freshTyvar
				mgu(out,TyImp(a,TyConj(in,a)),subst)
			case Rule("e") =>
				val a = freshTyvar
				mgu(in,TyConj(TyImp(a,out),a),subst)
			case Seq(term1,term2) =>
				val a = freshTyvar
				val s = tp(term1,in,a,subst)
				tp(term2,a,out,s)
			case Conj(term1,term2) =>
				val a = freshTyvar				
				val b = freshTyvar
				val c = freshTyvar
				val d = freshTyvar
				val s1 = tp(term1,a,b,subst)
				val s2 = tp(term2,c,d,s1)
				mgu(in,TyConj(a,c),mgu(out,TyConj(b,d),s2))
			case Imp(term1,term2) =>
				val a = freshTyvar				
				val b = freshTyvar
				val c = freshTyvar
				val d = freshTyvar
				val s1 = tp(term1,a,b,subst)
				val s2 = tp(term2,c,d,s1)
				mgu(in,TyImp(b,c),mgu(out,TyImp(a,d),s2))
		}
	}
	
def typeof(term:Prog) : ProgType = { 
	val a = freshTyvar
	val b = freshTyvar  
	val s = tp(term,a,b,emptySubst) 
	ProgType(s(a),s(b)) 
}

def main(args: Array[String])
{
	
	val x = Seq(Id(),Rule("c"))

	val y = Seq( Rule("c"), Seq(Rule("w1"), Id()) )

	val z = Imp(Rule("w1"),Rule("w1"))

	val a = TyImp(TyConj(Tyvar("a"),Tyvar("b")),Tyvar("a"))

	val b = TyImp(TyConj(Tyvar("a"),Tyvar("b")),Tyvar("b"))

	println("Some Programs with Types:")
	println(x+" : "+typeInfer.typeof(x))
	println(y+" : "+typeInfer.typeof(y))
	println(z+" : "+typeInfer.typeof(z))
	println()
	print("Two Types and their mgu: ")
	println(a+" ,"+b+" : "+typeInfer.mgu(a,b,typeInfer.emptySubst))
    	
   val s0 = typeInfer.emptySubst.extend(Tyvar("a"), TyConj(Tyvar("b"),Tyvar("c")) )
   val s1 = typeInfer.emptySubst.extend(Tyvar("a"),Tyvar("b"))
   val s2 = s1.extend(Tyvar("b"),Tyvar("c"))
		println("The substitution "+s2+" applied to type variable a: "+s2(Tyvar("a")))


	// catch { case TypeError(msg) => println("\n cannot type: "+current+"\n reason:"+msg)}
	
	
}

}