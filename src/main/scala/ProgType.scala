
case class ProgType(in: Type, out: Type)  {
  override def toString: String = in + " ---> " + out
}
