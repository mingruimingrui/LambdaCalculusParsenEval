
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

object Lab4 extends App with RegexParsers{

  //import scala.util.parsing.combinator._
  
  //type Tokens = StdLexical
  //val lexical = new StdLexical
  //lexical.reserved ++= List("let")
  //lexical.delimiters ++= List("\\",".","(",")","=","~>")
  

  abstract class Term //extends StdLexical
  case class Var(name: String) extends Term {
    override def toString = name
  }
  case class Fun(arg: String, body: Term) extends Term {
    override def toString = "\\" ++ arg ++ "." ++ body.toString
    def getArgs = Lab4.parseAll(arguments, arg).get
  }
  case class FApp(f: Term, v: Term) extends Term {
    override def toString = "("++f.toString ++ " " ++ v.toString++")"
  }
  case class Let(n: String,t1: Term, t2: Term) extends Term {
    override def toString = "(let "++n.toString ++ " = " ++ t1.toString++
    " in "++t2.toString ++")"
  }


  //class LambdaParsers extends RegexParsers {
    //def letTerm   : Parser[Term]      = let | simpleTerm
    def term      : Parser[Term]      = fApp | let | simpleTerm
    def simpleTerm: Parser[Term]      = fun | vari | "(" ~> term <~ ")"
    def fun       : Parser[Term]      = "\\" ~> arguments ~ "." ~ term ^^
                                        {case args ~ "." ~ term => Fun(args.mkString(" "), term) }
    def fApp      : Parser[Term]      = simpleTerm ~ rep1(simpleTerm) ^^
                                        {case l~r => (l /: r) { (l,rs) => FApp(l,rs) } }
    def arguments : Parser[List[Var]] = rep1(vari)
    def let       : Parser[Term]      = vari ~ "=" ~ simpleTerm ~ "in" ~ simpleTerm ^^
                                        {case v~"="~t1~"in"~t2 => Let(v.toString,t1,t2) }
    def vari      : Parser[Var]       = """[a-z]'*""".r ^^ { Var(_) }
  //}

  //unused :( I simply cannot complete
  def free_vars (t:Term) : List[String] =
    t match {
      case Var(n) => List(n)
      case Fun(arg,body) => {
        val vs = free_vars(body)
        //vs filterNot (x => (x==arg))
        vs filter (x => (Lab4.parseAll(arguments,arg).get) contains x )
      }
      case FApp(t1,t2) => free_vars(t1) ++ free_vars(t2)
      case Let(v,t1,t2) => {
         val vs = free_vars(t1)
         (vs filterNot (x => (x==v))) ++ free_vars(t2)
      }
    }
  
  
  
  def parse_lambda(x:String):Option[Term] = {
    val pattern = """let""".r
    val str = pattern.replaceAllIn(x, "")
    (Lab4.parseAll(Lab4.term,str)) match {
      case Success(a,_) => Some(a)
      case _            => None
    }
  }

  def print_lambda(x: String) = parse_lambda(x) match{
    case Some(a) => println(a.toString)
    case None    => println("Hmmm I don't think that this is a lambda term.")
  }
  
  
  
  //part B:
  

  def alphaConver(t: Term, x: Var, y: Term): Term ={
    //
    t match {
    case Var(_) => sub(t, x, y)
    case Fun(a,b) => {
      val aux = free_vars(Fun(a,b))
      val freeV ={if (aux.isEmpty) Nil
                  else Lab4.parseAll(Lab4.arguments,aux.mkString(" ")).get}
      if(freeV contains t) Fun(a,alphaConver(b,x,y))
      else Fun(a,b)
    }
    case FApp(a,b) => FApp(alphaConver(a,x,y),alphaConver(b,x,y))
  }
  }
  
  def betaReduc(t: Fun, y: Term): Term ={
    //
    t.getArgs match{
    case x::Nil => alphaConver(t.body,x,y)
    case x::xs  => {if (xs contains x) Fun(xs.mkString(" "),t.body)
                    else Fun(xs.mkString(" "),alphaConver(t.body,x,y)) }
    case _ => throw new Exception("Function arguments expected")
  }
  }
  
  def sub(t: Term, x: Term, y: Term): Term =
    t match{
    case Var(a) => {if(Var(a) == x) y
                    else Var(a)}
    case _ => t
  }

  def eval_to_value(x:Term):Term ={
    def aux(x:Term):Term =
    x match {
    case Var(_) => x
    case Fun(_,_) => x
    case FApp(Var(a),b) => FApp( Var(a) , aux(b) )
    case FApp(FApp(a,b),c) => aux( FApp( aux(FApp(a,b)) , aux(c) ) )
    case FApp(Fun(a,b),c) => betaReduc( Fun(a,b) , aux(c) )
    case Let(a,t1,t2) =>{
      val t = FApp(Fun(a,t2),t1)
      eval_to_value(t)
    }
    }
    aux(x)
  }
  
  //evaluate and print
  def print_eval(x:String)= parse_lambda(x) match{
    case Some(a) => println(eval_to_value(a))
    case None    => println("Hmmm I don't think that this is a lambda term.")
  }

}


