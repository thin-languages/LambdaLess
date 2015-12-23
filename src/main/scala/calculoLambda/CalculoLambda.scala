package calculoLambda

import scala.util.parsing.combinator._

object CalculoLambda {
  trait Expr
  case class Lambda(par:Var, body: Expr) extends Expr
  case class Var(id:String) extends Expr
  case class App(par1:Expr, par2:Expr) extends Expr
  
  trait LambdaParser extends RegexParsers{
    
    lazy val variable = "[a-zA-Z_]+".r ^^ Var
    
    lazy val lambda = ("""\"""~>variable<~".")~(expression) ^^ { case (v~e) => Lambda(v,e)}
    
    lazy val application = applicable ~ applicable.+ ^^ {case (i~as) => (i/:as)(App)}
    
    lazy val applicable = lambda | variable | "("~>expression<~")"
    
    lazy val expression:Parser[Expr] = applicable ~ applicable.* ^^ {case (i~as) => (i/:as)(App)}
        
  }
  
}
