package calculoLambda

import scala.util.parsing.combinator._

object CalculoLambda {
  trait Expr
  case class Lambda(par:Var, body: Expr) extends Expr
  case class Var(id:String) extends Expr
  case class App(par1:Expr, par2:Expr) extends Expr
  
  trait LambdaParser extends RegexParsers{
    
    lazy val variable = "[a-zA-Z_]+".r ^^ Var
    
    lazy val lambda = ("""\"""~>variable<~".")~(expresion) ^^ { case (v~e) => Lambda(v,e)}
    
    lazy val aplicacion = expresion~expresion ^^ { case (e1~e2) => App(e1,e2)}
    
    lazy val applicable = lambda | variable | "("~>expresion<~")"
    
    lazy val expresion:Parser[Expr] = applicable ~ expresion.* ^^ {case (i~as) => (as:\i)(App)}
    
  }
  
}