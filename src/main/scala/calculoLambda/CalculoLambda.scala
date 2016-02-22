package calculoLambda

import scala.util.parsing.combinator._

object CalculoLambda {
  trait Expr
  {
    def replace(toReplace:Expr, replacement:Expr):Expr
    def reduce:Expr
    def apply(param:Expr):Expr
  }
  case class Lambda(par:Var, body: Expr) extends Expr
  {
    def reduce = Lambda(par.reduce,body.reduce)
    def apply(param:Expr) = body.replace(par,param)
    def replace(toReplace:Expr,replacement:Expr) = body.replace(toReplace,replacement)  
  }
  case class Var(id:String) extends Expr
  {
    def reduce = this
    def apply(param:Expr) = App(this,param)
    def replace(toReplace:Expr,replacement:Expr) = if (toReplace == this) replacement else this
  }
  case class App(par1:Expr, par2:Expr) extends Expr
  {
    def reduce = par1.reduce.apply(par2.reduce)
    def apply(param:Expr) = this
    def replace(toReplace:Expr,replacement:Expr) =
      App(par1.replace(toReplace,replacement),par2.replace(toReplace,replacement))
  }
  
  trait LambdaParser extends RegexParsers{
    
    lazy val variable = "[a-zA-Z_]+".r ^^ Var
    
    lazy val lambda = ("""\"""~>variable<~".")~(expression) ^^ { case (v~e) => Lambda(v,e)}
    
    lazy val application = applicable ~ applicable.+ ^^ {case (i~as) => (i/:as)(App)}
    
    lazy val applicable = lambda | variable | "("~>expression<~")"
    
    lazy val expression:Parser[Expr] = application | applicable
        
  }
  
}
