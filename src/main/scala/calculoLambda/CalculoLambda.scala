package calculoLambda

import scala.util.parsing.combinator._

object CalculoLambda { 

  object BetaReduction{
     trait EagerEvaluation{
      implicit class EagerEval(expr:Expr){
        def reduce:Expr = expr match{
           case Var(id) => Var(id)
           case Lambda(param,body) => Lambda(param,body.reduce)
           case App(a:Var,b) => App(a,b.reduce)
           case App(a:App,b) => App(a.reduce,b.reduce).reduce
           case App(a:Lambda,b) => a.apply(b.reduce).reduce
        }
      }
    }
    trait LazyEvaluation{
      implicit class LazyEval(expr:Expr){
        def reduce:Expr = expr match{
          case Var(id) => Var(id)
          case Lambda(param,body) => Lambda(param,body.reduce)
          case App(a:Var,b) => App(a,b.reduce)
          case App(a:App,b) => App(a.reduce,b).reduce
          case App(a:Lambda,b) => a.apply(b).reduce
        }
      }
    }
  }
  trait Expr extends BetaReduction.LazyEvaluation
  {
    def replace(toReplace:Expr, replacement:Expr):Expr
    def apply(param:Expr):Expr = App(this, param)
  }
  case class Lambda(par:Var, body: Expr) extends Expr
  {
    override def apply(param:Expr) = body.replace(par,param)
    def replace(toReplace:Expr,replacement:Expr) = body.replace(toReplace,replacement)  
  }
  case class Var(id:String) extends Expr
  {
    def replace(toReplace:Expr,replacement:Expr) = if (toReplace == this) replacement else this
  }
  case class App(par1:Expr, par2:Expr) extends Expr
  {
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
