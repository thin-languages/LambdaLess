package calculoLambda_test

import org.scalatest._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import calculoLambda.CalculoLambda._

class CalculoLambdaTest extends FlatSpec with Matchers with LambdaParser{
    
  "sarlomps con parser variable" should "ser parseada a Var con id sarlomps" in {
    
    parseAll(variable, "sarlomps").get shouldBe Var("sarlomps")
    
  }
  
  """(\x.y)""" should "be parsed to a lambda with parameter x and body x" in {
    
    parseAll(lambda, """\x.y""").get shouldBe Lambda(Var("x"),Var("y"))
    
  }
  
  "Illegal variable" should "not be parsed" in {
    
    parseAll(variable, "sarlomps2").successful shouldBe false 
    
  }
  
    "An expresion" should "be parsed correctly" in {
    
    parseAll(expresion, """(\y.x) b (\x.y x)""").get shouldBe App(App(Lambda(Var("x"),App(Var("x"),Var("y"))),
                                                                Var("b")),Lambda(Var("y"),Var("x")))
    
  }

  
}