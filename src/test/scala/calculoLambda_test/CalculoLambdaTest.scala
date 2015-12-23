package calculoLambda_test

import org.scalatest._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import calculoLambda.CalculoLambda._

class CalculoLambdaTest extends FlatSpec with Matchers with LambdaParser {

  "sarlomps con parser variable" should "ser parseada a Var con id sarlomps" in {

    parseAll(variable, "sarlomps").get shouldBe Var("sarlomps")

  }

  "variable without name" should "not be parsed" in {

    parseAll(variable, "").successful shouldBe false

  }

  """application (x sar)""" should "be parsed to App x sar" in {

    parseAll(aplicacion, "x sar").get shouldBe App(Var("x"), Var("sar"))

  }
  

  
   """app*3 ((x y) z) sar""" should "be parsed to App x sar" in {

    parseAll(expresion, "((x y) z) sar").get shouldBe App(App(App(Var("x"),
                                                               Var("y")), 
                                                           Var("z")),
                                                       Var("sar"))

  }

  """lambda (\x.y)""" should "be parsed to a lambda with parameter x and body x" in {

    parseAll(lambda, """\x.y""").get shouldBe Lambda(Var("x"), Var("y"))

  }

  """lambda (\x.x sar)""" should "be parsed to a lambda with one parameter and App x sar in the body" in {

    parseAll(lambda, """\x.x sar""").get shouldBe Lambda(Var("x"), App(Var("x"), Var("sar")))

  }

  """lambda (\x.\y.\z."sar")""" should "be parsed to 3 lambdas with parameters x,y and z and the last body sar" in {

    parseAll(lambda, """\x.\y.\z.sar""").get shouldBe Lambda(Var("x"), Lambda(Var("y"), Lambda(Var("z"), Var("sar"))))

  }

  "Illegal variable" should "not be parsed" in {

    parseAll(variable, "sarlomps2").successful shouldBe false

  }

  "An expresion" should "be parsed correctly" in {

    parseAll(expresion, """((\y.x) b) (\x.y x)""").get shouldBe App(App(Lambda(Var("y"), Var("x")),
                                                                        Var("b")),
                                                                    Lambda(Var("x"),App(Var("y"), Var("x"))))

  }
  
   "Invalid expresion" should "not be parsed" in {

    parseAll(expresion, """((\y.x) b) ((\x.y x) invalidVar*-2)""").successful shouldBe false

  }
}