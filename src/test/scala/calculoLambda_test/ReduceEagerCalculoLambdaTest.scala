package calculoLambda_test

import org.scalatest._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import calculoLambda.CalculoLambda._
import calculoLambda.CalculoLambda.BetaReduction.EagerEvaluation
import scala.util.{Try}

class ReduceEagerLambdaTest extends FlatSpec with Matchers with LambdaParser with EagerEvaluation{

  val vA = Var("A")
  val vB = Var("B")
  val appAB = App(vA,vB)
  val lambdaIdentity = Lambda(vA,vA)
  val lambdaConstant:Var=>Lambda = v => if(v == vA) Lambda(vB,v) else Lambda(vA,v) 
  
  "A var" should "return itself" in {

    vA.reduce shouldBe vA

  }
  
  "An application of two vars" should "return itself" in {

    appAB.reduce shouldBe appAB

  }

  "A lambda of varA as parameter and varB as body" should "return itself" in {

    Lambda(vA,vB).reduce shouldBe Lambda(vA,vB)

  }

  "A lambda with a var and a reducible app" should "return a lambda with the reduced app" in {

    Lambda(vB,App(lambdaIdentity,vB)).reduce shouldBe Lambda(vB,vB)

  }
    
  "An application of a lambda and a var" should "return the result of the lambda" in {

    App(lambdaIdentity,vB).reduce shouldBe vB

  }
  
  "An app of a var and an app of a lambda and a var" should "return the app of the reduced app and the var" in {

    App(App(lambdaIdentity,vB),vA).reduce shouldBe App(vB,vA)

  }
  
  "Complex expression" should "return the same as in paper" in { //Aca llegue a mi limite para nombres expresivos en los tests

    App(App(Lambda(Var("X"),Lambda(Var("Y"),Var("X"))),Lambda(Var("Z"),Var("Z"))),Lambda(Var("A"),Var("A"))).reduce shouldBe Lambda(Var("A"),Var("A"))

  }
  
  "Expression that never ends" should "not end" in { 

    an [StackOverflowError] should be thrownBy App(Lambda(vA,App(vA,vA)),Lambda(vB,App(vB,vB))).reduce

  }

   "Expression that ends when lazy evaluated" should "not end" in { 

    an [StackOverflowError] should be thrownBy App(Lambda(vA,vB),App(Lambda(vA,App(vA,vA)),Lambda(vB,App(vB,vB)))).reduce

  }
}