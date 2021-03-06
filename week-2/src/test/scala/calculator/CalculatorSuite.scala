package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }

  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  /******************
    ** POLYNOMIAL **
    ******************/

  test("Polynomial tests") {
    val a = Var(1.0)
    val b = Var(6.0)
    val c = Var(9.0)
    val delta = Polynomial.computeDelta(a, b, c)
    val solutions = Polynomial.computeSolutions(a, b, c, delta)

    assert(delta() === 0.0)
    assert(solutions() === Set(-3.0))

    a.update(2)
    assert(delta() === -36.0)
    assert(solutions() === Set())

    b.update(9)
    assert(delta() === 9.0)
    assert(solutions() === Set(-3.0, -1.5))

    c.update(-5)
    assert(delta() === 121.0)
    assert(solutions() === Set(-5.0, 0.5))
  }

  /******************
    ** CALCULATOR **
    ******************/

  val variables = Map[String, Signal[Expr]](
    "a" -> Signal(Literal(1)),
    "b" -> Signal(Plus(Literal(1), Literal(2))),
    "c" -> Signal(Minus(Literal(1), Literal(2))),
    "d" -> Signal(Times(Literal(1), Literal(2))),
    "e" -> Signal(Divide(Literal(1), Literal(2))),
    "f" -> Signal(Plus(Plus(Literal(1), Literal(2)), Literal(3))),
    "g" -> Signal(Plus(Ref("a"), Ref("b"))),
    "h" -> Signal(Ref("i")),
    "i" -> Signal(Ref("h")),
    "j" -> Signal(Ref("j")),
    "k" -> Signal(Ref("z"))
  )

  val values = Calculator.computeValues(variables)

  test("Calculator literals expressions test") {
    assert(values("a")() === 1.0)
    assert(values("b")() === 3.0)
    assert(values("c")() === -1.0)
    assert(values("d")() === 2.0)
    assert(values("e")() === 0.5)
    assert(values("f")() === 6.0)
  }

  test("Calculator valid reference expressions test") {
    assert(values("g")() === 4.0)
  }

  test("Calculator invalid reference expressions test") {
    println()
    assert(values("h")().isNaN)
    assert(values("i")().isNaN)
    assert(values("j")().isNaN)
  }

  test("Calculator unknown reference test") {
    assert(values("k")().isNaN)
  }
}
