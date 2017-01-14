import org.scalacheck.Gen
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.{Checkers, PropertyChecks}

/**
  * Created by ps-dev on 1/13/2017.
  */
class HangmanChecks extends FunSuite with PropertyChecks with Matchers {

  test("order of guesses does not matter") {
    forAll(Gen.alphaChar, Gen.alphaChar) {(a,b) => {
      val h = Hangman()
      val h1 = h.guess(a).guess(b)
      val h2 = h.guess(b).guess(a)
      h1.showWord should equal(h2.showWord)
    }}
  }

  test("any sequence of guesses results in a valid state") {
    forAll(Gen.alphaStr) {(guesses) => {
      val h_final = guesses.foldLeft(Hangman())((h,g) => h.guess(g))
      h_final.isDead || (h_final.showWord contains '_')
    }}
  }

  test("show word is always a subset of letters you have guessed") {
    forAll(Gen.alphaStr) {(guesses) => {
      val h_final = guesses.foldLeft(Hangman())((h,g) => h.guess(g))
      guesses.toLowerCase.toSet should contain allElementsOf h_final.showWord.filter(c => c != '_').toSet
    }}
  }

  test("a winning game has the whole word filled out") {
    forAll(Gen.alphaStr) {(a) => {
      val h_final = a.foldLeft(Hangman())((h,g) => h.guess(g))
      if (h_final.isWin) {
        println("win")
        h_final.showWord should equal(h_final.word)
      }
    }}
  }

  test("guessing all the letters correctly wins the game") {
    val h = Hangman()
    forAll(Gen.pick(h.word.length, h.word)) {(guesses) => {
      println(guesses)
      val h_final = guesses.foldLeft(Hangman())((h,g) => h.guess(g))
      h_final.isWin shouldBe true
    }}
  }
}
