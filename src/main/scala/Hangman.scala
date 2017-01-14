/**
  * Created by ps-dev on 1/13/2017.
  */

import scala.io.Source.fromInputStream
import scala.io.StdIn.readChar
import scala.util.Random

case class Hangman(lives: Int, word: String, guesses: Set[Char]) {
  val word_letters = word.toSet
  val correct_guesses = guesses intersect word_letters
  val incorrect_guesses = guesses diff word_letters

  def guess(letter: Char) = Hangman(lives, word, guesses + letter.toLower)
  def isDead = incorrect_guesses.size >= lives
  def isWin = correct_guesses.size == word_letters.size
  def showWord = {
    word.map(c => if (guesses contains c) c else '_').mkString
  }
}

object Hangman {
  val numLives = 7
  val stream = getClass.getResourceAsStream("sowpods.txt")
  val dictionary = fromInputStream(stream).getLines.toIndexedSeq

  def apply(): Hangman = {
    dictionary.lift(Random.nextInt(dictionary.size)) match {
      case Some(w) => Hangman(lives = numLives, word = w.toLowerCase, guesses = Set())
      case None => throw new IllegalArgumentException("I cannot play with an empty dictionary")
    }
  }
}

object HangmanApp extends App {
  var game = Hangman()

  while (!game.isDead && !game.isWin) {
    print("Your guess: ")
    val guess = readChar()
    game = game.guess(guess)
    println(game.showWord)
  }

  if (game.isDead) println("You lose. The word was " + game.word)
  else println("You won")
}
