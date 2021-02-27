package com.evolution.bootcamp.adt

import scala.collection.SortedSet

// Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
// task you completed to join the bootcamp. Use your best judgement about particular data types to include
// in the solution, you can model concepts like:
//
// 1. Suit
// 2. Rank
// 3. Card
// 4. Hand (Texas or Omaha)
// 5. Board
// 6. Poker Combination (High Card, Pair, etc.)
// 7. Test Case (Board & Hands to rank)
// 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
//
// Make sure the defined model protects against invalid data. Use value classes and smart constructors as
// appropriate. Place the solution under `adt` package in your homework repository.
object Poker {

  sealed trait Suit

  object Suit {

    final case object Spades extends Suit

    final case object Hearts extends Suit

    final case object Clubs extends Suit

    final case object Diamonds extends Suit

    def apply(value: String): Either[String, Suit] = {
      value match {
        case "s" => Right(Spades)
        case "h" => Right(Hearts)
        case "c" => Right(Clubs)
        case "d" => Right(Diamonds)
        case _ => Left(s"unknown suit: $value")
      }
    }
  }

  sealed abstract class Rank(value: Int)

  object Rank {

    final case object Two extends Rank(2)

    final case object Three extends Rank(3)

    final case object Four extends Rank(4)

    final case object Five extends Rank(5)

    final case object Six extends Rank(6)

    final case object Seven extends Rank(7)

    final case object Eight extends Rank(8)

    final case object Nine extends Rank(9)

    final case object Ten extends Rank(10)

    final case object Jack extends Rank(11)

    final case object Queen extends Rank(12)

    final case object King extends Rank(13)

    final case object Ace extends Rank(14)

  }

  final case class Card(suit: Suit, rank: Rank)

  sealed abstract class Hand(cards: Set[Card])

  object Hand {

    final case class OmahaHand private(cards: Set[Card]) extends Hand(cards)

    final case class TexasHand private(cards: Set[Card]) extends Hand(cards)

    def apply(cards: Set[Card]): Either[String, Hand] = cards.size match {
      case 2 => Right(TexasHand(cards))
      case 5 => Right(OmahaHand(cards))
      case _ => Left(s"unknown hand size")
    }
  }

  final case class Board(cards: Set[Card])

  sealed abstract class Combination(rank: Int)

  object Combination {

    final case object HighCard extends Combination(1)

    final case object Pair extends Combination(2)

    final case object TwoPairs extends Combination(3)

    final case object ThreeOfAKind extends Combination(4)

    final case object Straight extends Combination(5)

    final case object Flush extends Combination(6)

    final case object FullHouse extends Combination(7)

    final case object FourOfAKind extends Combination(8)

    final case object StraightFlush extends Combination(9)

    def apply(cards: Set[Card]): Either[String, Combination] = ???

  }

  final case class TestCase(board: Board, hands: Set[Hand])

  final case class TestResult(testCase: TestCase, hands: SortedSet[Hand])

}
