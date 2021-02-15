package com.bootcamp.homework

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


object AlgebraicDataTypes {

  type ErrorMessage = String;

  sealed trait Suit

  object Suit {

    final case object Club extends Suit

    final case object Diamond extends Suit

    final case object Heart extends Suit

    final case object Spade extends Suit

    def apply(value: String): Either[ErrorMessage, Suit] = {
      value match {
        case "d" => Right(Diamond)
        case "s" => Right(Spade)
        case "c" => Right(Club)
        case "h" => Right(Heart)
        case _   => Left("Suit parsing error")
      }
    }
  }

  sealed trait Rank

  object Rank {

    final case object Two extends Rank

    final case object Three extends Rank

    final case object Four extends Rank

    final case object Five extends Rank

    final case object Six extends Rank

    final case object Seven extends Rank

    final case object Eight extends Rank

    final case object Nine extends Rank

    final case object Ten extends Rank

    final case object Jack extends Rank

    final case object Queen extends Rank

    final case object King extends Rank

    final case object Ace extends Rank

    def apply(value: String): Either[ErrorMessage, Rank] = {
      value match {
        case "2" => Right(Two)
        case "3" => Right(Three)
        case "4" => Right(Four)
        case "5" => Right(Five)
        case "6" => Right(Six)
        case "7" => Right(Seven)
        case "8" => Right(Eight)
        case "9" => Right(Nine)
        case "T" => Right(Ten)
        case "J" => Right(Jack)
        case "Q" => Right(Queen)
        case "K" => Right(King)
        case "A" => Right(Ace)
        case _   => Left("Rank parsing error")
      }
    }
  }

  final case class Card(rank: Rank, suit: Suit)

  sealed trait Hand

  object Hand {

    final case class Omaha(cards: List[Card]) extends Hand {
      def apply(cards: List[Card]): Either[ErrorMessage, Omaha] = {
        if (cards.length == 4) Right(Omaha(cards))
        else Left("Cards amount error")
      }
    }

    final case class Texas(cards: List[Card]) extends Hand {
      def apply(cards: List[Card]): Either[ErrorMessage, Texas] = {
        if (cards.length == 2) Right(Texas(cards))
        else Left("Cards amount error")
      }
    }

  }

  final case class Board(cards: List[Card]) {
    def apply(cards: List[Card]): Either[ErrorMessage, Board] = {
      if (cards.length == 5) Right(Board(cards))
      else Left("Cards amount error")
    }
  }

  sealed trait Combination

  object Combination {

    final object RoyalFlush extends Combination

    final object StraightFlush extends Combination

    final object FourOfAKind extends Combination

    final object FullHouse extends Combination

    final object Flush extends Combination

    final object Straight extends Combination

    final object ThreeOfAKind extends Combination

    final object TwoPair extends Combination

    final object Pair extends Combination

    final object HighCard extends Combination

  }

  final case class TestCase(board: Board, hands: List[Hand])

  final case class TestResult(testCase: TestCase, sortedHands: List[Hand])

}
