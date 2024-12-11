package apps
package app69

import scala.language.adhocExtensions
import org.scalacheck.*
import Gen.*
import Prop.*

import Card.*
import Suit.*

class TestTypes extends munit.FunSuite:
  
  test("CardSymbol is Correct"): 
    assertEquals(CardSymbols(Card(14,"♥️")), "🂱")
    assertEquals(CardSymbols(Card(2,"♥️")), "🂲")
    assertEquals(CardSymbols(Card(3,"♥️")), "🂳")
    assertEquals(CardSymbols(Card(4,"♥️")), "🂴")
    assertEquals(CardSymbols(Card(5,"♥️")), "🂵")
    assertEquals(CardSymbols(Card(6,"♥️")), "🂶")
    assertEquals(CardSymbols(Card(7,"♥️")), "🂷")
    assertEquals(CardSymbols(Card(8,"♥️")), "🂸")
    assertEquals(CardSymbols(Card(9,"♥️")), "🂹")
    assertEquals(CardSymbols(Card(10,"♥️")), "🂺")
    assertEquals(CardSymbols(Card(11,"♥️")), "🂻")
    assertEquals(CardSymbols(Card(12,"♥️")), "🂽")
    assertEquals(CardSymbols(Card(13,"♥️")), "🂾")
    assertEquals(CardSymbols(Card(14,"♦")), "🃁")
    assertEquals(CardSymbols(Card(2,"♦")), "🃂")
    assertEquals(CardSymbols(Card(3,"♦")), "🃃")
    assertEquals(CardSymbols(Card(4,"♦")), "🃄")
    assertEquals(CardSymbols(Card(5,"♦")), "🃅")
    assertEquals(CardSymbols(Card(6,"♦")), "🃆")
    assertEquals(CardSymbols(Card(7,"♦")), "🃇")
    assertEquals(CardSymbols(Card(8,"♦")), "🃈")
    assertEquals(CardSymbols(Card(9,"♦")), "🃉")
    assertEquals(CardSymbols(Card(10,"♦")), "🃊")
    assertEquals(CardSymbols(Card(11,"♦")), "🃋")
    assertEquals(CardSymbols(Card(12,"♦")), "🃍")
    assertEquals(CardSymbols(Card(13,"♦")), "🃎")
    assertEquals(CardSymbols(Card(14,"♠️")), "🂡")
    assertEquals(CardSymbols(Card(2,"♠️")), "🂢")
    assertEquals(CardSymbols(Card(3,"♠️")), "🂣")
    assertEquals(CardSymbols(Card(4,"♠️")), "🂤")
    assertEquals(CardSymbols(Card(5,"♠️")), "🂥")
    assertEquals(CardSymbols(Card(6,"♠️")), "🂦")
    assertEquals(CardSymbols(Card(7,"♠️")), "🂧")
    assertEquals(CardSymbols(Card(8,"♠️")), "🂨")
    assertEquals(CardSymbols(Card(9,"♠️")), "🂩")
    assertEquals(CardSymbols(Card(10,"♠️")), "🂪")
    assertEquals(CardSymbols(Card(11,"♠️")), "🂫")
    assertEquals(CardSymbols(Card(12,"♠️")), "🂭")
    assertEquals(CardSymbols(Card(13,"♠️")), "🂮")
    assertEquals(CardSymbols(Card(14,"♣️")), "🃑")
    assertEquals(CardSymbols(Card(2,"♣️")), "🃒")
    assertEquals(CardSymbols(Card(3,"♣️")), "🃓")
    assertEquals(CardSymbols(Card(4,"♣️")), "🃔")
    assertEquals(CardSymbols(Card(5,"♣️")), "🃕")
    assertEquals(CardSymbols(Card(6,"♣️")), "🃖")
    assertEquals(CardSymbols(Card(7,"♣️")), "🃗")
    assertEquals(CardSymbols(Card(8,"♣️")), "🃘")
    assertEquals(CardSymbols(Card(9,"♣️")), "🃙")
    assertEquals(CardSymbols(Card(10,"♣️")), "🃚")
    assertEquals(CardSymbols(Card(11,"♣️")), "🃛")
    assertEquals(CardSymbols(Card(12,"♣️")), "🃝")
    assertEquals(CardSymbols(Card(13,"♣️")), "🃞")

  test("order of suits is correct"):
    assertEquals(order(Spade), 0)
    assertEquals(order(Heart), 1)
    assertEquals(order(Diamond), 2)
    assertEquals(order(Club), 3)

  test("values are correct"):
    val all = Value.AllValues
    val test =
      (0 until 20).map(i =>
        if i < 2 || i > 14 then !all.contains(i)
        else all.contains(i)
      )
    assert(test.forall(_ == true))

  test("card suit and value comparators work correctly as well as their complete names"):
    val card1 = Card(11, Heart)
    val card2 = Card(7, Heart)
    val card3 = Card(11, Spade)
    assert(card1.sameSuit(card2) && !card1.sameValue(card2))
    assert(!card1.sameSuit(card3) && card1.sameValue(card3))
    assertEquals("J" + card1.suit, card1.completeName())
    assertEquals(s"${card2.value}${card2.suit}", card2.completeName())

  test("card symbols' apply method works corretly"):
    val card1 = Card(11, Heart)
    val card2 = Card(14, Club)
    val card3 = Card(2, Diamond)
    val card4 = Card(10, Spade)
    assertEquals(CardSymbols.apply(card1), "🂻")
    assertEquals(CardSymbols.apply(card2), "🃑")
    assertEquals(CardSymbols.apply(card3), "🃂")
    assertEquals(CardSymbols.apply(card4), "🂪")

object TypesSpecifications extends Properties("Types"):
  override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withMinSuccessfulTests(500)

  val suit: Gen[String] =
        val spade = const(Suit.Spade)
        val heart = const(Suit.Heart)
        val diamond = const(Suit.Diamond)
        val club = const(Suit.Club)
        oneOf(spade, heart, diamond, club)

  val value: Gen[Value] = choose(2, 14)

  val card: Gen[Card] =
      for
          v <- value
          s <- suit
      yield Card(v, s)

  given Arbitrary[Card] = Arbitrary(card)

  property("AllCards contains a randomly generated card") =
    forAll { (card: Card) => AllCards.apply.contains(card) }

