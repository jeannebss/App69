package apps

import cs214.webapp.UserId
import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.utils.WebappSuite

import scala.language.adhocExtensions
import org.scalacheck.*
import Gen.*
import Prop.*

import apps.Phase.* 
import apps.Event.*
import apps.Choice.* 
import apps.PhaseView.*
import apps.Card.*
import apps.CardView.*
import apps.Wire.*
import apps.Wire.eventFormat
import apps.Wire.PhaseViewWire
import apps.Wire.viewFormat

// Simple encode decode tests
class WireTest extends munit.FunSuite:
    val sm = app69.Logic()

    test("Choice wire encode and decode correctly"):
        val choices = Seq(Check, Call, Fold, Raise(100))
        for choice <- choices do
            val res = ChoiceWire.decode(ChoiceWire.encode(choice)).get
            assertEquals(choice, res)

    test("Phase wire encode and decode correctly"):
        val phases = Seq(
            InGame(1),
            PlayerChoice(2, Raise(50)),
            CardReveal,
            Reveal,
            EndGame
        )
        for phase <- phases do
            val res = PhaseWire.decode(PhaseWire.encode(phase)).get
            assertEquals(phase, res)

    test("Card wire encode and decode correctly"):
        for card <- AllCards.apply do
            val res = CardWire.decode(CardWire.encode(card)).get
            assertEquals(card, res)

    test("Hand wire encode and decode correctly"):
        for card1 <- AllCards.apply; card2 <- AllCards.apply do
            val hand = Hand(card1, card2)
            val res = HandWire.decode(HandWire.encode(hand)).get
            assertEquals(hand, res)
        
    test("ScoresView wire encode and decode correctly"):
        val scoresView = ScoresView(
            playerScores = Map("Player1" -> 150, "Player2" -> 100),
            poolBalance = 250
        )
        val res = ScoresViewWire.decode(ScoresViewWire.encode(scoresView)).get
        assertEquals(scoresView, res)

    test("PhaseView wire encode and decode correctly"):
        val phaseView = Seq(
            ChoiceSelection,
            ChoiceMade(Raise(20)),
            Winner
        )
        for pV <- phaseView do
            val res = PhaseViewWire.decode(PhaseViewWire.encode(pV)).get
            assertEquals(pV, res)

    test("CardView wire encode and decode correctly"):
        val inGameCards = InGameCards(
            Hand(Card(2, Suit.Diamond), Card(14, Suit.Heart)),
            Vector(Card(7, Suit.Spade), Card(11, Suit.Club))
        )
        val revealCards = RevealCards(
            playerCards = Map(
                "Player1" -> Hand(Card(3, Suit.Spade), Card(6, Suit.Heart)),
                "Player2" -> Hand(Card(5, Suit.Heart), Card(11, Suit.Club))
            ),
            dealerCards = Vector(Card(9, Suit.Diamond), Card(13, Suit.Club))
        )
        val res1 = CardViewWire.decode(CardViewWire.encode(inGameCards)).get
        val res2 = CardViewWire.decode(CardViewWire.encode(revealCards)).get
        assertEquals(inGameCards, res1)
        assertEquals(revealCards, res2)

    test("Event wire encode and decode correctly"):
        val events = Seq(
            PlayerAction(Raise(50)),
            EndGameChoice(choice = true),
            Ready
        )
        for event <- events do
            val res = eventFormat.decode(eventFormat.encode(event)).get
            assertEquals(event, res)

    test("View wire encode and decode correctly"):
        val view = View(
            phaseView = ChoiceSelection,
            scoresView = ScoresView(
                playerScores = Map("Player1" -> 200, "Player2" -> 300),
                poolBalance = 500
            ),
            cardView = InGameCards(
                playerCards = Hand(Card(10, Suit.Club), Card(11, Suit.Spade)),
                dealerCards = Vector(Card(4, Suit.Heart), Card(8, Suit.Diamond))
            )
        )
        val res = viewFormat.decode(viewFormat.encode(view)).get
        assertEquals(view, res)

// Randomly generated tests
object WireSpecifications extends Properties("Wire"):
    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withMinSuccessfulTests(500)

    val raise: Gen[Choice] =
        for
            x <- double
        yield Raise(x.toInt)
    
    val choice: Gen[Choice] = 
        oneOf(const(Check), const(Call), const(Fold), raise)
    
    val playerAction: Gen[Event] =
        for
            c <- choice
        yield PlayerAction(c)

    val endGameChoice: Gen[Event] = 
        for
            b <- oneOf(true, false)
        yield EndGameChoice(b)
    
    val event: Gen[Event] =
        oneOf(playerAction, endGameChoice, const(Ready))

    val inGame: Gen[Phase] =
        for
            i <- double
        yield InGame(i.toInt)

    val playerChoice: Gen[Phase] =
        for
            i <- double
            c <- choice
        yield PlayerChoice(i.toInt, c)

    val phase: Gen[Phase] =
        oneOf(inGame, playerChoice, const(CardReveal), const(Reveal), const(EndGame))
    
    val choiceMade: Gen[PhaseView] =
        for
            c <- choice
        yield ChoiceMade(c)

    val phaseView: Gen[PhaseView] =
        oneOf(const(ChoiceSelection), choiceMade, const(Winner))

    val userId: Gen[UserId] = Arbitrary.arbitrary[String]
    val balance: Gen[Balance] = Arbitrary.arbitrary[Int]
    val tupleUserBal: Gen[(UserId, Balance)] = Gen.zip(userId, balance)

    val scoresView: Gen[ScoresView] =
        for
            m <- buildableOf[Map[UserId, Balance], (UserId, Balance)](tupleUserBal)
            i <- double
        yield ScoresView(m, i.toInt)

    val suit: Gen[String] =
        val spade = const(Suit.Spade)
        val heart = const(Suit.Heart)
        val diamond = const(Suit.Diamond)
        val club = const(Suit.Club)
        oneOf(spade, heart, diamond, club)

    val value: Gen[Value] =
        for
            i <- choose(2, 14)
        yield i

    val card: Gen[Card] =
        for
            v <- value
            s <- suit
        yield Card(v, s)

    val hand: Gen[Hand] =
        for
            c1 <- card
            c2 <- card
        yield Hand(c1, c2)

    val cards: Gen[Vector[Card]] = Gen.containerOf[Vector, Card](card)

    val tupleUserHand: Gen[(UserId, Hand)] = Gen.zip(userId, hand)

    val inGameCards: Gen[CardView] =
        for 
            h <- hand
            c <- cards
        yield InGameCards(h, c)

    val revealCards: Gen[CardView] =
        for
            m <- Gen.buildableOf[Map[UserId, Hand], (UserId, Hand)](tupleUserHand)
            c <- cards
        yield RevealCards(m, c)

    val cardView: Gen[CardView] = 
        oneOf(inGameCards, revealCards)

    val view: Gen[View] =
        for
            p <- phaseView
            s <- scoresView
            c <- cardView
        yield View(p, s, c)

    given Arbitrary[Choice] = Arbitrary(choice)
    given Arbitrary[Event] = Arbitrary(event)
    given Arbitrary[Phase] = Arbitrary(phase)    
    given Arbitrary[PhaseView] = Arbitrary(phaseView)
    given Arbitrary[CardView] = Arbitrary(cardView)
    given Arbitrary[ScoresView] = Arbitrary(scoresView)
    given Arbitrary[View] = Arbitrary(view)
    given Arbitrary[Hand] = Arbitrary(hand)    
    given Arbitrary[Card] = Arbitrary(card)

    property("choice wire encodes and decodes correctly for arbitrary choice") =
        forAll { (choice: Choice) => ChoiceWire.decode(ChoiceWire.encode(choice)).get == choice } 

    property("event format wire encodes and decodes correctly for arbitrary event") =
        forAll { (event: Event) => eventFormat.decode(eventFormat.encode(event)).get == event }

    property("phase format wire encodes and decodes correctly for arbitrary phase") = 
        forAll { (phase: Phase) => PhaseWire.decode(PhaseWire.encode(phase)).get == phase } 

    property("phase view wire encodes and decodes correctly for arbitrary phase view") = 
        forAll { (phaseView: PhaseView) => PhaseViewWire.decode(PhaseViewWire.encode(phaseView)).get == phaseView } 

    property("card view wire encodes and decodes correctly for arbitrary card view") = 
        forAll { (cardView: CardView) => CardViewWire.decode(CardViewWire.encode(cardView)).get == cardView } 

    property("scores view wire encodes and decodes correctly for arbitrary scores view") = 
        forAll { (scoresView: ScoresView) => ScoresViewWire.decode(ScoresViewWire.encode(scoresView)).get == scoresView }

    property("view format wire encodes and decodes correctly for arbitrary view") =
        forAll { (view: View) => viewFormat.decode(viewFormat.encode(view)).get == view } 

    property("hand wire encodes and decodes correctly for arbitrary hand") =
        forAll { (hand: Hand) => HandWire.decode(HandWire.encode(hand)).get == hand } 

    property("card wire encodes and decodes correctly for arbitrary card") =
        forAll { (card: Card) => CardWire.decode(CardWire.encode(card)).get == card }
