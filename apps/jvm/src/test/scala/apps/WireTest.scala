package apps

import cs214.webapp.UserId
import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.utils.WebappSuite

import apps.Phase.* 
import apps.Event.*
import apps.Choice.* 
import apps.PhaseView.*
import apps.Card.*
import apps.CardView.*
import apps.Wire.*

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
