package apps

package app69

import cs214.webapp.UserId
import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.utils.WebappSuite

import scala.language.adhocExtensions
import org.scalacheck.*
import Gen.*
import Prop.*

import app69.Phase.* 
import app69.Event.*
import app69.Choice.* 
import app69.PhaseView.*
import app69.Card.*
import app69.PlayersView.*
import app69.Wire.*
import app69.Hands.*
import app69.Wire.eventFormat
import app69.Wire.PhaseViewWire
import app69.Wire.viewFormat

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
            CardReveal(Vector("user1", "user2")),
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

    test("Hands wire encode and decode correctly"):
        for card1 <- AllCards.apply; card2 <- AllCards.apply do
            val hand = Hand(card1, card2)
            val res = HandsWire.decode(HandsWire.encode(hand)).get
            assertEquals(hand, res)
            val empty = HandsWire.decode(HandsWire.encode(EmptyHand)).get
            assertEquals(empty, EmptyHand)

    test("PhaseView wire encode and decode correctly"):
        val phaseView = Seq(
            ChoiceSelection,
            NotPlaying,
            ChoiceMade(Raise(20), "user2"),
            Winners(Vector("user1", "user2", "user3")),
            IsReady(true),
            End(Vector("user1", "user2"), 200)
        )
        for pV <- phaseView do
            val res = PhaseViewWire.decode(PhaseViewWire.encode(pV)).get
            assertEquals(pV, res)

    test("Event wire encode and decode correctly"):
        val events = Seq(
            PlayerAction(Raise(50)),
            EndGameChoice(choice = true),
            Ready
        )
        for event <- events do
            val res = eventFormat.decode(eventFormat.encode(event)).get
            assertEquals(event, res)

    test("Table view wire encodes and decodes correctly"):
        val cards = Vector(Card(14,"♥️"), Card(7,"♥️"), Card(12,"♠️"))
        val balance = 300
        val res = TableView(cards, balance)
        assertEquals(TableViewWire.decode(TableViewWire.encode(res)).get, res)

    test("Players view wire encodes and decodes correctly"):
        val indexes = Map("user1" -> 1, "user2" -> 2)
        val balances = Map("user1" -> 100, "user2" -> 200)
        val active = Map("user1" -> true, "user2" -> false)
        val current = "user2"
        val bets = Map("user1" -> 10, "user2" -> 20)
        val hands = Map("user1" -> Hand(Card(6,"♥️"), Card(9,"♥️")), "user2" -> Hand(Card(7,"♥️"), Card(12,"♠️")))
        val inGame = InGamePlayer(indexes, balances, active, current, bets, hands("user2"))
        assertEquals(PlayersViewWire.decode(PlayersViewWire.encode(inGame)).get, inGame)
        val cardReveal = PlayerCardReveal(indexes, balances, active, hands)
        assertEquals(PlayersViewWire.decode(PlayersViewWire.encode(cardReveal)).get, cardReveal)


    test("View wire encode and decode correctly"):
        val indexes = Map("user1" -> 1, "user2" -> 2)
        val balances = Map("user1" -> 100, "user2" -> 200)
        val active = Map("user1" -> true, "user2" -> false)
        val current = "user2"
        val cards = Vector(Card(14,"♥️"), Card(7,"♥️"), Card(12,"♠️"))
        val balance = 300
        val bets = Map("user1" -> 10, "user2" -> 20)
        val hands = Map("user1" -> Hand(Card(6,"♥️"), Card(9,"♥️")), "user2" -> Hand(Card(7,"♥️"), Card(12,"♠️")))
        val view = View(
            phaseView = ChoiceSelection,
            playersView = InGamePlayer(indexes, balances, active, current, bets, hands("user2")),
            TableView(cards, balance)
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

    val userId: Gen[UserId] = Arbitrary.arbitrary[String]
    
    val choiceMade: Gen[PhaseView] =
        for
            c <- choice
            u <- userId
        yield ChoiceMade(c, u)

    val players: Gen[Vector[UserId]] = Gen.containerOf[Vector, UserId](userId)
    val balance: Gen[Balance] = Arbitrary.arbitrary[Int]

    val winners: Gen[PhaseView] =
        for 
            p <- players
        yield Winners(p)

    val cardReveal: Gen[Phase] =
        for
            w <- players
        yield CardReveal(w)
    
    val phase: Gen[Phase] =
        oneOf(inGame, playerChoice, cardReveal, const(Reveal), const(EndGame))

    val end: Gen[PhaseView] = 
        for
            w <- players
            b <- balance
        yield End(w, b)

    val isReady: Gen[PhaseView] =
        for
            b <- oneOf(true, false)
        yield IsReady(b)

    val phaseView: Gen[PhaseView] =
        oneOf(const(ChoiceSelection), const(NotPlaying), choiceMade, winners, isReady, end)

    val value: Gen[Value] =
        for
            i <- choose(2, 14)
        yield i
    
    val card: Gen[Card] =
        for
            v <- value
            s <- suit
        yield Card(v, s)
    
    val cards: Gen[Vector[Card]] = Gen.containerOf[Vector, Card](card)

    val tableView: Gen[TableView] = 
        for 
            c <- cards
            b <- balance
        yield TableView(c, b)

    val balances: Gen[Vector[Balance]] = Gen.containerOf[Vector, Balance](balance)

    val suit: Gen[String] =
        val spade = const(Suit.Spade)
        val heart = const(Suit.Heart)
        val diamond = const(Suit.Diamond)
        val club = const(Suit.Club)
        oneOf(spade, heart, diamond, club)

    val hand: Gen[Hands] =
        for
            c1 <- card
            c2 <- card
        yield Hand(c1, c2)

    val hands: Gen[Hands] = oneOf(hand, const(EmptyHand))

    val tupleUserIndex: Gen[(UserId, Int)] = Gen.zip(userId, Arbitrary.arbitrary[Int])
    val tupleUserBalance: Gen[(UserId, Balance)] = Gen.zip(userId, balance)
    val tupleUserBoolean: Gen[(UserId, Boolean)] = Gen.zip(userId, oneOf(true, false))
    val tupleUserHands: Gen[(UserId, Hands)] = Gen.zip(userId, hands)
    val tupleUserBet: Gen[(UserId, Bet)] = Gen.zip(userId, Arbitrary.arbitrary[Int])

    val mapPlayerIndex: Gen[Map[UserId, Int]] = 
        Gen.buildableOf[Map[UserId, Int], (UserId, Int)](tupleUserIndex)

    val mapPlayerBalance: Gen[Map[UserId, Balance]] = 
        Gen.buildableOf[Map[UserId, Balance], (UserId, Balance)](tupleUserBalance)

    val mapPlayerActive: Gen[Map[UserId, Boolean]] = 
        Gen.buildableOf[Map[UserId, Boolean], (UserId, Boolean)](tupleUserBoolean)

    val mapPlayerHands: Gen[Map[UserId, Hands]] = 
        Gen.buildableOf[Map[UserId, Hands], (UserId, Hands)](tupleUserHands)

    val mapPlayerBet: Gen[Map[UserId, Bet]] =
        Gen.buildableOf[Map[UserId, Bet], (UserId, Bet)](tupleUserBet)

    val inGamePlayer: Gen[PlayersView] =
        for 
            idx <- mapPlayerIndex
            bal <- mapPlayerBalance
            act <- mapPlayerActive
            cur <- userId
            bet <- mapPlayerBet
            hnd <- hands
        yield InGamePlayer(idx, bal, act, cur, bet, hnd)

    val playerCardReveal: Gen[PlayersView] =
        for
            idx <- mapPlayerIndex
            bal <- mapPlayerBalance
            act <- mapPlayerActive
            hnd <- mapPlayerHands
        yield PlayerCardReveal(idx, bal, act, hnd)

    val playersView: Gen[PlayersView] = oneOf(inGamePlayer, playerCardReveal)

    val view: Gen[View] =
        for
            ph <- phaseView
            pl <- playersView
            tb <- tableView
        yield View(ph, pl, tb)

    given Arbitrary[Choice] = Arbitrary(choice)
    given Arbitrary[Event] = Arbitrary(event)
    given Arbitrary[Phase] = Arbitrary(phase)    
    given Arbitrary[PhaseView] = Arbitrary(phaseView)
    given Arbitrary[PlayersView] = Arbitrary(playersView)
    given Arbitrary[TableView] = Arbitrary(tableView)
    given Arbitrary[View] = Arbitrary(view)
    given Arbitrary[Hands] = Arbitrary(hands)    
    given Arbitrary[Card] = Arbitrary(card)

    property("choice wire encodes and decodes correctly for arbitrary choice") =
        forAll { (choice: Choice) => ChoiceWire.decode(ChoiceWire.encode(choice)).get == choice } 

    property("event format wire encodes and decodes correctly for arbitrary event") =
        forAll { (event: Event) => eventFormat.decode(eventFormat.encode(event)).get == event }

    property("phase format wire encodes and decodes correctly for arbitrary phase") = 
        forAll { (phase: Phase) => PhaseWire.decode(PhaseWire.encode(phase)).get == phase } 

    property("phase view wire encodes and decodes correctly for arbitrary phase view") = 
        forAll { (phaseView: PhaseView) => PhaseViewWire.decode(PhaseViewWire.encode(phaseView)).get == phaseView } 

    property("players view wire encodes and decodes correctly for arbitrary players view") = 
        forAll { (playersView: PlayersView) => PlayersViewWire.decode(PlayersViewWire.encode(playersView)).get == playersView } 

    property("table view wire encodes and decodes correctly for arbitrary table view") = 
        forAll { (tableView: TableView) => TableViewWire.decode(TableViewWire.encode(tableView)).get == tableView } 

    property("view format wire encodes and decodes correctly for arbitrary view") =
        forAll { (view: View) => viewFormat.decode(viewFormat.encode(view)).get == view } 

    property("hands wire encodes and decodes correctly for arbitrary hand") =
        forAll { (hands: Hands) => HandsWire.decode(HandsWire.encode(hands)).get == hands } 

    property("card wire encodes and decodes correctly for arbitrary card") =
        forAll { (card: Card) => CardWire.decode(CardWire.encode(card)).get == card }
