package apps.poker

import cs214.webapp.*
import cs214.webapp.server.{StateMachine}
import ujson.Value
import apps.*

import scala.annotation.unused
import scala.util.{Random, Try}

class Logic extends StateMachine[Event, GameState, View]:
    
    val appInfo: AppInfo = AppInfo(
        id = "app69",
        name = "6poker9",
        description = "6poker9 is a fun and interactive poker game where players " +
            "strategically take turns, make decisions, and outplay opponents. " +
            "Designed for friendly gameplay, it offers an engaging experience for all skill levels.",
        year = 2024
        )

    override val wire = ???

    private val END_ROUND_PAUSE_MS = 5000

    private val STARTING_BALANCE = 1000

    import apps.Phase.* 
    import apps.Event.*
    import apps.Action.* 
    import apps.PhaseView.*
    import apps.Card.*

    override def init(clients: Seq[UserId]): GameState =
        val allCards = Random.shuffle(AllCards.get())
        val dealerCards = allCards.take(5).toSet
        val playerCards = Map.empty[UserId, Hand]
        clients.foreach(player =>
            val hand = allCards.drop(5).take(2)
            playerCards + (player -> (hand.head, hand.tail.head))
        )
        GameState(
            clients.map(_ -> STARTING_BALANCE).toMap,
            0,
            clients.map(_ -> 0).toMap,
            clients.head,
            dealerCards,
        )