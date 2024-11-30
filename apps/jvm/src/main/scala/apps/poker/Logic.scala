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
        val dealerCards = allCards.take(5).toList
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
            clients.zipWithIndex.map((player, index) =>
                    val cardStartIndex = 5 + index * 2
                    val hand = (allCards(cardStartIndex), allCards(cardStartIndex + 1))
                    player -> hand
                ).toMap,
            Phase.PreFlop, // on peut aussi directement mettre preFlop
            clients.map(_ -> false).toMap,
            clients.head,
            clients.map(_ -> 0).toMap,
            clients
        )


    override def transition(state: GameState)(userId: UserId, event: Event): Try[Seq[Action[State]]] = 
        val GameState(playerBalance, poolValue, roundBets, currentPlayer, dealerCards, playerCards, phase, canStillPlay, smallBlind, turnBets, players) = state

        if (userId == currentPlayer && canStillPlay(userId)) then
            phase match
                case Phase.PreFlop => event match
                    case PlayerAction(action) => action match
                        case Check => if(turnBets.forall((user, value) => value == 0)) then 
                            val nextP = players((players.indexOf(currentPlayer) + 1) % players.size)
                            if(players.indexOf(nextP) == players.indexOf(smallBlind) && turnBets.forall((key, value) => value == turnBets(currentPlayer))) then
                                val nextState = Phase.Flop
                            else 
                                val nextState = phase
                            state.copy(currentPlayer = nextP, phase = nextPhase)//tester si c'est la dernière phase
                            else throw IllegalMoveException("You have to call or fold!")
                        case Call =>
                        case Fold =>
                        case Raise(value) =>
                    
                    case EndGameChoice(choice) =>
                
                case Phase.Flop => 
                case Phase.Turn => 
                case Phase.Reverse =>
                case Phase.EndGame =>
        else{
            throw IllegalMoveException("not your turn")
        }


    override def project(state: GameState)(userId: UserId): View = 
        val GameState(playerBalance, poolValue, roundBets, currentPlayer, dealerCards, playerCards, phase, canStillPlay, smallBlind, turnBets, players) = state

        phase match
            case Phase.PreFlop => View(PhaseView.InGame(currentPlayer, playerCards(userId)), (playerBalance, poolValue), dealerCards.take(0))
            case Phase.Flop => View(PhaseView.InGame(currentPlayer, playerCards(userId)), (playerBalance, poolValue), dealerCards.take(3)) 
            case Phase.Turn => View(PhaseView.InGame(currentPlayer, playerCards(userId)), (playerBalance, poolValue), dealerCards.take(4)) 
            case Phase.Reverse => View(PhaseView.InGame(currentPlayer, playerCards(userId)), (playerBalance, poolValue), dealerCards.take(5))
            case Phase.EndGame => View(PhaseView.Winner(playerBalance.filter((key, value) => value != 0).head._1), (playerBalance, 0), dealerCards.take(0))
            
        
            
        