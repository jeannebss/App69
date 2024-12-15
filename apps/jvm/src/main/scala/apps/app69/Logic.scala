package apps
package app69

import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.server.{StateMachine}
import ujson.Value
import apps.*
import cs214.webapp.UserId
import app69.Hands.*



import scala.annotation.unused
import scala.util.{Random, Try}

class Logic extends StateMachine[Event, GameState, View]:
    
    val appInfo: AppInfo = AppInfo(
        id = "app69",
        name = "6poker9",
        description = "6poker9 is a strategic poker game designed for players of all skill " +
            "levels, emphasizing thoughtful decision-making and competitive, engaging gameplay.",
        year = 2024
    )

    override val wire = Wire

    private val END_ROUND_PAUSE_MS = 5000

    private val RANDOM = new Random()

    private val STARTING_BALANCE = 1000

    private val SMALL_BLIND = 10

    private var PLAYERS = Vector[UserId]()

    import app69.Phase.* 
    import app69.Event.*
    import app69.Choice.* 
    import app69.PhaseView.*
    import app69.Card.*
    import app69.PlayersView.*
    import app69.Hands.*
    override def init(clients: Seq[UserId]): GameState =
        PLAYERS = clients.toVector
        if clients.size < 2 || clients.size > 5 then
            throw IllegalMoveException("Not enough players")
        val allCards = RANDOM.shuffle(AllCards.apply)
        val dealerCards = allCards.take(5)
        val remainingCard = allCards.drop(5)
        val players = clients.toList
        val playerCards = (0 until players.size).map(n => (players(n), Hand(remainingCard(n * 2), remainingCard(n*2+1)))).toMap
        GameState(
            players,
            clients.map(_ -> STARTING_BALANCE).toMap.updated(clients.head, STARTING_BALANCE - SMALL_BLIND),
            0,
            clients.tail.head,
            dealerCards,
            playerCards,
            Phase.InGame(0),
            clients.map(_ -> true).toMap,
            clients.head,
            clients.head,
            clients.map(_ -> 0).toMap.updated(clients.head, SMALL_BLIND),
        )


    override def transition(state: GameState)(userId: UserId, event: Event): Try[Seq[Action[GameState]]] = Try:
        import Phase.*
        import Event.*
        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = state
        
        val number = players.size

        def selectNextPlayer(newActivePlayer: Map[UserId, Boolean], ref: Int): UserId =
            val player = players(ref)
            return if newActivePlayer(player) then player else selectNextPlayer(newActivePlayer, (ref + 1)%number)

        (phase, event) match
            case (InGame(turn), PlayerAction(choice)) =>
                if (userId != currentPlayer) then {
                    throw IllegalMoveException("It's not your turn")
                }

                val highestBet = turnBets.values.max
                //Check the highest amount you can raise(Not sure)
                val raiseCapacity = playerBalance.filter((k, v) => activePlayer(k)).map((k, v) => v - (highestBet - turnBets(k)))
                val maxRaise = raiseCapacity.min

                choice match
                    case Check =>
                        //No player actions
                        if (turnBets(userId) != highestBet || turnBets.forall((id, money) => money != 0))
                            then throw IllegalMoveException("You cannot check")
                        
                        val updateCurrentPlayer = selectNextPlayer(activePlayer, (players.indexOf(currentPlayer)+1)%number)
                        val turnOver = updateCurrentPlayer == highestBetter


                        val nextPoolValue = if turnOver then poolValue + turnBets.values.sum else poolValue
                        val nextTurnBet = if turnOver then players.map(_ -> 0).toMap else turnBets
                        val nextCurrentPlayer = if turnOver then 
                            selectNextPlayer(activePlayer, players.indexOf(smallBlind)) else updateCurrentPlayer


                        //Passe au reveal si un joueur est tapis ou si on est a la fin du dernier round
                        val nextPhase = if turnOver then 
                            if turn == 3 || playerBalance.exists((id, v) => v == 0) || activePlayer.count((id, v) => v) <= 1 then Reveal else InGame(turn + 1)
                            else InGame(turn)

                        //Si reveal calcul du gagnant et update avant de switch to reveal
                        if nextPhase == Reveal then
                            val winner = WinnerLogic.winner(playerCards, dealerCards, activePlayer)
                            val updatedPlayerBalance = playerBalance.map( (id, amount) =>
                                val newAmount = if winner.contains(id) then amount + nextPoolValue/winner.size else amount
                                (id, newAmount))

                            val nextPlayers = players.filter(id => updatedPlayerBalance(id) > 0)

                            val nextPlayerBalance = updatedPlayerBalance.filter((id, amount) => nextPlayers.contains(id))

                            val nextActivePlayer = nextPlayers.map( _ -> false).toMap

                            val nextActiveBlind = players.map(id => (id, nextPlayers.contains(id))).toMap
                            val nextCurrentPlayer = selectNextPlayer(nextActiveBlind, (players.indexOf(smallBlind)+1)%players.size)
                            val nextSmallBlind = selectNextPlayer(nextActiveBlind, (players.indexOf(smallBlind))%players.size)

                            
                            val showCardPhase = state.copy(phase = CardReveal(winner.toVector), playerBalance = updatedPlayerBalance, activePlayer = nextActivePlayer)

                            val nextState = state.copy(currentPlayer = nextCurrentPlayer, phase = nextPhase, playerBalance = nextPlayerBalance, activePlayer = nextActivePlayer, players = nextPlayers, poolValue = 0, smallBlind = nextSmallBlind, turnBets = nextPlayers.map(_ -> 0).toMap, highestBetter = nextSmallBlind)
                            
                            Seq(Action.Render(showCardPhase), Action.Pause(END_ROUND_PAUSE_MS), Action.Render(nextState))
                        else
                            val viewingPhase = PlayerChoice(turn, choice)
                            val nextHighestBetter = if turnOver then selectNextPlayer(activePlayer, players.indexOf(smallBlind)) else highestBetter
                            val nextDsiplayState = state.copy(phase = viewingPhase, turnBets = nextTurnBet)
                            val nextGamingState = state.copy(poolValue = nextPoolValue, currentPlayer = updateCurrentPlayer, phase = nextPhase, turnBets = nextTurnBet, highestBetter = nextHighestBetter)
                            Seq(Action.Render(nextDsiplayState), Action.Pause(END_ROUND_PAUSE_MS), Action.Render(nextGamingState))

                    case Call =>

                        if (turnBets.forall((id, money) => money == 0))
                            then throw IllegalMoveException("You cannot Call")

                        //Modify all the balance
                        val diff = highestBet - turnBets(currentPlayer)
                        if (diff > playerBalance(userId))
                            then throw IllegalMoveException("You cannot call more than your balance")
                        val updateTurnBet = turnBets.updated(userId, highestBet)
                        val nextPlayerBalance = playerBalance.updated(userId, playerBalance(userId) - diff)

                        val updateCurrentPlayer = selectNextPlayer(activePlayer, (players.indexOf(currentPlayer)+1)%number)
                        val turnOver = updateCurrentPlayer == highestBetter

                        //Update pool, bet and active player
                        val nextPoolValue = if turnOver then poolValue + updateTurnBet.values.sum else poolValue
                        val nextTurnBet = if turnOver then players.map(_ -> 0).toMap else updateTurnBet
                        val nextCurrentPlayer = if turnOver then 
                            selectNextPlayer(activePlayer, players.indexOf(smallBlind)) else updateCurrentPlayer

                        //Passe au reveal si un joueur est tapis ou si on est a la fin du dernier round
                        val nextPhase = if turnOver then 
                            if turn == 3 || nextPlayerBalance.exists((id, v) => v == 0) || activePlayer.count((id, v) => v) <= 1 then Reveal else InGame(turn + 1)
                            else InGame(turn)

                        //Si reveal calcul du gagnant et update avant de switch to reveal
                        if nextPhase == Reveal then
                            val winner = WinnerLogic.winner(playerCards, dealerCards, activePlayer) // détermine les Id du/des gagnants
                            val updatedPlayerBalance = nextPlayerBalance.map( (id, amount) =>
                                val newAmount = if winner.contains(id) then amount + nextPoolValue/winner.size else amount
                                (id, newAmount))


                            val nextPlayers = players.filter(id => updatedPlayerBalance(id) > 0)

                            val futurPlayerBalance = updatedPlayerBalance.filter((id, amount) => nextPlayers.contains(id))

                            val nextActiveBlind = players.map(id => (id, nextPlayers.contains(id))).toMap
                            val nextCurrentPlayer = selectNextPlayer(nextActiveBlind, (players.indexOf(smallBlind) + 1)%players.size)
                            val nextSmallBlind = selectNextPlayer(nextActiveBlind, (players.indexOf(smallBlind))%players.size)

                            val nextActivePlayer = nextPlayers.map( _ -> false).toMap
                            
                            val showCardPhase = state.copy(phase = CardReveal(winner.toVector), playerBalance = updatedPlayerBalance, activePlayer = nextActivePlayer)

                            val nextState = state.copy(currentPlayer = nextSmallBlind, phase = nextPhase, playerBalance = futurPlayerBalance, activePlayer = nextActivePlayer, players = nextPlayers, poolValue = 0, smallBlind = nextCurrentPlayer, turnBets = nextPlayers.map(_ -> 0).toMap, highestBetter = nextSmallBlind)
                            
                            Seq(Action.Render(showCardPhase), Action.Pause(5000), Action.Render(nextState))
                        else
                            val viewingPhase = PlayerChoice(turn, choice)
                            val nextDsiplayState = state.copy(phase = viewingPhase, turnBets = nextTurnBet) 
                            val nextHighestBetter = if turnOver then selectNextPlayer(activePlayer, players.indexOf(smallBlind)) else highestBetter
                            val nextState = state.copy(playerBalance = nextPlayerBalance, poolValue = nextPoolValue, currentPlayer = nextCurrentPlayer, phase = nextPhase, turnBets = nextTurnBet, highestBetter = nextHighestBetter)
                            Seq(Action.Render(nextDsiplayState), Action.Pause(END_ROUND_PAUSE_MS), Action.Render(nextState))

                    case Fold =>
                        if (turnBets(userId) - turnBets(highestBetter) == 0)
                            then throw IllegalMoveException("You cannot fold cause no one has raised")
                        //Fold Action
                        val nextActivePlayer = activePlayer.updated(userId, false)
                        
                        //Turn end and next player action
                        val updateCurrentPlayer = selectNextPlayer(nextActivePlayer, (players.indexOf(currentPlayer)+1)%number)
                        val turnOver = updateCurrentPlayer == highestBetter

                        //TurnOver reaction
                        val nextPoolValue = if turnOver then poolValue + turnBets.values.sum else poolValue
                        val nextTurnBet = if turnOver then players.map(_ -> 0).toMap else turnBets
                        val nextCurrentPlayer = if turnOver then 
                            selectNextPlayer(nextActivePlayer, players.indexOf(smallBlind)) else updateCurrentPlayer

                        //Passe au reveal si un joueur est tapis ou si on est a la fin du dernier round
                        val nextPhase = if turnOver then 
                            if turn == 3 || playerBalance.exists((id, v) => v == 0) || nextActivePlayer.count((id, v) => v) <= 1 then Reveal else InGame(turn + 1)
                            else InGame(turn)
                        
                        //Si reveal calcul du gagnant et update avant de switch to reveal phase
                        if nextPhase == Reveal then
                            val winner = WinnerLogic.winner(playerCards, dealerCards, nextActivePlayer)
                            val updatedPlayerBalance = playerBalance.map( (id, amount) =>
                                val newAmount = if winner.contains(id) then amount + nextPoolValue/winner.size else amount
                                (id, newAmount)
                            )

                            val nextPlayers = players.filter(id => updatedPlayerBalance(id) > 0)

                            val nextPlayerBalance = updatedPlayerBalance.filter((id, amount) => nextPlayers.contains(id))

                            val nextActiveBlind = players.map(id => (id, nextPlayers.contains(id))).toMap
                            val nextSmallBlind = selectNextPlayer(nextActiveBlind, (players.indexOf(smallBlind))%players.size)
                            val nextCurrentPlayer = selectNextPlayer(nextActivePlayer, (players.indexOf(nextSmallBlind))%players.size)

                            val updateActivePlayer = nextPlayers.map( _ -> false).toMap

                            val showCardPhase = state.copy(phase = CardReveal(winner.toVector), playerBalance = updatedPlayerBalance, activePlayer = updateActivePlayer)

                            val nextState = state.copy(currentPlayer = nextSmallBlind, phase = Reveal, playerBalance = nextPlayerBalance, activePlayer = updateActivePlayer, players = nextPlayers, poolValue = 0, smallBlind = nextSmallBlind, turnBets = nextPlayers.map(_ -> 0).toMap, highestBetter = nextSmallBlind )
                            Seq(Action.Render(showCardPhase), Action.Pause(END_ROUND_PAUSE_MS), Action.Render(nextState))
                        else
                            val viewingPhase = PlayerChoice(turn, choice)
                            val nextDsiplayState = state.copy(phase = viewingPhase, turnBets = nextTurnBet, activePlayer = nextActivePlayer)
                            val nextHighestBetter = if turnOver then selectNextPlayer(nextActivePlayer, players.indexOf(smallBlind)) else highestBetter
                            val nextState = state.copy(activePlayer = nextActivePlayer, poolValue = nextPoolValue, currentPlayer = nextCurrentPlayer, phase = nextPhase, turnBets = nextTurnBet, highestBetter = nextHighestBetter)
                            Seq(Action.Render(nextDsiplayState), Action.Pause(END_ROUND_PAUSE_MS), Action.Render(nextState))

                    case Raise(value) =>
                        //Players action link code, cannot turnOver if someone raise
                        val diff = highestBet + value - turnBets(currentPlayer)
                        if (diff > playerBalance(userId) || value > maxRaise || value <= 0)
                            then throw IllegalMoveException("You cannot raise more than your balance or 0")
                        val nextPlayerBalance = playerBalance.updated(userId, playerBalance(userId) - diff)
                        val nextTurnBet = turnBets.updated(userId, highestBet + value)
                        val nextHighestBetter = userId

                        val nextCurrentPlayer = selectNextPlayer(activePlayer, (players.indexOf(currentPlayer)+1)%number)

                        val viewingPhase = PlayerChoice(turn, choice)
                        val nextDsiplayState = state.copy(phase = viewingPhase, turnBets = nextTurnBet)

                        val nextState = state.copy(currentPlayer = nextCurrentPlayer, playerBalance = nextPlayerBalance, turnBets = nextTurnBet, highestBetter = nextHighestBetter)
                        Seq(Action.Render(nextDsiplayState), Action.Pause(END_ROUND_PAUSE_MS), Action.Render(nextState))
            case (InGame(turn),_) => throw IllegalMoveException("You can only play in this phase of the game")

            case (Reveal, Ready) => 
                

                val newActivePlayer = activePlayer.updated(userId, true)

                if (newActivePlayer.size == 1) then Seq(Action.Pause(1000), Action.Render(state.copy(phase = EndGame, activePlayer = PLAYERS.map(_ -> false).toMap)))
                else 

                 
                if newActivePlayer.forall((k,v) => v) then

                    val newAllCards = RANDOM.shuffle(AllCards.apply)
                    val newDealerCards = newAllCards.take(5)
                    val newRemainingCard = newAllCards.drop(5) 
                    val newPlayerCards = (0 until players.size).map(n => (players(n), Hand(newRemainingCard(n * 2), newRemainingCard(n*2+1)))).toMap
                    val newGameState = state.copy(  activePlayer = newActivePlayer, 
                                                    dealerCards = newDealerCards,
                                                    playerCards = newPlayerCards,
                                                    currentPlayer = selectNextPlayer(newActivePlayer, (players.indexOf(smallBlind) + 1)%players.size),
                                                    playerBalance = playerBalance.updated(userId, playerBalance(userId) - SMALL_BLIND),
                                                    turnBets = turnBets.updated(userId, SMALL_BLIND),
                                                    phase = InGame(0))
                    Seq(Action.Pause(1000), Action.Render(newGameState))
                else
                    //Good
                    val newGameState = state.copy(activePlayer = newActivePlayer)
                    Seq(Action.Pause(1000), Action.Render(newGameState))
                                    
            case (Reveal, _) => throw IllegalMoveException("Compare your cards to the other's")
            case (EndGame, Ready) => 
                val playersReady = activePlayer.updated(userId, true)
                if playersReady.forall((k, v) => v) then Seq(Action.Pause(1000), Action.Render(init(PLAYERS.toSeq)))
                else Seq(Action.Render(state.copy(activePlayer = playersReady)))
                    
            case (_) => throw IllegalMoveException("Unsupported phase")

    override def project(state: GameState)(userId: UserId): View =
        val GameState(
            _,
            playerBalance,
            poolValue,
            currentPlayer,
            dealerCards,
            playerCards,
            phase,
            activePlayer,
            smallBlind,
            highestBetter,
            turnBets
        ) = state
        
        def numberOfCard(turn: Int): Int =
            turn match
                case 0 => 0
                case 1 => 3
                case 2 => 4
                case _ => 5
        
        val indexes = PLAYERS.filter(_ != userId).zipWithIndex.toMap
        phase match
            case InGame(turn) =>
                val phaseView = 
                    if userId == currentPlayer then ChoiceSelection
                    else NotPlaying
                val tableView = TableView(
                    dealerCards.take(numberOfCard(turn)).toVector,
                    poolValue
                )
                val playersView = InGamePlayer(
                    indexes,
                    playerBalance,
                    activePlayer,
                    currentPlayer,
                    turnBets,
                    playerCards(userId)
                )
                View(phaseView, playersView, tableView)
            
            case PlayerChoice(turn, choice) =>
                val phaseView = ChoiceMade(choice, currentPlayer)
                val tableView = TableView(
                    dealerCards.take(numberOfCard(turn)).toVector,
                    poolValue
                )
                val playersView = InGamePlayer(
                    indexes,
                    playerBalance,
                    activePlayer,
                    currentPlayer,
                    turnBets,
                    playerCards(userId)
                )
                View(phaseView, playersView, tableView)

            case CardReveal(winners) =>
                val phaseView = Winners(winners)
                val tableView = TableView(dealerCards.toVector, poolValue)
                val playerView = PlayerCardReveal(
                    indexes,
                    playerBalance,
                    PLAYERS.map(player => (player, true)).toMap,
                    playerCards
                )
                View(phaseView, playerView, tableView)

            case Reveal =>
                val tableView = TableView(dealerCards.toVector, poolValue)
                val playerView = PlayerCardReveal(
                    indexes,
                    playerBalance,
                    PLAYERS.map(player => (player, true)).toMap,
                    playerCards
                )
                View(IsReady(activePlayer(userId)), playerView, tableView)

            case EndGame => 
                val tableView = TableView(dealerCards.toVector, poolValue)
                val playerView = PlayerCardReveal(
                    indexes,
                    playerBalance,
                    PLAYERS.map(player => (player, true)).toMap,
                    playerCards
                )
                val maxBalance = playerBalance.values.max
                val winners = playerBalance.filter(_._2 == maxBalance).keys.toVector
                val phaseView = End(winners, maxBalance)
                View(phaseView, playerView, tableView)
