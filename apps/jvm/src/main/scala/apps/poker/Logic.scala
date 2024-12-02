package apps.poker

import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.server.{StateMachine}
import ujson.Value
import apps.*
import scala.collection.immutable.{SortedMap}


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

    override val wire = Wire

    private val END_ROUND_PAUSE_MS = 5000

    private val STARTING_BALANCE = 1000

    import apps.Phase.* 
    import apps.Event.*
    import apps.Choice.* 
    import apps.PhaseView.*
    import apps.Card.*

    override def init(clients: Seq[UserId]): GameState =
        val allCards = Random.shuffle(AllCards.apply)
        val dealerCards = allCards.take(5)
        val remainingCard = allCards.drop(5).toList
        val listClients = clients.toList
        val playerCards = (0 until listClients.size).map(n => (listClients(n), Hand(remainingCard(n * 2), remainingCard(n*2+1)))).toMap
        GameState(
            SortedMap(clients.map(_ -> STARTING_BALANCE)*),
            0,
            clients.head,
            dealerCards,
            playerCards,
            Phase.InGame(0),
            clients.map(_ -> true).toMap,
            clients.head,
            clients.head,
            clients.map(_ -> 0).toMap,
        )


    override def transition(state: GameState)(userId: UserId, event: Event): Try[Seq[Action[GameState]]] = Try:
        import Phase.*
        import Event.*
        val GameState(playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = state
        val players = playerBalance.keys.toList
        val number = players.size

        def selectNextPlayer(newActivePlayer: Map[UserId, Boolean], ref: Int): UserId =
            val player = players(ref)
            return if newActivePlayer(player) then player else selectNextPlayer(newActivePlayer, (ref + 1)%number)

        //TODO add modulo and check a new turnOver check
        (phase, event) match
            case (InGame(turn), PlayerAction(choice)) =>
                require(userId == currentPlayer)

                val highestBet = turnBets.values.max
                //Check the highest amount you can raise(Not sure)
                val raiseCapacity = playerBalance.filter((k, v) => activePlayer(k)).map((k, v) => v - (highestBet - turnBets(k)))
                val maxRaise = raiseCapacity.min

                choice match
                    case Check =>
                        require(turnBets(userId) == highestBet)
                        
                        val updateCurrentPlayer = selectNextPlayer(activePlayer, (players.indexOf(currentPlayer)+1)%number)
                        val turnOver = updateCurrentPlayer == highestBetter

                        val nextPoolValue = if turnOver then poolValue + turnBets.values.sum else poolValue
                        val nextTurnBet = if turnOver then players.map(_ -> 0).toMap else turnBets
                        val nextCurrentPlayer = if turnOver then 
                            selectNextPlayer(activePlayer, players.indexOf(smallBlind)) else updateCurrentPlayer


                        val nextPhase = if turnOver then
                            if turn == 3 then Reveal else InGame(turn + 1)
                            else InGame(turn)
                        
                        val nextState = state.copy(poolValue = nextPoolValue, currentPlayer = nextCurrentPlayer, phase = nextPhase, turnBets = nextTurnBet)
                        Seq(Action.Render(nextState))

                    case Call =>
                        //Modify all the balance
                        val diff = highestBet - turnBets(currentPlayer)
                        val updateTurnBet = turnBets.updated(userId, highestBet)
                        val nextPlayerBalance = playerBalance.updated(userId, playerBalance(userId) - diff)

                        val updateCurrentPlayer = selectNextPlayer(activePlayer, (players.indexOf(currentPlayer)+1)%number)
                        val turnOver = updateCurrentPlayer == highestBetter

                        //Update pool, bet and active player
                        val nextPoolValue = if turnOver then poolValue + updateTurnBet.values.sum else poolValue
                        val nextTurnBet = if turnOver then players.map(_ -> 0).toMap else updateTurnBet
                        val nextCurrentPlayer = if turnOver then 
                            selectNextPlayer(activePlayer, players.indexOf(smallBlind)) else updateCurrentPlayer


                        val nextPhase = if turnOver then
                            if turn == 3 then Reveal else InGame(turn + 1)
                            else InGame(turn)
                        
                        
                        val nextState = state.copy(playerBalance = nextPlayerBalance, poolValue = nextPoolValue, currentPlayer = nextCurrentPlayer, phase = nextPhase, turnBets = nextTurnBet)
                        Seq(Action.Render(nextState))

                    case Fold =>
                        val nextActivePlayer = activePlayer.updated(userId, false)
                        
                        val updateCurrentPlayer = selectNextPlayer(nextActivePlayer, (players.indexOf(currentPlayer)+1)%number)
                        val turnOver = updateCurrentPlayer == highestBetter

                        val nextPoolValue = if turnOver then poolValue + turnBets.values.sum else poolValue
                        val nextTurnBet = if turnOver then players.map(_ -> 0).toMap else turnBets
                        val nextCurrentPlayer = if turnOver then 
                            selectNextPlayer(activePlayer, players.indexOf(smallBlind)) else updateCurrentPlayer
                        val nextPhase = if turnOver then
                            if turn == 3 then Reveal else InGame(turn + 1)
                            else InGame(turn)

                        val nextState = state.copy(activePlayer = nextActivePlayer, poolValue = nextPoolValue, currentPlayer = nextCurrentPlayer, phase = nextPhase, turnBets = nextTurnBet)
                        Seq(Action.Render(nextState))

                    case Raise(value) =>
                        val diff = highestBet + value - turnBets(currentPlayer)
                        require(diff <= playerBalance(userId) && value <= maxRaise)
                        val nextPlayerBalance = playerBalance.updated(userId, playerBalance(userId) - diff)
                        val nextTurnBet = turnBets.updated(userId, turnBets(userId) + value)

                        val nextCurrentPlayer = selectNextPlayer(activePlayer, (players.indexOf(currentPlayer)+1)%number)

                        val nextState = state.copy(currentPlayer = nextCurrentPlayer, playerBalance = nextPlayerBalance, turnBets = nextTurnBet)

                        Seq(Action.Render(nextState))
            
            case (InGame(turn),_) => throw IllegalMoveException("You can only play in this phase of the game")
            case (Reveal, Ready) => ???//TODO activate the player
            case (Reveal, _) => throw IllegalMoveException("Impossible move exeption") //TODO change message 
            case (_) => throw IllegalMoveException("Unsupported phase") //TODO Implement if we want a endgame

    override def project(state: GameState)(userId: UserId): View = 
        val GameState(
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
        
        phase match
            case InGame(turn) =>
                val scoresView = ScoresView(playerBalance, poolValue)
                val cardView = CardView(playerCards(userId), dealerCards.take(turn))
                val phaseView = ChoiceSelection(currentPlayer)
                View(phaseView, scoresView, cardView)
            case Reveal => 
                val scoresView = ScoresView(playerBalance, poolValue)
                val cardView = CardView(playerCards(userId), dealerCards) // needs to know turn
                val phaseView = ChoiceMade(currentPlayer, Choice.Call) // adapt
                View(phaseView, scoresView, cardView)
            case EndGame => 
                val scoresView = ScoresView(playerBalance, poolValue)
                val cardView = CardView(playerCards(userId), dealerCards) // no need 
                val phaseView = ChoiceSelection(currentPlayer) // No choice
                View(phaseView, scoresView, cardView)