package apps

package app69

import cs214.webapp.*
import cs214.webapp.client.*
import scalatags.JsDom.all
import scalatags.JsDom.all.*

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}
import cs214.webapp.client.graphics.WebClientAppInstance

import Phase.*
import PhaseView.* 
import Choice.*
import CardSymbols.back
import PlayersView.*
import Event.* 
import Hands.*

import org.scalajs.dom
import scala.compiletime.ops.double


@JSExportTopLevel("app69")
object UI extends WSClientApp:
    
    def appId: String = "app69"
    
    def uiId: UIId = "html"

    def init(userId: UserId, sendMessage: ujson.Value => Unit, target: Target): ClientAppInstance =
        UIInstance(userId, sendMessage, target)

class UIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: Target)
    extends WebClientAppInstance[Event, View](userId, sendMessage, target):

    override val wire = Wire

    override def render(userId: UserId, view: View): Frag =
        frag(
            head(
                link(
                    href := "https://fonts.googleapis.com/css2?family=Inknut+Antiqua&display=swap",
                    rel := "stylesheet"
                )
            ),
            h1(id :="name-of-page")("6poker9"),
            renderView(userId, view)
        )

    def renderView(userId: UserId, view: View): Frag =
        frag(
            renderPhase(userId, view.phaseView),
            div(cls := "all-table")(renderTable(userId, view.tableView)),
            renderPlayers(userId, view.playersView)
        )

    def renderPhase(userId: UserId, phaseView: PhaseView): Frag = 
        phaseView match
            case ChoiceSelection =>
                frag(
                    div(cls := "controls")(
                    button(id := "raise")(
                        "Raise: ",
                        input(
                            `type` := "type",
                            id := "bet",
                            placeholder := "Enter bet",
                            size := 6,
                            onkeydown := { (event: dom.KeyboardEvent) =>
                                if event.key == "Enter" then {
                                    val inputElement = event.target.asInstanceOf[dom.html.Input]
                                    val betValue = inputElement.value
                                    sendEvent(Event.PlayerAction(Choice.Raise(betValue.toInt)))
                                }
                            }
                        ),
                        " CHF"
                        ),
                    button(id := "check", onclick:={ () => sendEvent(PlayerAction(Check))})("Check"),
                    button(id := "call", onclick:={ () => sendEvent(PlayerAction(Call))})("Call"),
                    button(id := "fold", onclick:={ () => sendEvent(PlayerAction(Fold))})("Fold")
                    ) 
                )

            case NotPlaying => frag()

            case ChoiceMade(choice) => frag()

            case Winners(players) => frag(
                div(id := "winner")(
                    "Winner: ", players.mkString(", ")
                )
            )
            case IsReady => frag(
                div(cls := "controls")(
                    button(id := "continue", onclick:={ () => sendEvent(Event.Ready)})("Click if you're ready")
                )
            )
            case End(players,balance) => frag(
                div(id := "end")(
                    "Winner: ", players.mkString(", "),
                    "Balance: ", balance
                )
            )

    def renderPlayers(userId: UserId, playersView: PlayersView): Frag = 
        playersView match
            case InGamePlayer(playerIndex, playerBalance, activePlayers, currentPlayer, hand) =>
                frag(
                    renderUserId(userId, playerBalance(userId), hand, activePlayers(userId)),
                    frag(
                        playerIndex.keys.filter(_ != userId).toSeq.map{ user =>
                            renderOpponent(
                                user, 
                                playerIndex(user),
                                playerBalance(user),
                                activePlayers(user),
                                user == currentPlayer, 
                                EmptyHand
                            )
                        }
                    )
                )
                
            case PlayerCardReveal(playerIndex, playerBalance, activePlayers, playerHands) => 
                frag(
                    renderUserId(userId, playerBalance(userId), playerHands(userId), activePlayers(userId)),
                    frag(
                        playerIndex.keys.filter(_ != userId).toSeq.map { user =>
                            renderOpponent(
                                user,
                                playerIndex(user),
                                playerBalance(user),
                                activePlayers(user),
                                false,
                                playerHands(user)
                            )
                        }
                    )
                )
                

    def renderUserId(userId : UserId, balance: Balance, hand: Hands, stillInGame: Boolean): Frag = 
        frag(
            div(cls := "player", id := "current-player")(
                div(cls := "player-name")(userId),
                renderHand(hand, stillInGame),
                div(cls := "balance")(s"Balance : $balance CHF")
            )               
        )

    def renderOpponent(opponent : UserId, idOfPlayer: Int, balance: Balance, stillInGame: Boolean, isCurrentPlayer: Boolean, hand: Hands): Frag =
        val nameOfPlayer = if isCurrentPlayer then (opponent +"💰") else opponent
        frag(
            div(cls := "player", id := s"player$idOfPlayer")(
                div(cls := "player-name")(nameOfPlayer),
                renderHand(hand, stillInGame),
                div(cls := "balance")(s"Balance : $balance CHF")
            ) 
        )

    def renderTable(userId: UserId, tableView: TableView): Frag =
        frag(
            div(cls := "center-table")(renderDealerCards(tableView.dealerCards),
            div(cls := "amount-in-pool")("Amount in the pool: ", tableView.poolBalance," CHF")),
            div(cls := "pot")(span(cls := "money")("💰"))
        )

    def renderHand(hand: Hands, stillInGame: Boolean): Frag =
        if stillInGame then hand match
            case EmptyHand => frag(div(cls := "cards")(CardSymbols.back * 2))
            case Hand(first, second) => frag(div(cls := "cards")(CardSymbols(first) + CardSymbols(second)))    
        else 
            frag()
        
    def renderDealerCards(dealerCards: Vector[Card]): Frag =
        frag(
            div(cls := "deck")(
            span(cls := "cards-on-table")(CardSymbols.back * (5 - dealerCards.size)),
            span(cls := "turned-cards")(dealerCards.map(CardSymbols.apply).mkString)
            )
        )

    override def css: String = super.css + 
        """
            html {
                text-align: center;
                font-family: 'Inknut Antiqua', serif;
                color: black;
                background: #f6f6f6;
            }
            .player > div {
                line-height: 1.2; 
            }

            #name-of-page {
                position : absolute;
                font-size: 3em;
                top : 0%;
                left : 42%;
            }

            body {
                background-color: #e1e1e1;
                margin:1em;
                height:100%;
            }
            .all-table {
                position: absolute;  
                top: 50%;            
                left: 50%;           
                transform: translate(-50%, -50%); 
                display: grid;
                place-items: center;
                width: 50%;
                height: 60%;
                color: white;
                background-color: #bababa;
                border-radius: 50%;
            }
            .center-table {
                position: absolute;
                top:6%;
                display: grid;
                place-items: center;
                line-height:2;
                width: 92%;
                height: 88%;
                background-color: #4c654d;
                border-radius: 50%;
            }
            .deck {
                display:flex;
                flex-direction:column;
                font-size:5.5em;
                line-height:120%;
            }
            .player {
                position: absolute;
                display: flex;
                flex-direction: column;
            }
            .cards {
                font-size:5em;
                margin-bottom: 5%;
            }
            .pot {
                position: absolute;
                top: 50%;
                left: 10%;
                transform: translateY(-50%); 
                font-size: 2em;
                padding: 0.5em;
            }
            #current-player {
                margin-top:80%;
                margin-left:45%;
            }
            #player0 {
                margin-top:30%;
                margin-left:10%;
            }
            #player1 {
                margin-top:5%;
                margin-left:75%;
            }
            #player2 {
                margin-top:60%;
                margin-left:5%	
            }
            #player3 {
                margin-top:50%;
                margin-left:85%;
            }
            .controls {
                position: absolute;
                bottom: 1%;   
                display: flex;
                flex-direction: row;
                align-items: flex-end;  
                width: 60%;
                gap: 2%;
            }
            button {
                padding: 1px 20px;
                background-color: white;
                border: none;
                border-radius: 15px;
                cursor: pointer;
                font-family: 'Inknut Antiqua', serif;
            }
            #continue {
                background-color:black;
                color:white;
                padding: 5px 25px;
            }
            #raise {
                background-color: #4ba652;
                cursor: default;
            }
            #bet{
                padding : 0px;
                border: 1px solid #ccc;
                border-radius: 5px;
            }

            #check {
                background-color: #8e8e8e;
            }
            #call {
                background-color: #954d3f;
            }
            #fold {
                background-color:#f4625c;
            }
        """
