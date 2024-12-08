package apps
package app69

import apps.*
import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.{WebClientAppInstance}
import org.scalajs.dom
import scalatags.JsDom.all._
import apps.CardView.*
import apps.PhaseView.*

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("app69")
object UI extends WSClientApp:

    def uiId: UIId = "html"

    def appId: String = "app69"
    
    def init(userId: UserId, sendMessage: ujson.Value => Unit, target: dom.Element): ClientAppInstance =
        Instance(userId, sendMessage, target)

class Instance(userId: UserId, sendMessage: ujson.Value => Unit, target: dom.Element)
  extends WebClientAppInstance[Event, View](userId, sendMessage, target):
    override val wire: AppWire[Event, View] = Wire

    override def render(userId: UserId, view: View): Frag =
        val View(phaseView,scoresView,cardView) = view
        frag(
            head(
                link(
                    href := "https://fonts.googleapis.com/css2?family=Inknut+Antiqua&display=swap",
                    rel := "stylesheet"
                )
            ),
        phaseView match
            case ChoiceSelection => InGameCardsUIrender(false, userId, scoresView, cardView)
            case ChoiceMade(choice) => InGameCardsUIrender(false, userId, scoresView, cardView)
            case Winner => InGameCardsUIrender(true, userId, scoresView, cardView)
        )
        
    def InGameCardsUIrender(endOfTurn: Boolean, userId: UserId, scoresView: ScoresView, cardView : CardView): Frag =
        val playerCards: Map[String,String] = cardView match
            case InGameCards(playerCards, _) => Map(userId -> (CardSymbols.apply(playerCards.first) + CardSymbols.apply(playerCards.second)))
            case RevealCards(playerCards, _) => playerCards.map((userId, hand) => userId -> (CardSymbols.apply(hand.first) + CardSymbols.apply(hand.second)))
        val handOfDealer = cardView match
            case InGameCards(_, dealerCards) => dealerCards.map(CardSymbols.apply).mkString
            case RevealCards(_, dealerCards) => dealerCards.map(CardSymbols.apply).mkString
    
        val poolBalance = scoresView.poolBalance
        val playersScores = scoresView.playerScores
        val players = scoresView.playerScores.keys.toList.filter(_ != userId)
        val playersInGame = (0 to 4).map(i => players.lift(i).getOrElse("")).toList
        val winner = scoresView.playerScores.maxBy(_._2)._1
        val winnerBalance = scoresView.playerScores(winner)

        if (endOfTurn) then {
            frag(
            div(cls := "winner-notification")(
            h2("Game Over"),
            p(
            "The winner is ",
            b(winner),
            " with a balance of ",
            span(cls := "balance")(s"$winnerBalance CHF"),
            "."
            )))
        } else {
        frag(
                body(
                    div(cls := "game-container")(
                        div(cls := "game-table")(
                        div(cls := "all-table")(
                            div(cls := "center-table")(
                            div(cls := "deck")(
                                span(cls := "turned-cards")("🂠" * (5 - handOfDealer.length())),
                                span(cls := "cards-on-table")(handOfDealer)
                            ),
                            div(cls := "amount-in-pool")("Amount in the pool:", poolBalance)),
                            div(cls := "pot")(span(cls := "money")("💰"))
                            )
                        ),
                        
                        div(cls := "player", id := "player1")(
                            div(cls := "player-name")(playersInGame(0)),
                            div(cls := "cards")(if playersInGame(0)=="" then "" else if (endOfTurn) then playerCards(playersInGame(0)) else"🂠🂠"),
                            div(cls := "balance")("Balance :", if playersInGame(0)=="" then "" else playersScores(playersInGame(0)))
                        ),
                        div(cls := "player", id := "player2")(
                            div(cls := "player-name")(playersInGame(1)),
                            div(cls := "cards")(if playersInGame(1)=="" then "" else if (endOfTurn) then playerCards(playersInGame(1)) else"🂠🂠"),
                            div(cls := "balance")("Balance :", if playersInGame(1)=="" then "" else playersScores(playersInGame(1)))
                        ),
                        div(cls := "player", id := "player3")(
                            div(cls := "player-name")(userId),
                            div(cls := "cards")(playerCards.getOrElse(userId, "")),
                            div(cls := "balance")("Balance :", playersScores(userId))
                        ),
                        div(cls := "player", id := "player4")(
                            div(cls := "player-name")(playersInGame(2)),
                            div(cls := "cards")(if playersInGame(2)=="" then "" else if (endOfTurn) then playerCards(playersInGame(2)) else"🂠🂠"),
                            div(cls := "balance")("Balance :", if playersInGame(2)=="" then "" else playersScores(playersInGame(2)))
                        ),
                        div(cls := "player", id := "player5")(
                            div(cls := "player-name")(playersInGame(3)),
                            div(cls := "cards")(if playersInGame(3)=="" then "" else if (endOfTurn) then playerCards(playersInGame(3)) else"🂠🂠"),
                            div(cls := "balance")("Balance :", if playersInGame(3)=="" then "" else playersScores(playersInGame(3)))
                        )
                    ),
                    div(cls := "controls")(
                    button(id := "raise")(
                        "Raise: ",
                        input(
                            `type` := "text",
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
                    button(id := "check", onclick:={ () => sendEvent(Event.PlayerAction(Choice.Check))})("Check"),
                    button(id := "call", onclick:={ () => sendEvent(Event.PlayerAction(Choice.Call))})("Call"),
                    button(id := "fold",onclick:={ () => sendEvent(Event.PlayerAction(Choice.Fold))})("Fold")
                )  
            )
        )   
    }   

    override def css: String = super.css + """
    html{
        text-align: center;
        font-family: 'Inknut Antiqua', serif;
        color: black;
        background: #f6f6f6;
    }

    .player > div {
        line-height: 1.2; 
    }

    body{
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

    .center-table{
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

    .deck{
        display:flex;
        flex-direction:column;
        font-size:4.5em;
        line-height:120%;
    }

    .player{
        position: absolute;
        display: flex;
        flex-direction: column;
    }

    .cards{
        font-size:3em;
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

    #player1{
        top:30%;
        left:10%;
    }
    #player2{
        top:5%;
        left:75%;
    }
    #player3{
        top:85%;
        left:45%;
    }
    #player4{
        top:60%;
        left:5%	
    }
    #player5{
        top:50%;
        left:85%;
    }

    .controls{
        position: absolute;
        bottom: 1%; 
        left: 5%;   
        display: flex;
        flex-direction: row;
        align-items: flex-end;  
        width: 60%;
        gap: 2%;
    }

    button{
        padding: 1px 20px;
        background-color: white;
        border: none;
        border-radius: 15px;
        cursor: pointer;
        font-family: 'Inknut Antiqua', serif;
    }

    #raise{
        background-color: #4ba652;
        cursor: default;
    }
    #check{
        background-color: #8e8e8e;
    }
    #call{
        background-color: #954d3f;
    }
    #fold{
        background-color:#f4625c;
    }
    """
    