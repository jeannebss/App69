package apps
package app69

import apps.*
import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.{WebClientAppInstance}
import org.scalajs.dom
import scalatags.JsDom.all._

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


    override def css: String = super.css + """
    html {
        font-family: 'Inknut Antiqua', serif;
        background: #f6f6f6;
    }
    """

    override def render(userId: UserId, view: View): Frag = 
        val View(phaseView,scoresView,cardView) = view
        val handOfDealer = cardView.dealerCards.map(_.toString).mkString
        val playerCards = cardView.playerCards.first.toString + cardView.playerCards.second.toString
        frag(
            html(
                head(
                    link(
                        href := "https://fonts.googleapis.com/css2?family=Inknut+Antiqua&display=swap",
                        rel := "stylesheet"
                    )
                ),
                body(
                    div(cls := "game-container")(
                        div(cls := "game-table")(
                        div(cls := "space-before-table")(),
                        div(cls := "all-table")(
                            div(cls := "center-table")(
                            div(cls := "deck")(
                                span(cls := "turned-cards")("🂠" * (5 - cardView.dealerCards.size)),
                                span(cls := "cards-on-table")(handOfDealer)
                            ),
                            div(cls := "amount-in-pool")("Amount in the pool:", scoresView.poolBalance),
                            div(cls := "pot")(span(cls := "money")("💰"))
                            )
                        ),
                        div(cls := "player", id := "player1")(
                            div(cls := "player-name")("Jeanne"),
                            div(cls := "cards")("🂠🂠"),
                            div(cls := "balance")("Balance : 3")
                        ),
                        div(cls := "player", id := "player2")(
                            div(cls := "player-name")("Louis"),
                            div(cls := "cards")("🂠🂠"),
                            div(cls := "balance")("Balance : 3")
                        ),
                        div(cls := "player", id := "player3")(
                            div(cls := "player-name")("Jakub"),
                            div(cls := "cards")("🂠🂠"),
                            div(cls := "balance")("Balance : 3")
                        ),
                        div(cls := "player", id := "player4")(
                            div(cls := "player-name")(userId),
                            div(cls := "cards")(playerCards),
                            div(cls := "balance")("Balance : 3")
                        ),
                        div(cls := "player", id := "player5")(
                            div(cls := "player-name")("Alexis"),
                            div(cls := "cards")("🂠🂠"),
                            div(cls := "balance")("Balance : 3")
                        ),
                        div(cls := "player", id := "player6")(
                            div(cls := "player-name")("Mei"),
                            div(cls := "cards")("🂠🂠"),
                            div(cls := "balance")("Balance : 3")
                        )
                        ),
                        div(cls := "controls")(
                        button(cls := "raise")("Raise :"),
                        button(cls := "check-call")("Check/Call"),
                        button(cls := "fold")("Fold")
                        )
                    )
                )
            )
        )
    