package go.js

import japgolly.scalajs.react._

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom.document
import japgolly.scalajs.react.vdom.prefix_<^._

case class State(elapsed: Long)

class Backend($: BackendScope[_, State]) {

  var interval: js.UndefOr[js.timers.SetIntervalHandle] =
    js.undefined

  def tick() =
    $.modState(s => State(s.elapsed + 1))

  def start() =
    interval = js.timers.setInterval(1000)(tick())

}

object TutorialApp extends JSApp {
  val HelloMessage = ReactComponentB[String]("HelloMessage")
    .render(name => <.div("Hello ", name))
    .build

  val Timer = ReactComponentB[Unit]("Timer")
    .initialState(State(0))
    .backend(b => new Backend(b))
    .render($ => <.div("Elapsed: ", $.state.elapsed))
    .componentDidMount(_.backend.start())
    .componentWillMount(_.backend.interval foreach(js.timers.clearInterval))
    .build


  def main(): Unit = {
    React.render(Timer(()), document.body)
    println("Hello World!")
  }
}
