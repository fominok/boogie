import akka.actor.Actor
import akka.actor.Actor.Receive
import Toolbox._

sealed abstract class ApiRequest

case class SendMessage(id: String, msg: String) extends ApiRequest

class ApiActor(val token: String) extends Actor {
  override def receive: Receive = {
    case SendMessage(id, msg) => {
      val pair: (String, String) = if (id.length == 10 && id.dropRight(8) == "20")
        ("chat_id", id.drop(1))
        else ("user_id", id) //Глупому формату ответов LongPoll - глупые костыли
      val response = timedGet("https://api.vk.com/method/messages.send").params(Map(
          pair,
          "message" -> msg,
          "access_token" -> token
        )).asString.body
      println(response)
    }
    case _ =>
  }
}
