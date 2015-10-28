/**
 * Created by yolo on 26.08.15.
 */

import scala.xml.XML
import scalaj.http._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import Toolbox._


object Main extends App {

  def tokenRequest(login: String, pass: String): HttpResponse[String] = {
    timedGet("https://oauth.vk.com/token").params(Map(
      "grant_type" -> "password",
      "client_id" -> "3697615",
      "client_secret" -> "AlVXZFMUqyrnABp8ncuU",
      "username" -> login,
      "password" -> pass,
      "scope" -> "messages"
    )).asString
  }

  implicit val formats = DefaultFormats
  val config = XML.loadFile("config.xml")
  val pass: String = (config \ "pass").text
  val login: String = (config \ "login").text
  val json = parse(tokenRequest(login, pass).body)
  val token = (json \\ "access_token").extract[String]
  LongPoll.init(token)
}
