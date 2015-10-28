import scalaj.http._

/**
 * Created by yolo on 03.09.15.
 */
object Toolbox {
  val timeout = 30000

  def timedGet(url: String): HttpRequest = {
    Http(url).option(HttpOptions.connTimeout(30000)).option(HttpOptions.readTimeout(30000))
  }
}
