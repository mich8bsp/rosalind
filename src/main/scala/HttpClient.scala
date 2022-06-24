import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

object HttpClient {

  def get(protocol: String = "https",
          url: String,
          path: String)(implicit ec: ExecutionContext): Future[List[String]] = Future {
    val source = Source.fromURL(s"$protocol://$url$path")
    try {
      source.getLines()
        .toList
    } finally {
      source.close()
    }
  }
}