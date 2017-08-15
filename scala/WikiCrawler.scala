import scala.io.Source
import rx.lang.scala.Observable

object WikiCrawler extends App {

  // crawl a given list of topics from wikipedia

  def crawl(subjects: List[String]): Observable[String] = {

    Observable(subscriber => {

      for (sub <- subjects) {
        if (!subscriber.isUnsubscribed) {
          val url  = s"https://en.wikipedia.org/wiki/$sub"
          val html = Source.fromURL(url).mkString
          subscriber.onNext(html)
        }
      }

      if (!subscriber.isUnsubscribed) {
        subscriber.onCompleted()
        println("finished")
      }

    })

  }

  val subjects = List("Ruby_(programming_language)", "Python_(programming_language)")
  crawl(subjects).subscribe(println(_))

}