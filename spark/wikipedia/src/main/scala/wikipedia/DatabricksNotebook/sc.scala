import org.apache.spark.rdd.RDD

case class WikipediaArticle(title: String, text: String) {
  def mentionsLanguage(lang: String): Boolean = text.split(' ').contains(lang)
}

val filePath = "/FileStore/tables/btvckc731494048063761/wikipedia.dat"

def parse(contentLine: String) = {
  val subs = "</title><text>"
  val i = contentLine.indexOf(subs)
  val title = contentLine.substring(14, i)
  val text  = contentLine.substring(i + subs.length, contentLine.length-16)
  WikipediaArticle(title, text)
}

val wikiRdd: RDD[WikipediaArticle] =
  sc.textFile(filePath)
    .map(parse)
    .cache

val langs = List(
  "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
  "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

def occurrencesOfLang(lang: String, rdd: RDD[WikipediaArticle]): Int =
  rdd
    .aggregate(0)(
      (acc, article) =>
        if (article mentionsLanguage lang) acc + 1 else acc,
      _ + _
    )

def rankLangs(langs: List[String], rdd: RDD[WikipediaArticle]): List[(String, Int)] =
  langs
    .map {
      lang =>
        (lang, occurrencesOfLang(lang, rdd))
    }
    .sortBy(-_._2)

def makeIndex(langs: List[String], rdd: RDD[WikipediaArticle]): RDD[(String, Iterable[WikipediaArticle])] =
  rdd
    .flatMap {
      article =>
        langs
          .filter(article.mentionsLanguage)
          .map((_, article))
    }
    .groupByKey

def rankLangsUsingIndex(index: RDD[(String, Iterable[WikipediaArticle])]): List[(String, Int)] =
  index
    .mapValues(_.size)
    .sortBy(-_._2)
    .collect
    .toList

def rankLangsReduceByKey(langs: List[String], rdd: RDD[WikipediaArticle]): List[(String, Int)] =
  rdd
    .flatMap {
      article =>
        langs
          .filter(article.mentionsLanguage)
          .map((_, 1))
    }
    .reduceByKey(_ + _)
    .sortBy(-_._2)
    .collect
    .toList

val timing = new StringBuffer

def timed[T](label: String, code: => T): T = {
  val start = System.currentTimeMillis()
  val result = code
  val stop = System.currentTimeMillis()
  timing.append(s"Processing $label took ${stop - start} ms.\n")
  result
}

def run() = {
  val langsRanked: List[(String, Int)] = timed("Part 1: naive ranking", rankLangs(langs, wikiRdd))

  def index: RDD[(String, Iterable[WikipediaArticle])] = makeIndex(langs, wikiRdd)

  val langsRanked2: List[(String, Int)] = timed("Part 2: ranking using inverted index", rankLangsUsingIndex(index))

  val langsRanked3: List[(String, Int)] = timed("Part 3: ranking using reduceByKey", rankLangsReduceByKey(langs, wikiRdd))

  println(timing)

  println("1 results:")
  langsRanked.foreach{case((name, mentions)) => println(s"$name, $mentions")}

  println("2 results:")
  langsRanked2.foreach{case((name, mentions)) => println(s"$name, $mentions")}

  println("3 results:")
  langsRanked3.foreach{case((name, mentions)) => println(s"$name, $mentions")}
}

run()

/*** OUTPUT:
Processing Part 1: naive ranking took 129442 ms.
Processing Part 2: ranking using inverted index took 56857 ms.
Processing Part 3: ranking using reduceByKey took 49821 ms.

1 results:
  JavaScript, 1692
C#, 705
Java, 586
CSS, 372
C++, 334
MATLAB, 295
Python, 286
PHP, 279
Perl, 144
Ruby, 120
Haskell, 54
Objective-C, 47
Scala, 43
Clojure, 26
Groovy, 23
2 results:
  JavaScript, 1692
C#, 705
Java, 586
CSS, 372
C++, 334
MATLAB, 295
Python, 286
PHP, 279
Perl, 144
Ruby, 120
Haskell, 54
Objective-C, 47
Scala, 43
Clojure, 26
Groovy, 23
2 results:
  JavaScript, 1692
C#, 705
Java, 586
CSS, 372
C++, 334
MATLAB, 295
Python, 286
PHP, 279
Perl, 144
Ruby, 120
Haskell, 54
Objective-C, 47
Scala, 43
Clojure, 26
Groovy, 23
*/