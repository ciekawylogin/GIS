package gis

import java.io.InputStream
import scala.util.Random

/**
  * Created by michal on 5/20/16.
  */
object NameGenerator {
  lazy val firstNames: Seq[String] = {
    val stream: InputStream = getClass.getResourceAsStream("/first_names.txt")
    scala.io.Source.fromInputStream(stream).getLines.toList
  }

  lazy val lastNames: Seq[String] = {
    val stream: InputStream = getClass.getResourceAsStream("/last_names.txt")
    scala.io.Source.fromInputStream(stream).getLines.toList
  }

  def generateName() = {
    s"${Random.shuffle(firstNames).head} ${Random.shuffle(lastNames).head}"
  }
}
