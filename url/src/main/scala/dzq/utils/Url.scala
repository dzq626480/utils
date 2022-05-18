package dzq.utils

import java.net.URLDecoder
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

object Url {
  private val urlEncoderStrParttern: Regex = "%[0-9a-fA-F]{2}".r

  def getParams(url:String): Map[String, String] = {
    val urlParams = mutable.HashMap[String, String]()
    url.split(";").foreach{suburl =>
      if (suburl.indexOf("?") >= 0)
      {
        val Array(_, urlQuery) = suburl.split("\\?", 2)
        urlQuery.split("&").foreach { param =>
          val paramKV = param.split("=", 2)
          val paramK = paramKV(0).trim
          val paramV = if (paramKV.length == 2) paramKV(1).trim else ""

          if (paramK.nonEmpty && urlParams.getOrElse(paramK, "").isEmpty) {
            urlParams(paramK) = paramV
          }
        }
      }
    }

    urlParams.toMap
  }

  def urlDecoder(url: String): String = {
    try {
      var decoderUrl = url
      while (urlEncoderStrParttern.findFirstIn(decoderUrl).getOrElse("").nonEmpty) {
        decoderUrl = URLDecoder.decode(decoderUrl.replaceAll("%(?![0-9a-fA-F]{2})", "%25"), "utf8")
      }

      decoderUrl
    } catch {
      case e: Exception =>
        println(s"Url Decode Error: $url\n${e.getCause}\n${e.getMessage}")
        url
    }
  }

  def getPath(pagePath: String): String = {
    val decodePagePath = urlDecoder(pagePath)
    val cgiIdx = decodePagePath.indexOf("?")
    if (cgiIdx < 0) {
      clearPath(pagePath)
    } else {
      clearPath(decodePagePath.slice(0, cgiIdx))
    }
  }

  @tailrec
  def clearPath(path: String): String = {
    val urlLen = path.length
    if (path.slice(0, 1) == "/")
      clearPath(path.slice(1, urlLen))
    else if (path.slice(urlLen-1, urlLen) == "/")
      clearPath(path.slice(0, urlLen - 1))
    else if (path.takeRight(5) == ".html")
      clearPath(path.slice(0, urlLen - 5))
    else if (path.takeRight(4) == ".htm")
      clearPath(path.slice(0, urlLen - 4))
    else {
      path
    }
  }

}
