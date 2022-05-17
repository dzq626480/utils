package dzq.utils

import java.security.MessageDigest

object Hash {
  def SDBMHash(hashStr:String):Long = {
    var hash:Long = 0
    for (char <- hashStr) {
      hash = char + (hash << 6) + (hash << 16) - hash
    }
    hash
  }

  def md5(str: String): String = {
    MessageDigest.getInstance("MD5").digest(str.getBytes)
      .map(0xFF & _)
      .map { "%02x".format(_) }
      .foldLeft(""){_ + _}
  }

}
