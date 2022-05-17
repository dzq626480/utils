package dzq.utils

import scala.collection.mutable

class ArgumentParse(args: Array[String]) {
  val argsMap: mutable.HashMap[String, String] = new mutable.HashMap[String, String]()

  args.foreach { arg =>
    val arr = arg.split("=", 2)
    if (arr.size > 1) {
      argsMap.put(arr(0).trim, arr(1).trim)
    } else {
      argsMap.put(arr(0).trim, "")
    }
  }

  def getString(argName: String, defaultVal: String = ""): String = {
    argsMap.getOrElse(argName, defaultVal)
  }

  def getInt(argName: String, defaultVal: Int = 0): Int = {
    val v = argsMap.get(argName)
    if (v.isDefined) v.get.toInt else defaultVal
  }

  def getLong(argName:String, defaultVal:Long = 0):Long = {
    val v = argsMap.get(argName)
    if (v.isDefined) v.get.toLong else defaultVal
  }

  def getDouble(argName: String, defaultVal: Double = 0.0d): Double = {
    val v = argsMap.get(argName)
    if (v.isDefined) v.get.toDouble else defaultVal
  }

  def getFloat(argName: String, defaultVal: Float = 0.0f): Float = {
    val v = argsMap.get(argName)
    if (v.isDefined) v.get.toFloat else defaultVal
  }

  def getArray(argName: String, splitStr:String = ",", defaultVal: Array[String] = Array()): Array[String] = {
    val v = argsMap.get(argName)
    if (v.isDefined) v.get.split(splitStr) else defaultVal
  }

  def getBoolean(argName:String, defaultVal:Boolean = false):Boolean = {
    val v = argsMap.get(argName)
    if (v.isDefined) {
      v.get.toLowerCase match {
        case "true" => true
        case "false" => false
        case "yes" => true
        case "no" => false
        case "on" => true
        case "off" => false
        case _ => defaultVal
      }
    } else defaultVal
  }

  def add(argName:String, argValue:String): Unit = {
    argsMap.put(argName, argValue)
  }

  def remove(argName:String): Unit = {
    argsMap.remove(argName)
  }

}
