package dzq.utils

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

object SDateTime {
  /**
   * 获取当前日期之前或之后的日期
   * @param days 正数是之后的天数，负数是之前的天数
   * @return 日期类型
   */
  def dateAddDays(days: Int): Calendar = {
    val curTime = Calendar.getInstance()
    curTime.add(Calendar.DAY_OF_MONTH, days)
    curTime
  }

  /**
   * 获取当前日期之前或之后的日期
   * @param days 正数是之后的天数，负数是之前的天数
   * @param dateFormat 日期格式化字符串
   * @return 时间字符串
   */
  def dateAddDaysToStr(days: Int, dateFormat:String="yyyyMMdd"): String = {
    val dateFmt = new SimpleDateFormat(dateFormat)
    dateFmt.format(dateAddDays(days).getTime)
  }

  /**
   * 当前日期之前或之后的时间(毫秒)
   * @param days 正数是之后的天数，负数是之前的天数
   * @return 时间毫秒
   */
  def dateAddDaysToMillis(days: Int): Long = {
    dateAddDays(days).getTimeInMillis
  }

  /**
   * n天之后的凌晨零点时间
   * @param days 正数是之后的天数，负数是之前的天数
   * @return 毫秒
   */
  def getTimeAtZero(days: Int): Long = {
    val dateStr = dateAddDaysToStr(days)

    val dateTime = Calendar.getInstance
    dateTime.set(dateStr.substring(0,4).toInt, dateStr.substring(4,6).toInt-1, dateStr.substring(6,8).toInt, 0, 0, 0)
    dateTime.getTimeInMillis
  }

  /**
   * 当前格式化的日期
   * @param dateFormat 日期格式
   * @return 时间字符串
   */
  def curDateTimeStr(dateFormat:String="yyyy-MM-dd HH:mm:ss"): String = {
    val sdf = new SimpleDateFormat(dateFormat)
    val now = new Date() // or Calendar.getInstance
    sdf.format(now)
  }

  def curTimeMillis: Long = System.currentTimeMillis()

  /**
   * 从给定的日期，计算之前或之后的日期
   * @param fromDateStr 起始日期
   * @param days 正数是之后的天数，负数是之前的天数
   * @param dateFormat 日期的格式
   * @return 时间字符串
   */
  def getPreDate(fromDateStr:String, days:Int = -1, dateFormat:String="yyyyMMdd"): String = {
    val dateFmt = new SimpleDateFormat(dateFormat)
    val calendar = Calendar.getInstance

    calendar.setTime(dateFmt.parse(fromDateStr))
    calendar.add(Calendar.DAY_OF_MONTH, days)
    dateFmt.format(calendar.getTime)
  }

  def millisToDateTime(timestamp:Long, dateTimeFmt:String="yyyy-MM-dd HH:mm:ss"): String = {
    val calendar = Calendar.getInstance
    calendar.setTimeInMillis(timestamp)
    val dateFmt = new SimpleDateFormat(dateTimeFmt)
    dateFmt.format(calendar.getTime)
  }

  def dateTimeToMillis(datetime:String, dateTimeFmt:String="yyyy-MM-dd HH:mm:ss"): Long = {
    val dateFmt = new SimpleDateFormat(dateTimeFmt)
    dateFmt.parse(datetime).getTime
  }

  /**
   * 计算起始日期至最后日期之间的天数
   * @param startDate 起始日期
   * @param endDate 终止日期
   * @param dateTimeFmt 时间格式
   * @return 天数
   */
  def dayCount(startDate:String, endDate:String, dateTimeFmt:String="yyyy-MM-dd HH:mm:ss"): Int = {
    val startTime = dateTimeToMillis(dateTimeFmt, startDate)
    val endTime   = dateTimeToMillis(dateTimeFmt, endDate)

    ((endTime - startTime) / (24*3600*1000)) .toInt
  }

  /**
   * 起始日期增加或减少n分钟后的时间
   * @param beginDateTime 起始日期
   * @param amount 分钟
   * @param dateTimeFmt 日期格式
   * @return 时间字符串
   */
  def addMinutes(beginDateTime:String, amount:Int, dateTimeFmt:String="yyyy-MM-dd HH:mm:ss"): String = {
    val calendar = Calendar.getInstance
    val dateFmt  = new SimpleDateFormat(dateTimeFmt)
    calendar.setTime(dateFmt.parse(beginDateTime))
    calendar.add(Calendar.MINUTE, amount)
    dateFmt.format(calendar.getTime)
  }

  /**
   * 两时间内的时间秒
   * @param beginDateTime 起始日期
   * @param endDateTime 终止日期
   * @param dateTimeFmt 时间格式
   * @return 时间秒
   */
  def passSeconds(beginDateTime:String, endDateTime:String, dateTimeFmt:String="yyyy-MM-dd HH:mm:ss"):Int = {
    val beginTime = dateTimeToMillis(dateTimeFmt, beginDateTime)
    val endTime   = dateTimeToMillis(dateTimeFmt, endDateTime)

    val passTime = (endTime - beginTime) / 1000
    passTime.toInt
  }

  /**
   * 时间取模，获取间隔时间内的起始时间和终止时间
   * @param strDateTime 格式化的时间字符串
   * @param numMod 时间间隔(秒)
   * @param dateTimeFmt 时间格式
   * @return (时间间隔的起始时间, 时间间隔的终止时间)
   */
  def DateTimeMod(strDateTime:String, numMod:Int, dateTimeFmt:String = "yyyyMMddHHmm"): (String, String) = { // numMod is seconds
    val times = dateTimeToMillis(dateTimeFmt, strDateTime) / 1000L
    val mod   = times % numMod
    val beginTimes = times - mod
    val endTimes   = beginTimes + numMod

    val strBeginDateTime = millisToDateTime( beginTimes * 1000, dateTimeFmt)
    val strEndDateTime   = millisToDateTime(endTimes * 1000, dateTimeFmt)
    (strBeginDateTime, strEndDateTime)
  }

  /**
   * 重新格式化字符串
   * @param srcDateStr 需重新格式的日期
   * @param srcFmt 需重新格式的日期格式
   * @param dstFmt 你需要的日期格式
   * @return
   */
  def formatTrans(srcDateStr:String, srcFmt:String="yyyyMMddHHmmss", dstFmt:String="yyyy-MM-dd HH:mm:ss"):String = {
    val times = dateTimeToMillis(srcFmt, srcDateStr)
    millisToDateTime(times, dstFmt)
  }

  /**
   * 获取日期的星期
   * @param strDate 日期
   * @param dateFormat 日期格式
   * @return 星期几(中国习惯)
   */
  def dayOfWeek(strDate:String, dateFormat:String = "yyyy-MM-dd HH:mm:ss"):Int = {
    val calendar = Calendar.getInstance
    val dateFmt  = new SimpleDateFormat(dateFormat)
    calendar.setTime(dateFmt.parse(strDate))
    val ret = calendar.get(Calendar.DAY_OF_WEEK) - 1
    if (ret == 0) 7 else ret
  }

  /**
   * 获取日期中的星期一时间
   * @param strDate 日期
   * @param dateFormat 日期格式
   * @return 星期一时间
   */
  def mondayDate(strDate:String, dateFormat:String = "yyyy-MM-dd HH:mm:ss"):String={
    val calendar = Calendar.getInstance()
    val dateFmt  = new SimpleDateFormat(dateFormat)
    calendar.setTime(dateFmt.parse(strDate))
    calendar.set(Calendar.DAY_OF_WEEK, Calendar.MONDAY)
    dateFmt.format(calendar.getTime)
  }

  /**
   * 获取日期中的星期天时间
   * @param strDate 日期
   * @param dateFormat 日期格式
   * @return 星期天时间(中国习惯)
   */
  def sundayDate(strDate:String, dateFormat:String = "yyyy-MM-dd HH:mm:ss"):String={
    val calendar = Calendar.getInstance()
    val dateFmt  = new SimpleDateFormat(dateFormat)
    calendar.setTime(dateFmt.parse(strDate))
    calendar.set(Calendar.DAY_OF_WEEK, Calendar.SUNDAY)
    calendar.add(Calendar.WEEK_OF_YEAR, 1) // add one day for china date
    dateFmt.format(calendar.getTime)
  }
}
