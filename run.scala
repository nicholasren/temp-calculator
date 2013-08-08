import scala.io.Source
import scala.math._
import java.io._

object Main {
  def main(args: Array[String]):Unit = {

    List(
      ("data/lanzhou_forecast.txt", "兰州预报准确率"),
      ("data/cctv_forecast.txt", "CCTV预报准确率"),
      ("data/germany_forecast.txt", "德国预报准确率")).foreach {
      pair => {
        generate(pair._1, pair._2)
      }
    }
  }

  def generate(filePath: String, title: String) {
    //准确数据缺少2010-02-29
    val realHigh = loadRealData("data/real_high.txt")
    val realLow = loadRealData("data/real_low.txt")

    val forecasts = loadMonthlyForecasts(filePath)
  
    val monthlyResult: List[RangelyForecastResult] = forecasts.map(_.result(realHigh, realLow))
    val seasonalResult: List[RangelyForecastResult] = generateSeasonalResult(monthlyResult)
  
    val monthlyRate = monthlyResult.map(_.rate)
    val seasonalRate = seasonalResult.map(_.rate)
    export(monthlyRate, title + "-月.csv")
    export(seasonalRate, title + "-季度.csv")
  }
  
  def export(rates: Seq[ForecastRate], fileName: String) {
    val builder = new StringBuilder
    builder.append("Year,Range(Month/Season),24h High, 24h Low,48h High, 48h Low, 72h High, 72h Low").append("\n")
    rates.foreach {
      rate => {
        builder.append(rate).append("\n")
      }
    }
  
    val writer = new FileWriter(new File(fileName))
    writer.write(builder.toString())
    writer.close()
  }
  
  def loadRealData(filePath: String) = {
    Source.fromFile(new File(filePath)).getLines().toList
      .map(line => line.trim.split("\\s+").toList)
      .map(items => ((items(0) concat items(1)), items.drop(2).map(toFloat(_, 10))))
      .groupBy(_._1)
      .map(x => (x._1, x._2(0)._2))
  }
  
  def loadMonthlyForecasts(filePath: String): List[MonthlyForecast] = {
    Source.fromFile(new File(filePath)).getLines().toList
      .map(line => line.split("\\s+").toList)
      .groupBy(_(0))
      .map {
      monthly => {
        val f24s = findForecastByHoursBefore(monthly, 24)
        val f48s = findForecastByHoursBefore(monthly, 48)
        val f72s = findForecastByHoursBefore(monthly, 72)
  
        toMonthlyForecast(f24s, f48s, f72s, monthly._1)
      }
    }.toList.sortBy(x => x.year + x.month)
  }
  
  def generateSeasonalResult(monthlyResult: List[RangelyForecastResult]): List[RangelyForecastResult] = {
    monthlyResult.grouped(3).map {
      months => {
        val year = months(0).year
        val season = "%02d" format (months(0).range.toInt / 3) + 1
        val dailyResult = months.map(_.dailyResults).flatten
        new RangelyForecastResult(dailyResult, season, year)
      }
    }.toList
  }
  
  def findForecastByHoursBefore(monthly: (String, List[List[String]]), hoursBefore: Int): List[Forecast] = {
    monthly._2.find(_(1) == hoursBefore.toString).get.drop(2).grouped(2).toList.map(toForecast(_, hoursBefore))
  }
  
  def toMonthlyForecast(f24s: List[Forecast], f48s: List[Forecast], f72s: List[Forecast], yearAndMonth: String): MonthlyForecast = {
    val days = f24s.zipWithIndex.map {
      f24WithIdx => {
        val idx = f24WithIdx._2
        new DailyForecast(f24WithIdx._1, f48s(idx), f72s(idx), idx)
      }
    }
    new MonthlyForecast(days, yearAndMonth.substring(4), yearAndMonth.substring(0, 4))
  }
  
  def toFloat(x: String, ratio: Int): Float = {
    x.toFloat / ratio
  }
  
  def toForecast(x: List[String], hoursBefore: Int): Forecast = {
    new Forecast(toFloat(x(0), 1), toFloat(x(1), 1), hoursBefore)
  }
}



class MonthlyForecast(val dailyForecasts: List[DailyForecast], val month: String, val year: String) {
  override def toString = "{Month: %s%s, %s]".format(year, month, dailyForecasts)

  def result(realHigh: Map[String, List[Float]], realLow: Map[String, List[Float]]): RangelyForecastResult = {

    val monthAndYear = year.concat(month)
    if (realHigh.contains(monthAndYear)) {
      val realMonthHigh = realHigh(monthAndYear)
      val realMonthLow = realLow(monthAndYear)
      val dailyResults = dailyForecasts.map(_.result(realMonthHigh, realMonthLow))
      new RangelyForecastResult(dailyResults, month, year)
    }
    else {
      new RangelyForecastResult(List(), month, year)
    }

  }
}

class DailyForecast(val f24: Forecast, val f48: Forecast, val f72: Forecast, val day: Int) {
  override def toString = "[Daily %s : %s, %s, %s]".format(day, f24, f48, f72)

  def result(realMonthlyHigh: List[Float], realMonthlyLow: List[Float]) = {

    val realLow: Float = realMonthlyLow(day)
    val realHigh: Float = realMonthlyHigh(day)
    new DailyForecastResult(f24.result(realLow, realHigh), f48.result(realLow, realHigh), f72.result(realLow, realHigh))
  }
}


class Forecast(val low: Float, val high: Float, val hoursBefore: Int) {
  override def toString = "(%sh forecast: h: %s, l: %s)".format(hoursBefore, high, low)

  def result(realLow: Float, realHigh: Float) = new ForecastResult(abs(realLow - low) < 2, abs(realHigh - high) < 2, hoursBefore)
}

class ForecastResult(val low: Boolean, val high: Boolean, val hoursBefore: Int) {
  override def toString = "(%sh: l: %s, h: %s)".format(hoursBefore, high, low)
}

class DailyForecastResult(val f24: ForecastResult, val f48: ForecastResult, val f72: ForecastResult) {
  override def toString = "[Daily: %s, %s, %s]".format(f24, f48, f72)
}

class RangelyForecastResult(val dailyResults: List[DailyForecastResult], val range: String, val year: String) {
  override def toString = "[Monthly: %s, %s]".format(range, dailyResults)

  def rate = {
    val days = dailyResults.size
    val f24HighRight = dailyResults.map(_.f24).count(_.high).toFloat
    val f24LowRight = dailyResults.map(_.f24).count(_.low).toFloat
    val f48HighRight = dailyResults.map(_.f48).count(_.high).toFloat
    val f48LowRight = dailyResults.map(_.f48).count(_.low).toFloat
    val f72HighRight = dailyResults.map(_.f72).count(_.high).toFloat
    val f72LowRight = dailyResults.map(_.f72).count(_.low).toFloat

    new ForecastRate(year, range, f24HighRight / days, f24LowRight / days, f48HighRight / days, f48LowRight / days, f72HighRight / days, f72LowRight / days)
  }
}

object NoneMonthlyForecastResult extends RangelyForecastResult(List(), "none", "none")

class ForecastRate(val year: String, val timeRange: String, h24: Float, l24: Float, h48: Float, l48: Float, h72: Float, l72: Float) {
  override def toString = "%s, %s, %s, %s, %s, %s, %s, %s".format(year, timeRange, h24 * 100, l24 * 100, h48 * 100, l48 * 100, h72 * 100, l72 * 100)
}
