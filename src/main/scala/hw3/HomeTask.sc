import java.time.LocalDate.now

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.select.Elements

case class Date(day: Int, month: Int, year: Int)

class DayMonth(day: Int, month: Int) {
  def --(year: Int): Date = new Date(day, month, year)
}

implicit class IntExt3(i2: Int) {
  def --(i: Int): DayMonth = new DayMonth(i2, i)
}

object Date {
  def today : Date = {
    new Date(now.getDayOfMonth, now.getMonthValue, now.getYear)
  }
}

trait Currency {
  def getCode : String
}

object usd extends Currency {
  override def getCode: String = "usd"
}

object eur extends Currency {
  override def getCode: String = "eur"
}

object rub extends Currency {
  override def getCode: String = "rub"
}

object jpy extends Currency {
  override def getCode: String = "jpy"
}

case class AmountHolder(amount: Double, code: String)

case class Conversion(from: String, to: String, amount: Double, date: Date = Date.today) {
  def convert: Double = {
    var res : Double = 0
    if (to == "rub") {
      res = amount * getRateAgainstRub(from)
    } else if (from == "rub") {
      res = amount / getRateAgainstRub(to)
    } else {
      res = amount * getRateAgainstRub(from) / getRateAgainstRub(to)
    }
    res
  }

  private def getRateAgainstRub(currency: String) : Double = {
    val document: Document = Jsoup.connect(s"http://www.cbr.ru/eng/currency_base/daily.aspx?date_req=${date.day}.${date.month}.${date.year}").get()
    try {
      val table = document.select("table").get(0)
      val rows = table.select("tr")
      var res : Double = 0

      for (i <- 1 until rows.size()) {
        val row = rows.get(i)
        val tds : Elements = row.select("td")
        val unit : Int = tds.get(2).text.toInt
        val code : String = tds.get(1).text.toLowerCase()
        if (code == currency) res = tds.get(4).text.toDouble / unit
      }

      if (res == 0) throw new IllegalArgumentException("Unknown currency code")

      res
    } catch {
      case e: IndexOutOfBoundsException => throw new IllegalArgumentException("Wrong date")
    }
  }
}

object Conversion {
  implicit def toDouble(conversion: Conversion): Double = conversion.convert
}

implicit class CurrencyExt(from: Currency) {
  def to(target: Currency) = Conversion(from.getCode, target.getCode, 1.0)
}

implicit class ConversionExt(conversion: Conversion) {
  def on(date: Date) = new Conversion(conversion.from, conversion.to, conversion.amount, date)
}

implicit class CurrencyHolderExt(from: AmountHolder) {
  def to(target: Currency) = new Conversion(from.code, target.getCode, from.amount)
}

implicit class DoubleExt(amount: Double) {
  def usd = AmountHolder(amount, "usd")
  def eur = AmountHolder(amount, "eur")
  def rub = AmountHolder(amount, "rub")
}


3.usd to rub on 30--11--2014
1.rub to eur

//example usage:
val rate: Double = 5.eur to rub

//if user pass wrong date or create unknown currency, then it will throw exception
try {
  print(usd to rub on 40--11--2014: Double)
} catch {
  case e: Exception => e.getMessage
}
