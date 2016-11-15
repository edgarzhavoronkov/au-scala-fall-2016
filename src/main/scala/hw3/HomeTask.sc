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

case class CurrencyHolder(amount: Double, code: String)

case class Conversion(from: String, to: String, var date: Date = Date.today, var amount: Double) {
  def on(date: Date): Conversion = {
    this.date = date
    this
  }

  def convert: Conversion = {
    if (to == "rub") {
      this.amount = getRateAgainstRub(from)
    } else if (from == "rub") {
      this.amount = 1 / getRateAgainstRub(to)
    } else {
      this.amount = getRateAgainstRub(from) / getRateAgainstRub(to)
    }
    this
  }

  private def getRateAgainstRub(currency: String) : Double = {
    val document: Document = Jsoup.connect(s"http://www.cbr.ru/eng/currency_base/daily.aspx?date_req=${date.day}.${date.month}.${date.year}").get()
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
    res
  }
}

implicit class ConversionExt(conversion: Conversion) {
  def on(date: Date): Double = Conversion(conversion.from, conversion.to, date ,conversion.amount).convert.amount
}

implicit class CurrencyHolderExt(from: CurrencyHolder) {
  def to(target: Currency) = Conversion(from.code, target.getCode, amount = from.amount).convert
  implicit def toDouble(target: Currency): Double = Conversion(from.code, target.getCode, amount = from.amount).convert.amount
}

implicit class DoubleExt(amount: Double) {
  def usd = CurrencyHolder(amount, "usd")
  def eur = CurrencyHolder(amount, "eur")
  def rub = CurrencyHolder(amount, "rub")
}

1.usd to rub on 20--11--2014

1.rub to eur