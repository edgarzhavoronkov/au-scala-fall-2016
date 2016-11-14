import java.text.SimpleDateFormat
import java.util.Calendar

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.select.Elements

case class Date(day: Int, month: Int, year: Int)

class DayMonth(day: Int, month: Int) {
  def --(year: Int): Date = Date(day, month, year)
}

implicit class IntExt3(i2: Int) {
  def --(i: Int): DayMonth = new DayMonth(i2, i)
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

class Conversion(from: Currency, to: Currency, date : Date = null) {
  def convert : BigDecimal = {
    if (to == rub) {
      getRateAgainstRub(from)
    } else if (from == rub) {
      1 / getRateAgainstRub(to)
    } else {
      getRateAgainstRub(from) / getRateAgainstRub(to)
    }
  }

  private def getRateAgainstRub(currency: Currency) : BigDecimal = {
    //<shitcode>
    var desiredDate : String = ""
    if (date == null) {
      desiredDate = new SimpleDateFormat("dd.MM.yyyy").format(Calendar.getInstance().getTime)
    } else {
      desiredDate = s"${date.day}.${date.month}.${date.year}"
    }
    //</shitcode>
    val document: Document = Jsoup.connect(s"http://www.cbr.ru/eng/currency_base/daily.aspx?date_req=$desiredDate").get()
    val table = document.select("table").get(0)
    val rows = table.select("tr")
    var res : BigDecimal = 0

    for (i <- 1 until rows.size()) {
      val row = rows.get(i)
      val tds : Elements = row.select("td")
      val unit : Int = tds.get(2).text.toInt
      val code : String = tds.get(1).text.toLowerCase()
      if (code == currency.getCode) res = BigDecimal(tds.get(4).text) / unit
    }
    res
  }
}

implicit class CurrencyExt(from: Currency) {
  def to(to: Currency) = new Conversion(from, to).convert
}

//implicit class ConversionExt(from: Currency) {
//  def on(on: Date) = new CurrencyExt(from).to(to)
//}


jpy to rub
usd to rub
eur to rub
rub to eur
eur to usd





