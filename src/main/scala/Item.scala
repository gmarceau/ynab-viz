import java.util.Date

case class Item(date: Date, category: List[String], payee: String, memo: String, outflow: Double, inflow: Double) {
  def label: String = {
    val skipList = List("DBT CRD", "POS DEB", "PPD", "Payroll", "Paypal", "PAYPAL", "Payee",
      "T MOBILE", "ATM FEE", "AMAZON", "Amazon", "Check ", "CREDIT", "DDA", "DEBIT", "e-Pay", "Interest Deposit",
      "KEY FOODS", "FOODTOWN", "Adjustment", "MTA", "Transfer", "USPS").map(_.r)
    def skip(str: String): Boolean = skipList.find(_.findFirstIn(str).isDefined).isDefined
    val result = List(memo, payee, category.last).find(!skip(_)).get
//    println("label: " + this + " => " + result)
    result
  }
  def amt = outflow - inflow

}
