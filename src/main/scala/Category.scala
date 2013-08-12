
case class Category(name: String, items: List[Item]) {
  def expenses = items.filter(_.amt > 0).map(_.amt).sum
  def income = -items.filter(_.amt < 0).map(_.amt).sum
  def absorbIncomeIntoExpenses = {
    val ratio = (expenses - income) / expenses
    val result = for (i <- items) yield { i.copy(outflow = i.outflow * ratio, inflow = 0) }
    this.copy(items = result)
  }
  def absorbExpensesIntoIncome = {
    val ratio = (income - expenses) / income
    val result = for (i <- items) yield { i.copy(inflow = i.inflow * ratio, outflow = 0) }
    this.copy(items = result)
  }
}

