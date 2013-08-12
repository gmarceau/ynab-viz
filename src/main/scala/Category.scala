
case class Category(name: String, items: List[Item]) {
  def expenses = items.filter(_.amt > 0).map(_.amt).sum
  def income = -items.filter(_.amt < 0).map(_.amt).sum
  def absorbIncomeIntoExpenses = {
    val ratio = (expenses - income) / expenses
    val result = for (i <- items) yield { i.copy(adjustment = i.outflow * ratio - i.outflow + i.inflow) }
    this.copy(items = result)
  }
  def absorbExpensesIntoIncome = {
    val ratio = (income - expenses) / income
    val result = for (i <- items) yield { i.copy(adjustment = -(i.inflow * ratio - i.inflow - i.outflow)) }
    this.copy(items = result)
  }
}

