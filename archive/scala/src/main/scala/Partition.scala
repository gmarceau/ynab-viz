object Partition {

//  def apply[T](items: List[T]): List[List[T]] = {
//
//    val n = items.size
//
//    return [] if k <= 0
//    return seq.map((x) -> [x]) if k > n
//
//    table = (0 for x in [0...k] for y in [0...n])
//    solution = (0 for x in [0...k-1] for y in [0...n-1])
//    table[i][0] = seq[i] + (if i then table[i-1][0] else 0) for i in [0...n]
//    table[0][j] = seq[0] for j in [0...k]
//    for i in [1...n]
//    for j in [1...k]
//    m = _.min(([_.max([table[x][j-1], table[i][0]-table[x][0]]), x] for x in [0...i]), (o) -> o[0])
//    table[i][j] = m[0]
//    solution[i-1][j-1] = m[1]
//
//    n = n-1
//    k = k-2
//    ans = []
//    while k >= 0
//    ans = [seq[i] for i in [(solution[n-1][k]+1)...n+1]].concat ans
//      n = solution[n-1][k]
//    k = k-1
//
//      [seq[i] for i in [0...n+1]].concat ans
//  }
}
