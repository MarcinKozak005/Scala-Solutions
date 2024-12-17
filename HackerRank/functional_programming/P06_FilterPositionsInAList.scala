def f(arr: List[Int]): List[Int] = {
    (for (i <- arr.indices if i % 2 == 1) yield arr(i)).toList
}