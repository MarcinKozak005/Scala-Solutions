def f(arr:List[Int]):Int = {
    if (arr == Nil) 0
    else f(arr.tail) + 1
}