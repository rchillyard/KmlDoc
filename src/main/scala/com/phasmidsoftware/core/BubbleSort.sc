
def sort[X: Ordering] (xs: List[X]): Unit = {
    val ordering = implicitly[Ordering[X]]
    def inner(sorted: List[X], x: X, unsorted: List[X]): List[X] =
        unsorted match {
            case Nil => sorted
            case head :: Nil => inner(head :: sorted, Nil)
            case first :: second :: tail =>
                if (ordering.compare(first, second) <= 0)
                    inner(first :: sorted, second :: tail)
                else
                    inner(second :: sorted, first :: tail)
        }
    def outer(unsorted: List[X], sorted: List[X]): List[X] =
        unsorted match {
            case Nil => sorted
            case h :: tail => outer(inner(Nil, unsorted))
        }
}