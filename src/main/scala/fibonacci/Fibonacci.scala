package fibonacci

import scala.annotation.tailrec

object Fibonacci {
  def fibonacci(limit: Long): BigInt = {
    @tailrec
    def helper(limit: Long, a: BigInt, b: BigInt): BigInt = if (limit <= 1) a else helper(limit - 1, b, a + b)

    helper(limit, 1, 1)
  }
}
