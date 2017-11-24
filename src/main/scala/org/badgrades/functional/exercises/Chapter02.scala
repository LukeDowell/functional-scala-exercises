package org.badgrades.functional.exercises

import scala.annotation.tailrec

object Chapter02 {

  /**
    * Write a recursive function to get the nth Fibonacci number. The
    * first two Fibonacci numbers are 0 and 1. The nth number is always
    * the sum of the previous two. Your definition should use a local
    * tail-recursive function.
    */
  object `2.1` {
    def fibonacciNumberAtIndex(index: Int): Int = {
      assert(index > 0, "The index of a fibonacci sequence starts at 1")
      if (index == 1) return 0 else if (index == 2) return 1

      @tailrec
      def fibonacci(prev: Int, curr: Int, n: Int): Int = {
        if (n == index) curr
        else fibonacci(curr, prev + curr, n + 1)
      }

      fibonacci(0, 1, 2)
    }
  }

  /**
    * Implement isSorted, which checks whether an Array[A] is sorted
    * according to a given comparison function
    */
  object `2.2` {
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = as.zipWithIndex
      .forall { case (element, index) =>
        if (index == as.length - 1) true
        else ordered(element, as(index + 1))
      }
  }

  /**
    * Let's look at another example, currying, which converts a function f
    * of two arguments into a function of one argument that partially applies f.
    * Here again there's only one implementation that compiles. Write this implementation:
    *
    */
  object `2.3` {
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)
  }

  /**
    * Implement uncurry, which reverses the transformation of curry. Note that since =>
    * associates to the right, A => (B => C) can be written as A => B => C
    */
  object `2.4` {
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
  }

  /**
    * Implement the higher order function that composes two functions
    */
  object `2.5` {
    def compose[A, B, C](f: B => C, g: A => B): A => C = (a) => f(g(a))
  }

  def main(args: Array[String]): Unit = {

  }
}