import recursive.revs

import java.util.Scanner

object GCD {
  def task1_gcd(a: Int, b: Int): Int = {
    if(b ==0) a else task1_gcd(b, a%b)
  }
}

object recursive {
  def revs(s: String): String = {
    // if (s.isEmpty) “”
    if (s.length == 1) s
    else revs(s.tail) + " " + s.head
    //else revs(s.substring(1)) + s.charAt(0)
  }
}

object main {
  @throws[java.lang.Exception]
  def main(args: Array[String]): Unit = {

    println("Input 1 of 2 to choose a task: ")
    val in = new Scanner(System.in)
    val choose = in.nextInt()

    if (choose == 1) {
      //task1'

      println("Put 1 number: ")
      val a = in.nextInt()
      println("Put 2 number: ")
      val b = in.nextInt()
      System.out.print("The answer is: " + GCD.task1_gcd(a, b))
      System.out.print("\n==================")
    } else if (choose == 2) {
      // task2

      println("Put number to make it revert: ")
      val number = in.nextInt()
      val numberToString = number.toString
      print("The answer is: " + revs(numberToString))
    } else {
      println("Incorrect number")
    }
  }
}
