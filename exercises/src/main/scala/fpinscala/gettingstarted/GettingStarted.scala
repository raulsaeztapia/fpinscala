package fpinscala.gettingstarted

// A comment!
/* Another comment */
/** A documentation comment */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("fibonacci", 8, fib))
    println(formatResult("increment", 7, (x: Int) => x + 1))
    println(formatResult("increment2", 7, (x) => x + 2))
    println(formatResult("increment3", 7, x => x + 3))
    println(formatResult("increment4", 7, _ + 4))
    println(formatResult("increment5", 7, x => {val r = x + 5; r}))
  }

  // A definition of factorial, using a local, tail recursive function
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  // Another implementation of `factorial`, this time with a `while` loop
  def factorial2(n: Int): Int = {
    var acc = 1
    var i = n
    while (i > 0) { acc *= i; i -= 1 }
    acc
  }

  // Exercise 1: Write a function to compute the nth fibonacci number

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, a: Int, b: Int): Int = n match {
      case 0 => a
      case _ => loop(n - 1, b, (a + b))
    }
    loop(n, 0, 1)
  }

  // This definition and `formatAbs` are very similar..
  private def formatFactorial(n: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(n, factorial(n))
  }

  // We can generalize `formatAbs` and `formatFactorial` to
  // accept a _function_ as a parameter
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }
}

object FormatAbsAndFactorial {

  import MyModule._

  // Now we can use our general `formatResult` function
  // with both `abs` and `factorial`
  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
  }
}

// Functions get passed around so often in FP that it's
// convenient to have syntax for constructing a function
// *without* having to give it a name
object AnonymousFunctions {
  import MyModule._

  // Some examples of anonymous functions:
  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("increment", 7, (x: Int) => x + 1))
    println(formatResult("increment2", 7, (x) => x + 1))
    println(formatResult("increment3", 7, x => x + 1))
    println(formatResult("increment4", 7, _ + 1))
    println(formatResult("increment5", 7, x => { val r = x + 1; r }))
  }
}

object MonomorphicBinarySearch {

  def main(args: Array[String]): Unit = {
    println(binarySearch(Array(10.0, 11.1, 12.2), 111.1))
  }

  // First, a binary search implementation, specialized to `Double`,
  // another primitive type in Scala, representing 64-bit floating
  // point numbers
  // Ideally, we could generalize this to work for any `Array` type,
  // so long as we have some way of comparing elements of the `Array`
  def binarySearch(ds: Array[Double], key: Double): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val d = ds(mid2) // We index into an array using the same
                         // syntax as function application
        if (d == key) mid2
        else if (d > key) go(low, mid2, mid2-1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, ds.length - 1)
  }

}

object PolymorphicFunctions {

  def main(args: Array[String]): Unit = {
    println(binarySearch(Array(10.0, 11.1, 12.2), 111.1, (x: Double, y: Double) => x > y))
    println(isSorted(Array(10.0, 11.1, 12.2), (x: Double, y: Double) => x > y))
    // aquí es interesante interpretar que estamos pasando un primer parámetro de tipo Double,
    // y que el segundo parámetro es una función que como primer parámetro debe ser del mismo tipo que el primer
    // parámetro, y que partial1 devuelve una función que utiliza el primer parámetro que pasamos aquí pero que no le
    // hemos pasado un parámetro de tipo B pero que será el que utilice para hacer los cálculos. La función que es
    // devuelta nosotros la invocamos con el (0) como parámtro de tipo B, es decir de tipo Int.
    println(partial1(1D, (x: Double, y: Int) => x > y)(0))
    // aquí hacemos la misma invocación pero con un valor que nos devuelve false ya que 1D no es mayor que 1: Int.
    println(partial1(1D, (x: Double, y: Int) => x > y)(1))
    // curry nos muestra que podemos invocar a una función por partes. El resultado de invocar a la función con sólo
    // el primer parámetro será una función que acepta el segundo parámetro como su primer parámetro::
    // aquí nos devuelve la función con el primer parámetro
    println(curry((x: Double, y: Int) => x + y > 0))
    // aquí nos devuelve la función con el segundo parámetro
    println(curry((x: Double, y: Int) => x + y > 0)(4D))
    // aquí nos devuelve el resultado de la ejecución de la función con los dos parámetros
    println(curry((x: Double, y: Int) => x + y > 0)(4D)(5))
    // ahora vamos a agrupar varias funciones en una sóla:
    // vemos como los parámetros que pasamos a uncurry son función Double, función Int y el resultado es una función
    // que toma dos parámetros, que deben cumplir con los tipos A y B, y devuelve Boolean ( tipo C )
    println(uncurry((x: Double) => (y: Int) => x + y > 0)(4D, 5))
    // compose, para incluir el resultado de una función como entrada de otra función
    println("Compose (3 to Double and check that is equals to 3) --> " +
        compose((x: Double) => x == 3, (y: Int) => y.toDouble)(3))
  }

  // Here's a polymorphic version of `binarySearch`, parameterized on
  // a function for testing whether an `A` is greater than another `A`.
  def binarySearch[A](as: Array[A], key: A, gt: (A,A) => Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key,a)) mid2
        else if (greater) go(low, mid2, mid2-1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, as.length - 1)
  }

  // Exercise 2: Implement a polymorphic function to check whether
  // an `Array[A]` is sorted
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(i: Int, prev: A): Boolean =
      if (i == as.length) true
      else if (gt(as(i), prev)) go(i + 1, as(i))
      else false
    if (as.length == 0) true
    else go(1, as(0))
  }

  // Polymorphic functions are often so constrained by their type
  // that they only have one implementation! Here's an example:
  // aquí en resumidas cuentas lo que hacemos es pasar un primer parámetro como tipo A,
  // un segundo parámetro como un función que toma como parámetros los tipos A y B y que devuelve un tipo C,
  // y por último esta función devuelve una función que pasa como parámetro un tipo B y devuelve un tipo C
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a, b)

  // Exercise 3: Implement `curry`.

  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = { a => b => f(a, b) }

  // NB: The `Function2` trait has a `curried` method already

  // Exercise 4: Implement `uncurry`
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = { (a, b) => f(a)(b) }

  /*
  NB: There is a method on the `Function` object in the standard library,
  `Function.uncurried` that you can use for uncurrying.

  Note that we can go back and forth between the two forms. We can curry
  and uncurry and the two forms are in some sense "the same". In FP jargon,
  we say that they are _isomorphic_ ("iso" = same; "morphe" = shape, form),
  a term we inherit from category theory.
  */

  // Exercise 5: Implement `compose`

  def compose[A,B,C](f: B => C, g: A => B): A => C = { a => f(g(a)) }
}
