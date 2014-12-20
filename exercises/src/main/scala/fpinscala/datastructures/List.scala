package fpinscala.datastructures

import annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =  l match {
    // es una buena práctica devolver un error si una estructura de datos vacía porque ayuda a que posibles
    // errores motivados por manejar estructuras de datos vacías no se dilaten en el tiempo
    case Nil => sys.error("This list is empty")
    // también es buena práctica utilizar '_' en la parte izquierda cuando el valor sea irrelevante en el lado derecho
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead in a empty list")
    case Cons(_, t) => Cons(h, t)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  /**
   * Método recursivo que componiendo el List hasta que encuentra un último Cons(h, t) cuyo t es Nil. Éste último no
   * lo incluye.
   *
   * @param l
   * @tparam A
   * @return
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /**
   * Método tail recursive que se apoya en un ListBuffer para mejorar el rendimiento de esta estructura de datos y
   * que cuando llega al Cons(h, t) cuyo t es Nil devuelve el List construido a partir de todos los elementos Int
   * incluídos en el buffer.
   *
   * @param l
   * @tparam A
   * @return
   */
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) =>  List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }
    go(l)
  }

  /* EXERCISE 7: Can product implemented using foldRight immediately halt the recursion and return 0.0 if it
  encounters a 0.0 ? Why or why not? */
  /* creo que la recursión no se detendría porque el producto se calcularía después de que se hayan inferido
  todos los tipos mediante el uso del segundo parámetro, que es una función, y es precisamente la que provoca la
  recursión ya que invocamos al mismo foldRight */

  def length[A](l: List[A]): Int = sys.error("todo")

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = sys.error("todo")

  def map[A, B](l: List[A])(f: A => B): List[B] = sys.error("todo")

}

/**
 * He tenido que crear es objeto para poder montar un método main. En el List object companion no puedo poner el
 * método main porque nunca se ejecutará. List es un trait y no será nunca, ni el trait ni su object companion,
 * un objeto que pueda ser instanciado para ejecutar su método main.
 */
object Prueba {

  def main(args: Array[String]): Unit = {
    // exercise 1: la pregunta era cuál case, en el pattern matching, es el acertado en val x.... definido
    // más arriba. La respuesta es el tercer case.
    println("Exercise 1: " + List.x)
    // exercise 2: vamos a probar el método tail así que primero hacemos la lista y luego el tail para comprobar
    val lista2: List[Int] = List(1,2,3,4,5)
    val lista2_tail: List[Int] = List.tail(lista2)
    println("Exercise 2: " + lista2_tail)
    // exercise 3: vamos a modificar el head de la lista
    println("Exercise 3: " + List.setHead(lista2, 33))
    // exercise 4: ahora vamos a probar el método drop que coge los últimos n
    println("Exercise 4: " + List.drop(lista2, 3))
    // exercise 5: probamos el método dropWhile con un case muy interesante que conlleva un condicional
    println("Exercise 5: " + List.dropWhile(lista2, (x: Int) => x < 3))
    // exercise 6: probamos el método init donde cogemos todos los items menos el último
    println("Exercise 6: " + List.init(lista2))
    println("Exercise 6.2: " + List.init2(lista2)) // es digno de ver las operaciones que se hacen en la clase Vector
    // con bytes para manejar los bloques donde se almacenan los datos dentro de la estructura O_o
  }
}
