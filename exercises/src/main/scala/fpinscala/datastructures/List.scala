package fpinscala.datastructures

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

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

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

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

  @tailrec
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
    import scala.collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) =>  List(buf.toList: _*)
      case Cons(h, t) => {
        buf += h
        go(t)
      }
    }
    go(l)
  }

  /* EXERCISE 7: Can product implemented using foldRight immediately halt the recursion and return 0.0 if it
  encounters a 0.0 ? Why or why not? */
  /* creo que la recursión no se detendría porque el producto se calcularía después de que se hayan inferido
  todos los tipos mediante el uso del segundo parámetro, que es una función, y es precisamente la que provoca la
  recursión ya que invocamos al mismo foldRight. Siempre se evaluará primero el argumento de la función antes que la
  propia función, y el argumento es el foldRight */

  /* EXERCISE 8: See what happens when you pass Nil and Cons themselves to
  foldRight ,like this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
  What do you think this says about the relation3ship between foldRight and the data constructors of List ? */
  /* creo que la relación entre foldRight y el constructor (método apply) de List es que foldRight podría generalizar
   la funcionalidad del propio constructor */

  // en la función que definimos ponemos nosotros los argumentos y así decidimos qué utilizamos en la parte de la
  // derecha. Como no vamos a usar el primer argumento no le dotamos de nombre
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product3(ds: List[Int]): Int = foldLeft(ds, 1)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))

  /* The implementation of `foldRight` in terms of `reverse` and `foldLeft` is a common trick for avoiding stack
  overflows when implementing a strict `foldRight` function as we've done in this chapter. (We'll revisit this in a
  later chapter, when we discuss laziness).
  The other implementations build up a chain of functions which, when called, results in the operations being
  performed with the correct associativity. We are calling `foldRight` with the `B` type being instantiated to `B =>
  B`, then calling the built up function with the `z` argument. Try expanding the definitions by substituting equals
  for equals using a simple example, like `foldLeft(List(1,2,3), 0)(_ + _)` if this isn't clear. Note these
  implementations are more of theoretical interest - they aren't stack-safe and won't work for large lists. */
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
  foldLeft(reverse(l), z)((b,a) => f(a,b))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
  foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
  foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def length3[A](l: List[A]): Int = foldRightViaFoldLeft(l, 0)((_, acc) => acc + 1)
  def length4[A](l: List[A]): Int = foldRightViaFoldLeft_1(l, 0)((_, acc) => acc + 1)
  def reverse2[A](l: List[A]): List[A] = foldLeftViaFoldRight(l, List[A]())((acc, h) => Cons(h, acc))

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((h, acc) => Cons(h, acc))
//  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def concat[A](lists: List[List[A]]): List[A] = foldRight(lists, Nil: List[A])(append)

  def aggregate1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, acc) => Cons(h + 1, acc))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, acc) => Cons(h.toString, acc))

  def toString(l: List[Double]): String = foldRight(l, "": String)((h, acc) => {
    if (acc.isEmpty) h.toString.concat(acc)
    else h.toString.concat(", ").concat(acc)
  })

  /* A natural solution is using `foldRight`, but our implementation of `foldRight` is not stack-safe. We can use
   `foldRightViaFoldLeft` to avoid the stack overflow (variation 1), but more commonly, with our current
   implementation of `List`, `map` will just be implemented using local mutation (variation 2). Again, note that the
   mutation isn't observable outside the function, since we're only mutating a buffer that we've allocated. */
  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def map2[A, B](l: List[A])(f: A => B): List[B] = foldRightViaFoldLeft(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def mapRsaez[A, B](l: List[A])(f: A => B): List[B] = {
    import scala.collection.mutable.ListBuffer
    val buf = new ListBuffer[B]
    @tailrec
    def go(cur: List[A]): List[B] = cur match {
      case Nil => List(buf.toList: _*) // convirtiendo a nuestra List implementada
      case Cons(h, t) => buf += f(h); go(t)
    }
    go(l)
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRightViaFoldLeft(l, Nil: List[A])(
    (h, acc) => if (f(h)) Cons(h, acc) else acc)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  def filterByFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(x => if (f(x)) Cons(x, Nil) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, addPairwise(ta, tb))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
  }


  def startsWith[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => false
    case Cons(h, t) if startsWith(l, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }
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
    println("Exercise 9 --> length: " + List.length(lista2))
    println("Exercise 10 --> foldLeft: " + List.foldLeft(lista2, 0)(_ + _))
    println("Exercise 11a --> sum3 with foldLeft: " + List.sum3(lista2))
    println("Exercise 11b --> product3 with foldLeft: " + List.product3(lista2))
    println("Exercise 11c --> length2 with foldLeft: " + List.length2(lista2))
    println("Exercise 12 --> reverse list using some fold: " + List.reverse(lista2))
    println("Exercise 13a --> length: " + List.length3(lista2))
    println("Exercise 13b --> length: " + List.length4(lista2))
    println("Exercise 13c --> reverse using foldLeftViaFoldRight: " + List.reverse2(lista2))
    val lista3: List[Int] = List(6,7,8,9)
    println("Exercise 14 --> append using foldLef: " + List.append2(lista2, lista3))
    println("Exercise 15 --> append lists of list using foldRight: " + List.concat(List(lista2, lista3)))
    val lista4: List[Double] = List(1.0,2.0,3.0,4.0)
    println("Exercise 17a --> List[Double] to List[String]: " + List.doubleToString(lista4))
    println("Exercise 17b --> List[Double] to String: " + List.toString(lista4))
    println("Exercise 18a --> implementation of map: " + List.map(lista4)(value => value.toString))
    println("Exercise 18b --> implementation of map: " + List.map2(lista4)(value => value.toString))
    println("Exercise 18c --> implementation of map rsaez: " + List.mapRsaez(lista4)(value => value.toString))
    println("Exercise 19 --> implementation of filter: " + List.filter(lista2)(x => x % 2 == 0))
    println("Exercise 20 --> implementation of flatMap: " + List.flatMap(lista2)(x => List(x, x)))
    println("Exercise 21 --> implementation of filter with flatMap: " + List.filterByFlatMap(lista2)(x => x > 2))
    println("Exercise 22 --> addPairwise method for plus integers from two lists : " + List.addPairwise(lista2, lista3))
    println("Exercise 23 --> doPairwise method for do operations on two lists on Integers: " +
        List.zipWith(lista2, lista3)(_ + _))
    val lista5 = List("a", "b", "c")
    val lista6 = List("d", "e", "f")
    println("Exercise 23b --> doPairwise method for do operations on two lists of Strings: " +
        List.zipWith(lista5, lista6)(_ + " -> " + "" + _))
    println("Exercise 24 --> a list has a subsequence list: " + List.addPairwise
        (lista2, lista3))
  }
}
