package u03

import Optionals.Optional
import Optionals.Optional.*

object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    /* EXTENSION [A] */
    extension [A](l: Sequence[A])

      def head: A = l match
        case Cons(h, _) => h

      def tail: Sequence[A] = l match
        case Cons(_, t) => t
        case _          => Nil()

        // def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
        //   case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
        //   case Cons(_, t)            => filter(t)(pred)
      //   case Nil()                 => Nil()

      def filter(pred: A => Boolean): Sequence[A] = l match
        case Nil() => Nil()
        case _ =>
          flatMap(l)(a =>
            a match
              case a if pred(a) => Cons(a, Nil())
              case _            => Nil()
          )

      def take(n: Int): Sequence[A] = n match
        case n if n == 0 => Nil()
        case _ =>
          l match
            case Nil() => Nil()
            case _     => Cons(l.head, l.tail.take(n - 1))

      def concat(l2: Sequence[A]): Sequence[A] = l match
        case Nil() => l2
        case _ =>
          l2 match
            case Nil() => l
            case _     => Cons(l.head, l.tail.concat(l2))

      /* EXTENSION [A, B] */
    extension [A, B](l: Sequence[A])

      def map(mapper: A => B): Sequence[B] = l match
        case Nil() => Nil()
        case _     => l.flatMap(a => Cons(mapper(a), Nil()))

      // def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      //   case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      //   case Nil()      => Nil()

      def zip(second: Sequence[B]): Sequence[(A, B)] =
        l match
          case Nil() => Nil()
          case _ =>
            second match
              case Nil() => Nil()
              case _ if l.tail == Nil() =>
                Cons((l.head, second.head), Nil())
              case _ =>
                Cons((l.head, second.head), l.tail.zip(second.tail))

      def flatMap(mapper: A => Sequence[B]): Sequence[B] = l match
        case Nil() => Nil()
        case _     => mapper(l.head).concat(l.tail.flatMap(mapper))

    extension (l: Sequence[Int])

      def sum: Int = l match
        case Cons(h, t) => h + t.sum
        case _          => 0

      def min: Optional[Int] = l match
        case Nil() => Empty()
        case _ if l.head <= orElse(l.tail.min, Integer.MAX_VALUE) =>
          Just(l.head)
        case _ => l.tail.min

      def foldLeft(default: Int)(operator: (Int, Int) => Int): Int = l match
        case Nil() => default
        case _     => l.tail.foldLeft(operator(default, l.head))(operator)

    import u02.Modules.Person
    import u02.Modules.Person.*

    extension (l: Sequence[Person])
      def courses: Sequence[String] = l match
        case Nil() => Nil()
        case _     => filter(map(l)(t => orElse(course(t), "")))(t => t != "")

@main def trySequences =
  import Sequences.*
  val l =
    Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
