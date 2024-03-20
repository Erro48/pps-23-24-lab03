package u03

import Optionals.Optional
import Optionals.Optional.*

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def head[E](l: Sequence[E]): E = l match
      case Cons(h, _) => h

    def tail[E](l: Sequence[E]): Sequence[E] = l match
      case Cons(_, t) => t
      case _ => Nil()
    

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    // def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
    //   case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
    //   case Nil()      => Nil()

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Nil() => Nil()
      case _ => flatMap(l)(a => Cons(mapper(a), Nil()))
    

    // def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
    //   case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
    //   case Cons(_, t)            => filter(t)(pred)
    //   case Nil()                 => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Nil() => Nil()
      case _ => flatMap(l1)(a => a match
        case a if pred(a) => Cons(a, Nil())
        case _ => Nil())
        
    

    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = first match
      case Nil() => Nil()
      case _ => second match
        case Nil() => Nil()
        case _ => Cons((head(first), head(second)), zip(tail(first), tail(second)))
    
    def take[A](l: Sequence[A])(n: Int): Sequence[A] = n match
      case n if n == 0 => Nil()
      case _ => l match
        case Nil() => Nil()
        case _ => Cons(head(l), take(tail(l))(n-1))
    
    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
      case Nil() => l2
      case _ => l2 match
        case Nil() => l1
        case _ => Cons(head(l1), concat(tail(l1), l2))

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Nil() => Nil()
      case _ => concat(mapper(head(l)), flatMap(tail(l))(mapper))

    def min(l: Sequence[Int]): Optional[Int] = l match
      case Nil() => Empty()
      case _ if head(l) <= orElse(min(tail(l)), Integer.MAX_VALUE) => Just(head(l))
      case _ => min(tail(l))

    def foldLeft(l: Sequence[Int])(default: Int)(operator: (Int, Int) => Int): Int = l match
      case Nil() => default
      case _ => foldLeft(tail(l))(operator(default, head(l)))(operator)
    
    import u02.Modules.Person
    import u02.Modules.Person.* 

    def courses(l: Sequence[Person]): Sequence[String] = l match
      case Nil() => Nil()
      case _ => filter(map(l)(t => orElse(course(t), "")))(t => t != "")

      
    
    
@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
