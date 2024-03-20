package u03

import Optionals.Optional.*
import org.junit.*
import org.junit.Assert.*
import u02.Modules.Person
import Person.*

class SequenceTest:
  import u03.Sequences.*
  import Sequence.*

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))
  
  @Test def testTake() =
    assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
    assertEquals(Nil(), take(l)(0))
    assertEquals(Nil(), take(Nil())(2))
  
  @Test def testZip() = 
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(l, l2))
    assertEquals(Nil(), zip(l, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(l, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(l2, Nil()))
    assertEquals(Nil(), concat(Nil(), Nil()))

  @Test def testFlatMap() =
    assertEquals(l, flatMap(l)(v => Cons(v, Nil())))  // same list
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(10, Cons(10, Cons(20, Cons(20, Cons(30, Cons(30, Nil())))))), flatMap(l)(v => Cons(v, Cons(v, Nil()))))  // 10, 10, 20, 20, 30, 30
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMin() =
    assertEquals(Just(10), min(l))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Empty(), min(Nil()))
    assertEquals(Just(10), min(Cons(20, Cons(10, Cons(30, Nil())))))

  @Test def testTail() =
    assertEquals(Cons(20, Cons(30, Nil())), tail(l))
    assertEquals(Nil(), tail(Nil()))

  @Test def testHead() =
    assertEquals(10, head(l))
    assertEquals(20, head(tail(l)))

  @Test def testAccumulator() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil ()))))
    assertEquals(0, foldLeft(Nil())(0)(_ + _))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(0, foldLeft(lst)(16)(_ - _))
    assertEquals(6, foldLeft(Cons(1, Cons(2, Cons(3, Nil()))))(0)(_ + _))

  @Test def testTeacherCourses() =
    val l: Sequence[Person] =
            Cons(Teacher("Mirko", "PPS"),
            Cons(Student("Mario", 2022),
            Cons(Teacher("Alessandro", "PCD"), Nil())))
    assertEquals(Cons("PPS", Cons("PCD", Nil())), courses(l))
    assertEquals(Nil(), courses(Nil()))
    assertEquals(Nil(), courses(Cons(Student("Mario", 2022), Nil())))
