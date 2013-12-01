package suggestions

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import suggestions.observablex._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import scala.concurrent.duration.DurationInt
import java.util.NoSuchElementException
import rx.lang.scala.Observable
import scala.util._

/**
 * Created with IntelliJ IDEA.
 * User: julien
 * Date: 1/12/13
 * Time: 1:04 PM
 * To change this template use File | Settings | File Templates.
 */
@RunWith(classOf[JUnitRunner])
class ObservableExTest extends FunSuite {
  def testObservable(future: Future[Unit], expectedCount: (Int, Int, Int)) {

    var countNext = 0
    var countError = 0
    var countCompleted = 0

    ObservableEx(future).subscribe(
      (_ => { countNext += 1 }),
      (_ => {
        countError += 1
      }),
      (() => { countCompleted += 1 }))

    Thread.sleep(1000)

    assert(countNext === expectedCount._1, countNext)
    assert(countError === expectedCount._2, countError)
    assert(countCompleted === expectedCount._3, countCompleted)
  }

  test("Observable should emit the future value and complete") {
    testObservable(Future {
      Thread.sleep(500)
    }, (1,0,1))
  }

  test("Observable should emit an error when the future fails") {
    testObservable(Future {
      Thread.sleep(500)
      throw new NoSuchElementException
    }, (0,1,0))
  }

  test("Observable replay future value to observer registering after the future has finished") {
    testObservable( Future {
      1
    }, (1,0,1))
  }

//  test("timedout"){
//    val s = Observable(1, 2, 3).zip(Observable.interval(700 millis)).timedOut(1L)
//
//    var countNext = 0
//    var countError = 0
//    var countCompleted = 0
//
//    s.subscribe(
//      (_ => { countNext += 1 }),
//      (_ => {
//        countError += 1
//      }),
//      (() => { countCompleted += 1 }))
//
//    Thread.sleep(1500)
//
//    assert(countNext === 1, countNext)
//    assert(countError === 0, countError)
//    assert(countCompleted === 1, countCompleted)
//  }
//
//  test("concat recovered") {
//    val s = Observable(1,2,3,4,5).concatRecovered(
//      num => if (num != 4) Observable(num) else Observable(new Exception)
//    )
//
//    var countNext = 0
//    var countSuccess = 0
//    var countFailure = 0
//    var countError = 0
//    var countCompleted = 0
//
//    s.subscribe(
//      (x => {
//        countNext += 1
//        x match {
//          case Success(_) => { countSuccess += 1 }
//          case Failure(_) => { countFailure += 1 }
//        }
//      }),
//      (_ => {
//        countError += 1
//      }),
//      (() => { countCompleted += 1 }))
//
//    Thread.sleep(1000)
//
//    assert(countNext === 5, countNext)
//    assert(countSuccess === 4, countNext)
//    assert(countFailure === 1, countNext)
//    assert(countError === 0, countError)
//    assert(countCompleted === 1, countCompleted)
//  }

}
