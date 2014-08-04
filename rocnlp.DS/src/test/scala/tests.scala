/**
* Created by Omid on 8/3/2014.
*/

import collection.mutable.Stack
import org.scalatest._

import scala.collection.mutable

case class booleanTest(value:Int)
case class diseaseTest(value:String)

class tests extends FlatSpec with Matchers {

  "A DS" should "combine hypothesises correctly" in {
    val h1 = new Hypothesis[booleanTest]
    val h2 = new Hypothesis[booleanTest]
    val a = booleanTest(0)
    val notA = booleanTest(1)

    h1.addAssignment(Set(a),0.8)
    h1.addAssignment(Set(notA),0.0)

    h2.addAssignment(Set(a),0.9)
    h2.addAssignment(Set(notA),0.0)

    val ds = new DempsterShafer[booleanTest](h2 :: h1 :: Nil)
//    ds.aggregated.foreach(println(_))
    //ds.aggregated.size should be(3)

    ds.aggregated.foreach{case (set,double) => {
      if(set.contains(a) && set.size == 1)
        Math.abs(double - 0.98) should be < 0.00001
      if(set.contains(a) && set.contains(notA))
        Math.abs(double - 0.02) should be < 0.00001
    }}

  }

  "A DS" should "combine hypothesises of diseases correctly" in {
    val h1 = new Hypothesis[diseaseTest]
    val h2 = new Hypothesis[diseaseTest]
    val allergy = diseaseTest("A")
    val flu = diseaseTest("F")
    val cold = diseaseTest("C")
    val pneumonia = diseaseTest("P")

    val A = allergy
    val F = flu
    val C = cold
    val P = pneumonia

    h1.addAssignment(Set(F,C,P),0.6)
    h2.addAssignment(Set(A,F,C),0.8)


    val ds = new DempsterShafer[diseaseTest](h2 :: h1 :: Nil)
    println(ds.aggregated)
    ds.confidenceInterval.foreach(println(_))
    ds.aggregated.foreach{case (set,double) => {
      if(set.contains(A) && set.size == 1)
        Math.abs(double - 0.9) should be < 0.00001

    }}

  }
}
