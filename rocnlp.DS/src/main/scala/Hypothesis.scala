/**
 * Created by Omid on 8/1/2014.
 */

class Hypothesis[A](){
  var totalMass = 0.0
  var assignments:scala.collection.mutable.Map[Set[A], Double] =scala.collection.mutable.Map.empty
  def addAssignment(candidates:Set[A], mass:Double) = {
    if(assignments.contains(candidates)) {
      totalMass = totalMass - assignments(candidates)
      totalMass = totalMass + mass
      assignments(candidates) = mass
      normalize
    }
    else {
      totalMass = totalMass + mass
      assignments = assignments + (candidates -> mass)
      normalize
    }
  }
  def normalize = {
    if(totalMass>1) {
      assignments = assignments.map{case(key,value) => key -> value/totalMass}
    }
  }
  def getAssignment = assignments

}
