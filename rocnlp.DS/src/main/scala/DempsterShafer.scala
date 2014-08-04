/**
 * Created by Omid on 8/2/2014.
 */
class DempsterShafer[A](hypothesis:List[Hypothesis[A]]) {
  var allAssignments = hypothesis.map(m => m.getAssignment.keySet).flatten.toSet
  val allCandidatesSet = allAssignments.flatten.toSet
  allAssignments = allAssignments + allCandidatesSet

  val allAssignmentsTable = hypothesis.map(h => {
    allAssignments.map(copySet => {
      if(h.getAssignment.keySet.contains(copySet))
        copySet -> h.getAssignment(copySet)
      else
        copySet -> 0.0
    }).toMap + (allCandidatesSet -> (1.0 - h.totalMass))
  })

  val initialFold:Map[Set[A],Double] = Map(allCandidatesSet -> 1.0)
  val aggregated  = allAssignmentsTable.foldLeft(initialFold){(res,element) => {
    val allIntersects = element.map { case (candE, massE) =>
      res.map { case (candR, massR) =>
        List((candE.intersect(candR), massE * massR))
      }.toList.flatten
    }.toList.flatten
    val tmpNormalizer = allIntersects.groupBy(_._1).filter(_._1.size == 0)
    val normalizer =  if(tmpNormalizer.size > 0) {
      tmpNormalizer.map { case (key, value) =>
        value.foldLeft(0.0) { (e, i) => i._2 + e}
      }.head
    }
    else
      0.0
    allIntersects.groupBy(_._1).map{case(key,value) =>
      key -> value.foldLeft(0.0){(e,i) => i._2 + e}/(1.0 - normalizer)
    }
  }}


  val belief = aggregated map{
    case(set,mass) =>
      set -> aggregated.filter(e => e._1.intersect(set) == e._1)
              .map(_._2).sum
  }

  val plausibility = aggregated map{
    case(set,mass) =>
      set -> aggregated.filter(e => e._1.intersect(set).size > 0)
        .map(_._2).sum
  }

  val confidenceInterval = aggregated map{
    case(set,mass) =>
      set -> (mass, belief(set),plausibility(set))

  }
}
