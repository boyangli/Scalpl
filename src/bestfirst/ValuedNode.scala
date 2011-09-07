package bestfirst

import scala.math.Ordered

class ValuedNode [N]  (
		val content:N,
		val value:Double
) extends Ordered[ValuedNode[N]]
{
  override def compare(that:ValuedNode[N]):Int =
  {
    if (this.value > that.value) -1
    else if (this.value < that.value) 1
    else 0
  }
}