import org.junit.runner.RunWith
import org.scalacheck.Prop._
import org.scalacheck._
import org.scalatest.PropSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import state.History

@RunWith(classOf[JUnitRunner])
class TestHistory extends PropSpec with Checkers {
	
	val historyGen:Gen[(History[Int],Int)] = for {
		past <- Gen.containerOf[List,Int](Gen.choose(0,10))
		present <- Gen.choose(0,10)
		future <- Gen.containerOf[List,Int](Gen.choose(0,10))
		input <- Gen.choose(0,10)
	} yield (History(past,present,future),input)
	
	property("Update is correct") {
		check(
			forAll(historyGen) { (input:(History[Int],Int)) => input match {
				case(history, inp) =>
					history.past == history.update(inp).past &&
						history.update(inp).present == inp
			}}
		)
	}
	
	
	property("Advance is correct") {
		check(
			forAll(historyGen) { (input:(History[Int],Int)) => input match {
				case(history, inp) =>
					(history.present :: history.past) == history.advance(inp).past &&
						history.update(inp).present == inp
			}}
		)
	}
	
	property("Undo is correct") {
		check(
			forAll(historyGen) { (input:(History[Int],Int)) => input match {
				case(history, _) =>
					if(history.past.isEmpty)
						history.undo().isEmpty
					else {
						val undoed = history.undo()
						undoed.isDefined &&
							undoed.get.past == history.past.tail &&
							undoed.get.present == history.past.head &&
							undoed.get.future == history.present :: history.future
					}
			}}
		)
	}
	
	property("Redo is correct") {
		check(
			forAll(historyGen) { (input:(History[Int],Int)) => input match {
				case (history, _) =>
					if (history.future.isEmpty)
						history.redo().isEmpty
					else {
						val redone = history.redo()
						redone.isDefined &&
							redone.get.past == history.present :: history.past &&
							redone.get.present == history.future.head &&
							redone.get.future == history.future.tail
					}
			}}
		)
	}
}
