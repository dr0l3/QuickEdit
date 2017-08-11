import actions.StringInput
import com.intellij.openapi.util.TextRange
import org.scalatest.FlatSpec
import state._
import util.Jump
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop._

class TestReducers extends FlatSpec{
	
	"Initial state and stringinput" should "give a new updatestate" in {
		val input = StringInput("a")
		val initialState = UpdateSnapshot(List.empty,List.empty,"",Concrete(1))
		val text = "hello world"
		val textRange = new TextRange(0,text.length)
		val state = DTPPState(History(List.empty,initialState,List.empty),false,"",1,text,textRange,textRange)
		
		val res = Jump.update(state,input)
		assert(res.history.past == List.empty)
		assert(res.history.future == List.empty)
		assert(!res.exiting)
	}
	
}

object ReducerProperties extends Properties("Test specification") {
	
	val strInputGen:Gen[(StringInput, String)] = for {
		str <- Gen.alphaStr
	} yield (StringInput(str), str)
	
	val searchUpdateWithMatch:Gen[(StringInput, DTPPState,String, String)] = for {
		text <- Gen.alphaStr
		if text.length > 0
		caretPos <- Gen.choose(0,text.length)
		subStringStart <- Gen.choose(0, text.length-1)
		subStringEnd <- Gen.choose(subStringStart, text.length)
		if subStringStart < subStringEnd
		search = text.substring(subStringStart,subStringEnd)
		uss = UpdateSnapshot(List.empty,List.empty,"",Concrete(caretPos))
	} yield (
		StringInput(search),
		DTPPState(History(List.empty,uss,List.empty),false,"",caretPos,text, new TextRange(0,text.length),new TextRange(0,text.length)),
		search, text)
	
	val searchUpdateWithOutMatch:Gen[(StringInput, DTPPState, String,String)] = for {
		text <- Gen.alphaLowerStr
		search <- Gen.alphaLowerStr
		if text.length > 0 && search.length > 0
		if !(text contains search)
		caretPos <- Gen.choose(0,text.length)
		uss = UpdateSnapshot(List.empty,List.empty,"",Concrete(caretPos))
	} yield (
		StringInput(search),
		DTPPState(History(List.empty,uss,List.empty),false,"",caretPos,text, new TextRange(0,text.length),new TextRange(0,text.length)),
		search, text)
	
	property("Test area") = forAll(strInputGen) { (input:(StringInput,String)) => input match {
		case(strInput, str) => strInput.value.length == str.length
	}}
	
	property("Update with succesful search") =
		forAll(searchUpdateWithMatch) { (input: (StringInput, DTPPState, String, String)) => input match {
			case (strInput, state, search, text) =>
				val res = Jump.update(state,strInput)
				res.history.past.isEmpty &&
				res.history.future.isEmpty &&
				(res.history.present match {
					case ss: UpdateSnapshot =>
						ss.markers.nonEmpty ||
						text.indexOf(search) == state.initialCaretPos
					case _ => false
				}) &&
				!res.exiting
	}}
	
	property("Update with nonsuccesful search") =
		forAll(searchUpdateWithOutMatch) { (input: (StringInput, DTPPState, String, String)) => input match {
			case (strInput, state, _, _) =>
				val res = Jump.update(state,strInput)
				res.history.past.isEmpty &&
				res.history.future.isEmpty &&
				(res.history.present match {
					case ss: UpdateSnapshot => ss.markers.isEmpty
					case _ => false
				}) &&
				!res.exiting
	}}
	
}