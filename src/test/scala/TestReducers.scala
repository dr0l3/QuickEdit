import actions._
import com.intellij.openapi.util.TextRange
import dtpp.util.EditorUtil
import org.scalacheck.{Gen, Properties}
import org.scalatest.FlatSpec
import state._
import util.{Constants, Jump, MarkerUtil}
import org.scalacheck.Prop._

import scala.collection.JavaConverters._

object TestReducers extends Properties("Update and string input") {
	
	val randomScroll:Gen[DTPPInput] = for {
		_ <- Gen.choose(0,1)
		possibleOptions = List(ScrollUpInput(), ScrollDownInput(), ScrollHomeInput())
		returned <- Gen.oneOf(possibleOptions)
	} yield returned
	
	val acceptStateGenerator:Gen[DTPPState] = for {
		searchText <- Gen.alphaStr
		repetitions <- Gen.choose(1,50)
		repsAsList = List.fill(repetitions)(searchText)
		text <- Gen.containerOfN[List,String](repetitions,Gen.alphaStr)
		allText = scala.util.Random.shuffle(repsAsList ::: text).mkString
		hits = EditorUtil.getMatchesSets(searchText,allText,java.util.Collections.emptyList())
		if hits.first.size() > 0
		prim = Set.empty ++ hits.first.asScala.map(_.toInt)
		sec = Map.empty ++ hits.second.asScala.map(p => p._1.toInt -> p._2)
		markers = MarkerUtil.convert2MarkersUnique(prim,sec, searchText)
		appended = markers._1.values.toList ::: markers._2.toList
		updateState = UpdateSnapshot(markers = appended,List.empty,searchText, Concrete(0))
		selectState = SelectSnapshot(markers = appended, List.empty, searchText, Concrete(0))
		acceptState = AcceptSnapshot(JumpEffect(Concrete(0),Concrete(1)))
		textRange = new TextRange(0, allText.length)
	} yield DTPPState(History(List(selectState,updateState), acceptState, List.empty), false, "", 0, allText, textRange, textRange)
	
	val selectStateGenerator:Gen[DTPPState] = for {
		searchText <- Gen.alphaStr
		repetitions <- Gen.choose(1,50)
		repsAsList = List.fill(repetitions)(searchText)
		text <- Gen.containerOfN[List,String](repetitions,Gen.alphaStr)
		allText = scala.util.Random.shuffle(repsAsList ::: text).mkString
		hits = EditorUtil.getMatchesSets(searchText,allText,java.util.Collections.emptyList())
		if hits.first.size() > 0
		prim = Set.empty ++ hits.first.asScala.map(_.toInt)
		sec = Map.empty ++ hits.second.asScala.map(p => p._1.toInt -> p._2)
		markers = MarkerUtil.convert2MarkersUnique(prim,sec, searchText)
		appended = markers._1.values.toList ::: markers._2.toList
		updateState = UpdateSnapshot(markers = appended,List.empty,searchText, Concrete(0))
		selectState = SelectSnapshot(markers = appended, List.empty, searchText, Concrete(0))
		textRange = new TextRange(0, allText.length)
	} yield DTPPState(History(List(updateState), selectState, List.empty), false, "", 0, allText, textRange, textRange)
	
	val updateStateGenerator:Gen[DTPPState] = for {
		searchText <- Gen.alphaStr
		repetitions <- Gen.choose(1,50)
		repsAsList = List.fill(repetitions)(searchText)
		text <- Gen.containerOfN[List,String](repetitions,Gen.alphaStr)
		allText = scala.util.Random.shuffle(repsAsList ::: text).mkString
		hits = EditorUtil.getMatchesSets(searchText,allText,java.util.Collections.emptyList())
		if hits.first.size() > 0
		prim = Set.empty ++ hits.first.asScala.map(_.toInt)
		sec = Map.empty ++ hits.second.asScala.map(p => p._1.toInt -> p._2)
		markers = MarkerUtil.convert2MarkersUnique(prim,sec, searchText)
		appended = markers._1.values.toList ::: markers._2.toList
		updateState = UpdateSnapshot(markers = appended,List.empty,searchText, Concrete(0))
		textRange = new TextRange(0, allText.length)
	} yield DTPPState(History(List.empty, updateState, List.empty), false, "", 0, allText, textRange, textRange)
	
	val randomState:Gen[DTPPState] = for {
		accept <- acceptStateGenerator
		update <- updateStateGenerator
		select <- selectStateGenerator
		states = List(accept,update,select)
		returned <- Gen.oneOf(states)
	} yield returned
	
	val searchUpdateWithMatch:Gen[(StringInput, DTPPState,String, String)] = for {
		text <- Gen.alphaStr
		if text.length > 0
		caretPos <- Gen.choose(0,text.length)
		subStringStart <- Gen.choose(0, text.length-1)
		subStringEnd <- Gen.choose(subStringStart, text.length)
		if subStringStart < subStringEnd
		search = text.substring(subStringStart,subStringEnd)
		initialState = UpdateSnapshot(List.empty,List.empty,"",Concrete(caretPos))
	} yield (
		StringInput(search),
		DTPPState(History(List.empty,initialState,List.empty),false,"",caretPos,text, new TextRange(0,text.length),new TextRange(0,text.length)),
		search, text)
	
	
	property("(update, stringinput) -> update with markers when search matches and match is not initial caret pos") =
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
	
	val searchUpdateWithOutMatch:Gen[(StringInput, DTPPState, String,String)] = for {
		text <- Gen.alphaLowerStr
		search <- Gen.alphaLowerStr
		if text.length > 0 && search.length > 0
		if !(text contains search)
		caretPos <- Gen.choose(0,text.length)
		initialState = UpdateSnapshot(List.empty,List.empty,"",Concrete(caretPos))
	} yield (
		StringInput(search),
		DTPPState(History(List.empty,initialState,List.empty),false,"",caretPos,text, new TextRange(0,text.length),new TextRange(0,text.length)),
		search, text)
	
	property("(update, stringinput) -> update when no matches") =
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
	
	val combinedWithNoMods:Gen[(DTPPState,StringInput)] = for {
		inp <- Gen.alphaChar
		state <- selectStateGenerator
	} yield (state,StringInput(inp.toString.toLowerCase))
	
	property("(select, stringinput) -> Accept || widen || no match") =
		forAll(combinedWithNoMods) { (inp: (DTPPState, StringInput)) =>
			inp match {
				case (state: DTPPState, input: StringInput) =>
					val res = Jump.update(state, input)
					res.history.present match {
						case _: AcceptSnapshot => true //case Accept
						case ss: SelectSnapshot => //case widen or no match
							input.value == Constants.markerAlphabet.last.toString || //case widen
							!ss.markers.exists(m => m.repText.toUpperCase == input.value.toUpperCase) //no match
						case _ => false
					}
			}
		}
	
	val combinedWithShift:Gen[(DTPPState,ShiftStringInput)] = for {
		inp <- Gen.alphaChar
		state <- selectStateGenerator
	} yield (state,ShiftStringInput(inp.toString.toLowerCase))
	
	property("(select, shiftstringinput) -> Accept || widen || no match") =
		forAll(combinedWithShift) { (inp: (DTPPState, ShiftStringInput)) =>
			inp match {
				case (state: DTPPState, input: ShiftStringInput) =>
					val res = Jump.update(state, input)
					res.history.present match {
						case _: AcceptSnapshot => true //case Accept
						case ss: SelectSnapshot => //case widen or no match
							input.value == Constants.markerAlphabet.last.toString || //case widen
							!ss.markers.exists(m => m.repText.toUpperCase == input.value.toUpperCase) //no match
						case _ => false
					}
			}
		}
	
	val combinedWithAlt:Gen[(DTPPState,AltStringInput)] = for {
		inp <- Gen.alphaChar
		state <- selectStateGenerator
	} yield (state,AltStringInput(inp.toString.toLowerCase))
	
	property("(select, altstringinput) ->  Accept || widen || no match") =
		forAll(combinedWithAlt) { (inp: (DTPPState, AltStringInput)) =>
			inp match {
				case (state: DTPPState, input: AltStringInput) =>
					val res = Jump.update(state, input)
					res.history.present match {
						case _: AcceptSnapshot => true //case Accept
						case ss: SelectSnapshot => //case widen or no match
							input.value == Constants.markerAlphabet.last.toString || //case widen
							!ss.markers.exists(m => m.repText.toUpperCase == input.value.toUpperCase) //no match
						case _ => false
					}
			}
		}
	
	property("(select, escape) -> update") =
		forAll(selectStateGenerator) { (i: DTPPState) => i match {
			case state: DTPPState =>
				val input = EscapeInput()
				val res = Jump.update(state,input)
				(res.history.present match {
					case _: UpdateSnapshot => true
					case _ => false
				}) &&
				res.history.future.head == state.history.present
		}}
	
	
	property("(update, enter) -> select") =
		forAll(updateStateGenerator) { (i: DTPPState) => i match {
			case state: DTPPState =>
				val input = EnterInput()
				val res = Jump.update(state,input)
				(res.history.present match {
					case _: SelectSnapshot => true
					case _ => false
				}) &&
				res.history.past.head == state.history.present
		}}
	
	property("(select, enter) -> widen") =
		forAll(selectStateGenerator) { (i: DTPPState) => i match {
			case state: DTPPState =>
				val input = EnterInput()
				val res = Jump.update(state,input)
				(res.history.present match {
					case _: SelectSnapshot => true
					case _ => false
				}) &&
				res.history.past.head == state.history.present
		}}
	
	val randomScrollWithRandomState:Gen[(DTPPInput,DTPPState)] = for {
		input <- randomScroll
		state <- randomState
	} yield (input,state)
	
	property("(x, scroll) -> x") =
		forAll(randomScrollWithRandomState) { (i: (DTPPInput,DTPPState)) => i match {
			case (input: DTPPInput,state: DTPPState) =>
				val res = Jump.update(state,input)
				(res.history.past == state.history.past) &&
				((res.history.present, state.history.present) match {
					case (_: UpdateSnapshot, _: UpdateSnapshot) => true
					case (_: SelectSnapshot, _: SelectSnapshot) => true
					case (_: AcceptSnapshot, _: AcceptSnapshot) => true
					case (_,_) => false
				})
		}}
}