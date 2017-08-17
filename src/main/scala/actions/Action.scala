package actions
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.editor.Editor
import state._
import util.{StartUtil, _}

import scala.collection.mutable


/**
  * Created by dr0l3 on 4/6/17.
  */
class JumpEntry extends AnAction{
	override def actionPerformed(anActionEvent: AnActionEvent) {
		println("Startin the action")
		val stateInflaters = StartUtil.createInflatersAndAddComponent(anActionEvent)
		val editor = anActionEvent.getData(CommonDataKeys.EDITOR)
		val startState = DTPPState.initialState(editor)
		println(startState)
		new BaseAction(editor, stateInflaters, startState, Jump).start(anActionEvent)
	}
}

class DeleteEntry extends AnAction {
	override def actionPerformed(anActionEvent: AnActionEvent) {
		val stateInflaters = StartUtil.createInflatersAndAddComponent(anActionEvent)
		val editor = anActionEvent.getData(CommonDataKeys.EDITOR)
		val startState = DTPPState.initialState(editor)
		new BaseAction(editor, stateInflaters, startState, Delete).start(anActionEvent)
	}
}

class CopyEntry extends AnAction {
	override def actionPerformed(anActionEvent: AnActionEvent) {
		val stateInflaters = StartUtil.createInflatersAndAddComponent(anActionEvent)
		val editor = anActionEvent.getData(CommonDataKeys.EDITOR)
		val startState = DTPPState.initialState(editor)
		new BaseAction(editor, stateInflaters, startState, Copy).start(anActionEvent)
	}
}

class CutEntry extends AnAction {
	override def actionPerformed(anActionEvent: AnActionEvent) {
		val stateInflaters = StartUtil.createInflatersAndAddComponent(anActionEvent)
		val editor = anActionEvent.getData(CommonDataKeys.EDITOR)
		val startState = DTPPState.initialState(editor)
		new BaseAction(editor, stateInflaters, startState, Cut).start(anActionEvent)
	}
}

trait DtppAction {
	def start(e: AnActionEvent)
	def receiveInput(input: DTPPInput)
}


class BaseAction(val editor: Editor,
                 val stateInflaters: mutable.MutableList[StateInflater],
                 var state: DTPPState,
                 val reducer: DTPPReducers) extends DtppAction{
	override def start(e: AnActionEvent) {
		val popups = new PopupInflater(editor, this)
		stateInflaters += popups
		val caretPos = editor.getCaretModel.getOffset
		stateInflaters.foreach(inf => inf.inflateState(state))
	}
	
	override def receiveInput(input: DTPPInput) {
		state = reducer.update(state,input)
		if(state.exiting){
			stateInflaters.foreach(inf => inf.deflateState())
		} else {
			stateInflaters.foreach(inf => inf.inflateState(state))
		}
	}
}

sealed trait SelectableMarker extends DTPPMarker{
	def start: Int
	def end: Int
	def text: String
	def repText: String
}
sealed trait DTPPMarker
case class PrimaryMarker(start: Int,
                         end: Int,
                         text: String,
                         repText: String) extends DTPPMarker with SelectableMarker
case class SecondaryMarker(start: Int, end: Int, text: String, repText: String) extends SelectableMarker
case class SelectedMarker(start: Int, end: Int, text: String) extends DTPPMarker



sealed trait DTPPInput
case class AcceptInput() extends DTPPInput
case class StringInput(value: String) extends DTPPInput
case class EnterInput() extends DTPPInput
case class ScrollUpInput() extends DTPPInput
case class ScrollDownInput() extends DTPPInput
case class ScrollHomeInput() extends DTPPInput
case class EscapeInput() extends DTPPInput
case class AltEscapeInput() extends DTPPInput
case class AltStringInput(value: String) extends DTPPInput
case class ShiftStringInput(value: String) extends DTPPInput
