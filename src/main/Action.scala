package main

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.util.TextRange
import main.MarkerType.MarkerType
import main.ModifierCombination.ModifierCombination
import main.ScrollDirection.ScrollDirection
import state._
import util._

import scala.collection.mutable


/**
  * Created by dr0l3 on 4/6/17.
  */
class JumpEntry extends AnAction{
	override def actionPerformed(anActionEvent: AnActionEvent) {
		println("Startin the action")
		val stateInflaters = StartUtil.createInflatersAndAddComponent(anActionEvent)
		val editor = anActionEvent.getData(CommonDataKeys.EDITOR)
		val effectCreator = (offset: Int, editor: Editor) => () => EditorUtil.performMove(offset, editor)
		new SingleOverlayAction(editor, stateInflaters, State(), effectCreator, effectCreator).start(anActionEvent)
	}
}

class DeleteEntry extends AnAction {
	override def actionPerformed(anActionEvent: AnActionEvent) {
		val stateInflaters = StartUtil.createInflatersAndAddComponent(anActionEvent)
		val editor = anActionEvent.getData(CommonDataKeys.EDITOR)
		val effectCreator: (Int, Int, Editor) => () => Unit = (off1: Int, off2: Int, editor: Editor) => () => EditorUtil.performDelete(off1,off2, editor)
		val undoCreator: (Int, Int, Editor) => () => Unit = (off1: Int, off2: Int, editor: Editor) => {
			val textRange = new TextRange(off1, off2)
			val text = editor.getDocument.getText(textRange)
			() => EditorUtil.performPaste(off1, editor, text)
		}
		new TwoOverlayAction(editor, stateInflaters, State(), effectCreator, undoCreator).start(anActionEvent)
	}
}

class CopyEntry extends AnAction {
	override def actionPerformed(anActionEvent: AnActionEvent) {
		val stateInflaters = StartUtil.createInflatersAndAddComponent(anActionEvent)
		val editor = anActionEvent.getData(CommonDataKeys.EDITOR)
		val effectCreator: (Int, Int, Editor) => () => Unit = (off1: Int, off2: Int, editor: Editor) => () => {
			EditorUtil.performMarkRange(off1, off2, editor)
			EditorUtil.performCopy(off1,off2, editor)
		}
		val undoCreator: (Int, Int, Editor) => () => Unit = (off1: Int, off2: Int, editor: Editor) => () => Unit
		new TwoOverlayAction(editor, stateInflaters, State(), effectCreator, undoCreator).start(anActionEvent)
	}
}

class CutEntry extends AnAction {
	override def actionPerformed(anActionEvent: AnActionEvent) {
		val stateInflaters = StartUtil.createInflatersAndAddComponent(anActionEvent)
		val editor = anActionEvent.getData(CommonDataKeys.EDITOR)
		val effectCreator: (Int, Int, Editor) => () => Unit = (off1: Int, off2: Int, editor: Editor) => () => EditorUtil.performCut(off1,off2, editor)
		val undoCreator: (Int, Int, Editor) => () => Unit = (off1: Int, off2: Int, editor: Editor) => {
			val textRange = new TextRange(off1, off2)
			val text = editor.getDocument.getText(textRange)
			() => EditorUtil.performPaste(off1, editor, text)
		}
		new TwoOverlayAction(editor, stateInflaters, State(), effectCreator, undoCreator).start(anActionEvent)
	}
}

trait DtppAction {
	def start(e: AnActionEvent)
	def receiveInput(input: Input)
}

class TwoOverlayAction(val editor: Editor,
                       val stateInflaters: mutable.MutableList[StateInflater],
                       var state: State,
                       val effectCreator: (Int, Int, Editor) => () => Unit,
                       val undoCreator: (Int, Int, Editor) => () => Unit) extends DtppAction{
	override def start(e: AnActionEvent): Unit = {
		val popups = new PopupInflater(editor, this)
		stateInflaters += popups
		val caretPos = editor.getCaretModel.getOffset
		state.snapshots = Snapshot(List.empty,List.empty, List.empty, caretPos, caretPos, "", PluginState.UPDATE, 1) :: Nil
		stateInflaters.foreach(inf => inf.inflateState(state.snapshots.last))
	}
	
	override def receiveInput(input: Input): Unit = {
		println("input: " + input)
		val currentState = state.snapshots.head
		println("Currentstate: " + currentState)
		val newState = currentState.snapshotState match {
			case PluginState.UPDATE =>
				input match {
					case StringInput(_, _) =>
						Reducers.updateString(currentState, input.asInstanceOf[StringInput], editor)
					case EnterInput(_) =>
						Reducers.updateEnter(currentState, input.asInstanceOf[EnterInput], editor)
					case EscapeInput(_) =>
						Reducers.updateEscape(currentState, input.asInstanceOf[EscapeInput], editor)
					case ScrollInput(_, _) =>
						Reducers.updateScroll(currentState, input.asInstanceOf[ScrollInput], editor)
					case _ => currentState
				}
			case PluginState.SELECTING =>
				input match {
					case StringInput(_, _) =>
						Reducers.selectStringTwo(currentState, input.asInstanceOf[StringInput], editor, effectCreator, undoCreator)
					case EscapeInput(_) =>
						Reducers.selectEscape(currentState, input.asInstanceOf[EscapeInput], editor)
					case _ => currentState
				}
			case PluginState.ACCEPT =>
				input match {
					case AcceptInput() =>
						Reducers.acceptAccept(currentState, input.asInstanceOf[AcceptInput], editor)
					case EscapeInput(_) =>
						Reducers.acceptEscape(currentState, input.asInstanceOf[EscapeInput], editor)
					case _ => currentState
				}
			case _ => currentState
		}
		
		state = ActionUtil.handleNewState(currentState, newState, state, stateInflaters)
	}
}

class SingleOverlayAction(val editor: Editor,
                          val stateInflaters: mutable.MutableList[StateInflater],
                          var state: State,
                          val effectCreator: (Int, Editor) => () => Unit,
                          val undoCreator: (Int, Editor) => () => Unit) extends DtppAction{
	override def start(e: AnActionEvent) {
		val popups = new PopupInflater(editor, this)
		stateInflaters += popups
		val caretPos = editor.getCaretModel.getOffset
		state.snapshots = Snapshot(List.empty, List.empty, List.empty, caretPos, caretPos, "", PluginState.UPDATE, 1) :: Nil
		stateInflaters.foreach(inf => inf.inflateState(state.snapshots.last))
	}
	
	override def receiveInput(input: Input) {
		println("input: " + input)
		val currentState = state.snapshots.head
		println("Currentstate: " + currentState)
		val newState = currentState.snapshotState match {
			case PluginState.UPDATE =>
				input match {
					case StringInput(_, _) =>
						Reducers.updateString(currentState, input.asInstanceOf[StringInput], editor)
					case EnterInput(_) =>
						Reducers.updateEnter(currentState, input.asInstanceOf[EnterInput], editor)
					case EscapeInput(_) =>
						Reducers.updateEscape(currentState, input.asInstanceOf[EscapeInput], editor)
					case ScrollInput(_, _) =>
						Reducers.updateScroll(currentState, input.asInstanceOf[ScrollInput], editor)
					case _ => currentState
				}
			case PluginState.SELECTING =>
				input match {
					case StringInput(_, _) =>
						Reducers.selectString(currentState, input.asInstanceOf[StringInput], editor, effectCreator, undoCreator)
					case EscapeInput(_) =>
						Reducers.selectEscape(currentState, input.asInstanceOf[EscapeInput], editor)
					case _ => currentState
				}
			case PluginState.ACCEPT =>
				input match {
					case AcceptInput() =>
						Reducers.acceptAccept(currentState, input.asInstanceOf[AcceptInput], editor)
					case EscapeInput(_) =>
						Reducers.acceptEscape(currentState, input.asInstanceOf[EscapeInput], editor)
					case _ => currentState
				}
			case _ => currentState
		}
		
		state = ActionUtil.handleNewState(currentState, newState, state, stateInflaters)
	}
}

case class Marker(start: Int, end: Int, orgText: String, repText: String, mType: MarkerType)

object MarkerType extends Enumeration {
	type MarkerType = Value
	val SELECTED, PRIMARY, SECONDARY = Value
}

object ModifierCombination extends Enumeration {
	type ModifierCombination = Value
	val NONE, ALT, CTRL, ALT_CTRL, SHIFT = Value
}

object ScrollDirection extends Enumeration {
	type ScrollDirection = Value
	val UP, DOWN, HOME = Value
}

trait Input
case class AcceptInput() extends Input
case class StringInput(value: String, modifiers: ModifierCombination) extends Input
case class EnterInput(modifiers: ModifierCombination) extends Input
case class ScrollInput(dir: ScrollDirection, mods: ModifierCombination) extends Input
case class EscapeInput(modifiers: ModifierCombination) extends Input
