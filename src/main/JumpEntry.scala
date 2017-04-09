package main

import java.awt.Graphics
import java.util.UUID
import javax.swing.JComponent

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.editor.{Editor, LogicalPosition, ScrollType}
import com.intellij.openapi.ui.popup.{JBPopup, JBPopupFactory}
import com.intellij.ui.components.JBTextField
import main.ListenerType.ListenerType
import main.MarkerType.MarkerType
import main.ModifierCombination.ModifierCombination
import main.PluginState.PluginState
import main.ScrollDirection.ScrollDirection
import util._

import scala.collection.mutable


/**
  * Created by dr0l3 on 4/6/17.
  */
class JumpEntry extends AnAction{
	override def actionPerformed(anActionEvent: AnActionEvent) {
		println("Startin the action")
		if(anActionEvent == null) return
		val editor = anActionEvent.getData(CommonDataKeys.EDITOR)
		val markerPanel = new MarkerPainter(editor)
		editor.getContentComponent.add(markerPanel)
		val effectExecutor = new EffectExecutor(editor)
		val scrollInflater = new ScrollInflater(editor)
		val stateInflaters = mutable.MutableList[StateInflater](markerPanel, effectExecutor, scrollInflater)
		
		new JumpAction(editor, stateInflaters, State()).start(anActionEvent)
	}
}

class DeleteEntry extends AnAction {
	override def actionPerformed(anActionEvent: AnActionEvent) {
		println("Startin the action")
		if(anActionEvent == null) return
		val editor = anActionEvent.getData(CommonDataKeys.EDITOR)
		val markerPanel = new MarkerPainter(editor)
		editor.getContentComponent.add(markerPanel)
		val effectExecutor = new EffectExecutor(editor)
		val scrollInflater = new ScrollInflater(editor)
		val stateInflaters = mutable.MutableList[StateInflater](markerPanel, effectExecutor, scrollInflater)
		
		new DeleteAction(editor, stateInflaters, State()).start(anActionEvent)
	}
}

trait DtppAction {
	def start(e: AnActionEvent)
	def receiveInput(input: Input)
}

class DeleteAction(val editor: Editor, val stateInflaters: mutable.MutableList[StateInflater], val state: State) extends DtppAction{
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
						Reducers.selectStringTwo(currentState, input.asInstanceOf[StringInput], editor)
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
		
		println("newState:" + newState)
		
		if((currentState.snapshotState == PluginState.UPDATE &&
			newState.snapshotState == PluginState.SELECTING) ||
			(currentState.snapshotState == PluginState.SELECTING &&
				newState.snapshotState == PluginState.ACCEPT) || (
			currentState.snapshotState == PluginState.SELECTING &&
			newState.snapshotState == PluginState.UPDATE)){
			state.snapshots = newState :: state.snapshots
		} else {
			state.snapshots = newState :: state.snapshots.tail
		}
		if(newState.snapshotState == PluginState.UNDO){
			if(state.snapshots.tail.isEmpty){
				state.snapshots = List(state.snapshots.head.copy(snapshotState = PluginState.EXIT))
			} else {
				state.snapshots = state.snapshots.tail
			}
		}
		
		if(state.snapshots.head.snapshotState != PluginState.EXIT){
			println("The stack:" + state.snapshots)
			println("inflating: " + state.snapshots.head)
			stateInflaters.foreach(inf => inf.inflateState(state.snapshots.head))
		} else {
			println("Exit")
			stateInflaters.foreach(inf => inf.deflateState())
		}
	}
}

class JumpAction(val editor: Editor, val stateInflaters: mutable.MutableList[StateInflater], val state: State) extends DtppAction{
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
						Reducers.selectString(currentState, input.asInstanceOf[StringInput], editor)
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
		
		println("newState:" + newState)
		
		if((currentState.snapshotState == PluginState.UPDATE &&
			newState.snapshotState == PluginState.SELECTING) ||
			(currentState.snapshotState == PluginState.SELECTING &&
			newState.snapshotState == PluginState.ACCEPT)){
			state.snapshots = newState :: state.snapshots
		} else {
			state.snapshots = newState :: state.snapshots.tail
		}
		if(newState.snapshotState == PluginState.UNDO){
			if(state.snapshots.tail.isEmpty){
				state.snapshots = List(state.snapshots.head.copy(snapshotState = PluginState.EXIT))
			} else {
				state.snapshots = state.snapshots.tail
			}
		}
		
		if(state.snapshots.head.snapshotState != PluginState.EXIT){
			println("The stack:" + state.snapshots)
			println("inflating: " + state.snapshots.head)
			stateInflaters.foreach(inf => inf.inflateState(state.snapshots.head))
		} else {
			println("Exit")
			stateInflaters.foreach(inf => inf.deflateState())
		}
	}
}

case class Marker(start: Int, end: Int, orgText: String, repText: String, mType: MarkerType)

object MarkerType extends Enumeration {
	type MarkerType = Value
	val SELECTED, PRIMARY, SECONDARY = Value
}

trait StateInflater {
	def inflateState(snapshot: Snapshot)
	def deflateState()
}

class PopupInflater(val editor: Editor, val inputReceiver: DtppAction) extends StateInflater{
	var textField: JBTextField = new JBTextField("", 10)
	var popup: Option[JBPopup] = None
	var listeners: List[Listener] = List.empty[Listener]
	var overlayNumber = 1
	
	def listenerTypeToListener(missing: ListenerType, inputReceiver: DtppAction, editor: Editor, textField: JBTextField): Listener = {
		missing match {
			case ListenerType.NonAccept => new NonAccept(inputReceiver, editor)
			case ListenerType.NonChar => new NonCharListener(textField, inputReceiver)
			case ListenerType.SelectMarkersCharListener => new SelectMarkersCharListener(textField, inputReceiver)
			case ListenerType.UpdateMarkersCharListener => new Updater(inputReceiver, textField)
		}
	}
	
	override def inflateState(snapshot: Snapshot) {
		if(overlayNumber != snapshot.overlays){
			listeners.foreach(lis => lis.unregister())
			listeners = List.empty
			textField = new JBTextField(snapshot.search, 10)
			if(popup.isDefined){
				popup.get.dispose()
			}
			popup = None
			overlayNumber = snapshot.overlays
		}
		
		if(snapshot.snapshotState == PluginState.UPDATE){
			updatePopup(snapshot)
			textField.setEditable(true)
			updateListeners(Constants.updateListeners)
		}
		
		if(snapshot.snapshotState == PluginState.SELECTING){
			updatePopup(snapshot)
			textField.setEditable(false)
			updateListeners(Constants.selectingListeners)
		}
		
		if(snapshot.snapshotState == PluginState.ACCEPT){
			if(popup.isDefined){
				popup.get.dispose()
				popup = None
			}
			updateListeners(Constants.acceptListeners)
		}
	}
	
	private def updatePopup(snapshot: Snapshot) = {
		if(popup.isEmpty){
			val point = JBPopupFactory.getInstance().guessBestPopupLocation(editor)
			popup = Some(JBPopupFactory.getInstance().createComponentPopupBuilder(textField,textField)
				.setCancelKeyEnabled(false)
				.setFocusable(true)
				.setMovable(false)
				.setShowBorder(true)
				.setRequestFocus(true)
				.createPopup())
			popup.get.show(point)
		} else {
			val point = JBPopupFactory.getInstance().guessBestPopupLocation(editor)
			if(!popup.get.isVisible){
				popup.get.show(point)
			}
			if(!textField.hasFocus) {
				textField.requestFocus()
			}
			
		}
	}
	
	private def updateListeners(requiredListeners: List[ListenerType]): Unit = {
		val unwantedListeners = listeners.filterNot(lis => requiredListeners.contains(lis.getType))
		unwantedListeners.foreach(lis => lis.unregister())
		listeners = listeners.filterNot(lis => unwantedListeners.contains(lis))
		//add listeners that are supposed to be there
		val missingListenerTypes = requiredListeners.filterNot(lisType => listeners.map(lis => lis.getType).contains(lisType))
		val newListeners: List[Listener] = missingListenerTypes.map(missing => listenerTypeToListener(missing, inputReceiver, editor, textField))
		newListeners.foreach(lis => lis.register())
		listeners = newListeners ::: listeners
	}
	
	override def deflateState() = {
		listeners.foreach(lis => lis.unregister())
		JBPopupFactory.getInstance().getChildPopups(editor.getComponent).forEach(p => p.dispose())
	}
}


class MarkerPainter(val editor: Editor, var markers: List[Marker] = List.empty) extends JComponent with StateInflater{
	override def inflateState(snapshot: Snapshot) {
		//println("markers " + snapshot.markers.length)
		if(snapshot.snapshotState == PluginState.ACCEPT){
			markers = List.empty
		} else {
			val nonSelectedMarkers = snapshot.markers.filterNot(snapshot.selectedMarkers.toSet)
			markers = nonSelectedMarkers ::: snapshot.selectedMarkers
		}
		paintComponent(editor.getContentComponent.getGraphics)
		repaint()
	}
	
	override def deflateState() {
		editor.getContentComponent.remove(this)
	}
	
	override def paintComponent(graphics: Graphics): Unit = {
		super.paintComponent(graphics)
		PaintUtil.setupLocationAndBoundsOfPanel(editor, this)
		PaintUtil.paintMarkers(markers, editor, this, graphics)
	}
}

class EffectExecutor(val editor: Editor) extends StateInflater {
	var executedEffects: List[(() => Unit, () => Unit, String)] = List.empty
	override def inflateState(snapshot: Snapshot): Unit = {
		val toExecute = snapshot.effects.filterNot(effect => executedEffects.map(t => t._3).contains(effect._3))
		val toUndo = executedEffects.filterNot(effect => snapshot.effects.map(t => t._3).contains(effect._3))
		executedEffects = toExecute ::: executedEffects
		executedEffects = executedEffects.filterNot(eff => toUndo.map(t => t._3).contains(eff._3))
		toExecute.foreach(tuple => tuple._1())
		toUndo.foreach(tuple => tuple._2())
	}
	
	override def deflateState() = {}
}

class ScrollInflater(val editor: Editor) extends StateInflater{
	override def inflateState(snapshot: Snapshot): Unit = {
		val logPos: LogicalPosition = editor.offsetToLogicalPosition(snapshot.markerPaintCenter)
		editor.getScrollingModel.scrollTo(logPos, ScrollType.MAKE_VISIBLE)
	}
	
	override def deflateState() = {}
}

case class State(var snapshots: List[Snapshot]= List.empty)
case class Snapshot(markers: List[Marker],
                   selectedMarkers: List[Marker],
                   effects: List[(() => Unit, () => Unit, String)],
                   initialCaretPos: Int,
                   markerPaintCenter: Int,
                   search: String,
                   snapshotState: PluginState,
                   overlays: Int)

object PluginState extends Enumeration{
	type PluginState = Value
	val UPDATE, SELECTING, ACCEPT, EXIT, UNDO = Value
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
