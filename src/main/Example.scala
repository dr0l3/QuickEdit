package main

import java.awt.Graphics
import java.util.UUID
import javax.swing.JComponent

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.ui.popup.{JBPopup, JBPopupFactory}
import com.intellij.ui.JBColor
import com.intellij.ui.components.JBTextField
import main.ListenerType.ListenerType
import main.MarkerType.MarkerType
import main.ModifierCombination.ModifierCombination
import main.PluginState.PluginState
import util.{Constants, EditorUtil, MarkerUtil, PaintUtil}

import scala.collection.JavaConverters._
import scala.collection.mutable


/**
  * Created by dr0l3 on 4/6/17.
  */
class Example extends AnAction{
	override def actionPerformed(anActionEvent: AnActionEvent) {
		println("Startin the action")
		if(anActionEvent == null) return
		val editor = anActionEvent.getData(CommonDataKeys.EDITOR)
		val markerPanel = new MarkerPainter(editor)
		editor.getContentComponent.add(markerPanel)
		val effectExecutor = new EffectExecutor(editor)
		val stateInflaters = mutable.MutableList[StateInflater](markerPanel, effectExecutor)
		new BaseAction(editor, stateInflaters, State()).start(anActionEvent)
	}
}

trait DtppAction {
	def start(e: AnActionEvent)
	def receiveInput(input: Input)
}

class BaseAction(val editor: Editor, val stateInflaters: mutable.MutableList[StateInflater], val state: State) extends DtppAction{
	override def start(e: AnActionEvent) {
		val popups = new PopupInflater(editor, this)
		stateInflaters += popups
		val caretPos = editor.getCaretModel.getOffset
		state.snapshots = Snapshot(List.empty, List.empty, caretPos, caretPos, "", PluginState.UPDATE, 1) :: Nil
		stateInflaters.foreach(inf => inf.inflateState(state.snapshots.last))
	}
	
	override def receiveInput(input: Input) {
		println("input: " + input)
		val currentState = state.snapshots.head
		println("Currentstate: " + currentState)
		val newState = currentState.snapshotState match {
			case PluginState.UPDATE =>
				input match {
					case StringInput(search, _) =>
						if(search == ""){
							currentState.copy(markers = List.empty)
						} else {
							if(search == currentState.search) return
							val doc = EditorUtil.entireDocument(editor)
							val hits: List[Int] = EditorUtil.getMatchesForStringInTextRange(search, editor, doc).asScala.map(_.toInt).toList
							val sortedHits: List[Int] = hits.sortBy(offset => Math.abs(currentState.markerPaintCenter-offset))
							val markers = MarkerUtil.convertToMarkers(search, sortedHits, Constants.markerAlphabet, List.empty)
							currentState.copy(markers = markers)
						}
					case EnterInput(_) =>
						currentState.copy(snapshotState = PluginState.SELECTING)
					case EscapeInput(_) =>
						currentState.copy(snapshotState = PluginState.UNDO)
					case _ => currentState
				}
			case PluginState.SELECTING =>
				input match {
					case StringInput(search, _) =>
						val maybeMarkerHit = currentState.markers.find(mar => mar.repText == search.toUpperCase)
						if(maybeMarkerHit.isEmpty){
							currentState
						} else {
							val currentPosition = editor.getCaretModel.getOffset
							val effect = () => EditorUtil.performMove(maybeMarkerHit.get.start, editor)
							val undoer = () => EditorUtil.performMove(currentPosition, editor)
							val id = UUID.randomUUID().toString
							val tuple = (effect, undoer, id)
							currentState.copy(effects = List(tuple),snapshotState = PluginState.ACCEPT)
						}
					case EscapeInput(_) =>
						currentState.copy(snapshotState = PluginState.UNDO)
					case _ => currentState
				}
			case PluginState.ACCEPT =>
				input match {
					case AcceptInput() =>
						currentState.copy(snapshotState = PluginState.EXIT)
					case EscapeInput(_) =>
						currentState.copy(snapshotState = PluginState.UNDO)
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
	
	def listenerTypeToListener(missing: ListenerType, inputReceiver: DtppAction, editor: Editor, textField: JBTextField): Listener = {
		missing match {
			case ListenerType.NonAccept => new NonAccept(inputReceiver, editor)
			case ListenerType.NonChar => new NonCharListener(textField, inputReceiver)
			case ListenerType.SelectMarkersCharListener => new SelectMarkersCharListener(textField, inputReceiver)
			case ListenerType.UpdateMarkersCharListener => new Updater(inputReceiver, textField)
		}
	}
	
	override def inflateState(snapshot: Snapshot) {
		if(snapshot.snapshotState == PluginState.UPDATE){
			updatePopup()
			textField.setEditable(true)
			updateListeners(Constants.updateListeners)
		}
		
		if(snapshot.snapshotState == PluginState.SELECTING){
			updatePopup()
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
	
	private def updatePopup() = {
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
			markers = snapshot.markers
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
		markers.foreach(PaintUtil.paintMarker(_,editor, JBColor.GRAY, JBColor.WHITE, JBColor.RED, this, graphics))
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
case class State(var snapshots: List[Snapshot]= List.empty)
case class Snapshot(markers: List[Marker],
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

trait Input
case class AcceptInput() extends Input
case class StringInput(value: String, modifiers: ModifierCombination) extends Input
case class EnterInput(modifiers: ModifierCombination) extends Input
case class ScrollInput(modifiers: ModifierCombination) extends Input
case class EscapeInput(modifiers: ModifierCombination) extends Input
