package state

import java.awt.Graphics
import javax.swing.JComponent

import actions.DtppAction
import com.intellij.openapi.editor.{Editor, LogicalPosition, ScrollType}
import com.intellij.openapi.ui.popup.{JBPopup, JBPopupFactory}
import com.intellij.ui.components.JBTextField

import actions.ListenerType.ListenerType
import state.PluginState.PluginState
import util.{Constants, PaintUtil}

import actions._

/**
  * Created by dr0l3 on 4/9/17.
  */
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
				.setCancelOnClickOutside(false)
				.setCancelOnOtherWindowOpen(false)
				.setCancelOnWindowDeactivation(false)
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
	
	override def deflateState() = {
		editor.getMarkupModel.removeAllHighlighters()
	}
}

class ScrollInflater(val editor: Editor) extends StateInflater{
	override def inflateState(snapshot: Snapshot): Unit = {
		val logPos: LogicalPosition = editor.offsetToLogicalPosition(snapshot.markerPaintCenter)
		editor.getScrollingModel.scrollTo(logPos, ScrollType.MAKE_VISIBLE)
	}
	
	override def deflateState() = {}
}

case class State(var snapshots: List[Snapshot] = List.empty)
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
