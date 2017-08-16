package state

import java.awt.Graphics
import javax.swing.JComponent

import actions.ListenerType.ListenerType
import actions.{DtppAction, _}
import com.intellij.openapi.editor.{Editor, LogicalPosition, ScrollType}
import com.intellij.openapi.ui.popup.{JBPopup, JBPopupFactory}
import com.intellij.openapi.util.TextRange
import com.intellij.ui.components.JBTextField
import dtpp.util.EditorUtil
import util.{Constants, PaintUtil}

import scala.collection.mutable

/**
  * Created by dr0l3 on 4/9/17.
  */
trait StateInflater {
	def inflateState(state: DTPPState)
	
	def deflateState()
}

class PopupInflater(val editor: Editor, val inputReceiver: DtppAction) extends StateInflater {
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
	
	override def inflateState(state: DTPPState) {
		val overlays = state.history.present match {
			case _: AcceptSnapshot => 0
			case ss: SelectSnapshot => ss.selectedMarkers.size
			case ss: UpdateSnapshot => ss.selectedMarkers.size
		}
		val search = state.history.present match {
			case _: AcceptSnapshot => state.search
			case ss: SelectSnapshot => ss.search
			case ss: UpdateSnapshot => ss.search
		}
		if (overlayNumber != overlays) {
			listeners.foreach(lis => lis.unregister())
			listeners = List.empty
			textField = new JBTextField(search, 10)
			if (popup.isDefined) {
				popup.get.dispose()
			}
			popup = None
			overlayNumber = overlays
		}
		
		state.history.present match {
			case _: AcceptSnapshot =>
				if (popup.isDefined) {
					popup.get.dispose()
					popup = None
				}
				updateListeners(Constants.acceptListeners)
			case _: SelectSnapshot =>
				updatePopup(state.history.present)
				textField.setEditable(false)
				updateListeners(Constants.selectingListeners)
			case _: UpdateSnapshot =>
				updatePopup(state.history.present)
				textField.setEditable(true)
				updateListeners(Constants.updateListeners)
		}
		
	}
	
	private def updatePopup(snapshot: DTPPSnapshot) = {
		if (popup.isEmpty) {
			val point = JBPopupFactory.getInstance().guessBestPopupLocation(editor)
			popup = Some(JBPopupFactory.getInstance().createComponentPopupBuilder(textField, textField)
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
			if (!popup.get.isVisible) {
				popup.get.show(point)
			}
			if (!textField.hasFocus) {
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


class MarkerPainter(val editor: Editor, var markers: List[DTPPMarker] = List.empty) extends JComponent with StateInflater {
	override def inflateState(state: DTPPState) {
		state.history.present match {
			case _: AcceptSnapshot => markers = List.empty
			case ss: SelectSnapshot =>
				markers = ss.markers ::: ss.selectedMarkers
			case ss: UpdateSnapshot =>
				markers = ss.markers ::: ss.selectedMarkers
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
	private val delTextByEffect:mutable.Map[DTPPEffect, String] = scala.collection.mutable.Map()
	
	override def deflateState() = {
		editor.getMarkupModel.removeAllHighlighters()
	}
	
	override def inflateState(state: DTPPState) = {
		state.history.future.headOption.foreach {
			case AcceptSnapshot(eff) => undoEffect(eff)
			case _ => ()
		}
		
		state.history.present match {
			case AcceptSnapshot(eff) => executeEffect(eff)
			case _ => ()
		}
	}
	
	def executeEffect(effect: DTPPEffect) = {
		effect match {
			case JumpEffect(end, _) =>
				val endOffset = end.flatten(editor).offset
				EditorUtil.performMove(endOffset, editor)
			case eff: DeleteEffect =>
				val (startOffset, endOffset) = eff.validatedFlatten(editor)
				delTextByEffect.put(eff,EditorUtil.performDelete(startOffset, endOffset+1, editor))
			case eff: CutEffect =>
				val (startOffset, endOffset) = eff.validatedFlatten(editor)
				delTextByEffect.put(eff,EditorUtil.performCut(startOffset, endOffset+1, editor))
			case PasteEffect(start, text) =>
				val startOffset = start.flatten(editor).offset
				EditorUtil.performPaste(startOffset, editor, text)
			case eff: CopyEffect =>
				val (startOffset, endOffset) = eff.validatedFlatten(editor)
				EditorUtil.performCopy(startOffset, endOffset+1, editor)
		}
	}
	
	def undoEffect(effect: DTPPEffect) = {
		effect match {
			case JumpEffect(_, initial) =>
				val offset = initial.flatten(editor).offset
				EditorUtil.performMove(offset, editor)
			case eff: DeleteEffect =>
				val maybeText = delTextByEffect.remove(eff)
				maybeText.foreach(text => {
					val (offset,_) = eff.validatedFlatten(editor)
					EditorUtil.performPaste(offset,editor,text)
				})
			case eff: CutEffect=>
				val maybeText = delTextByEffect.remove(eff)
				maybeText.foreach(text => {
					val (offset,_) = eff.validatedFlatten(editor)
					EditorUtil.performPaste(offset,editor,text)
				})
			case eff: PasteEffect =>
				val startOffset = eff.start.flatten(editor).offset
				EditorUtil.performDelete(startOffset, startOffset + eff.text.length, editor)
			case CopyEffect(_, _) => ()
		}
	}
}

class ScrollInflater(val editor: Editor) extends StateInflater {
	override def deflateState() = {}
	
	override def inflateState(state: DTPPState) = {
		(state.history.present match {
			case ss: UpdateSnapshot => Some(ss.centerPoint)
			case ss: SelectSnapshot => Some(ss.centerPoint)
			case _: AcceptSnapshot => None
		}).map(centerPoint => {
			centerPoint.flatten(editor).offset
		}).flatMap(offset => {
			if (!EditorUtil.getVisibleTextRange(editor).contains(offset)) Some(editor.offsetToLogicalPosition(offset))
			else None
		}).foreach(pos => editor.getScrollingModel.scrollTo(pos, ScrollType.MAKE_VISIBLE))
	}
}

case class DTPPState(history: History[DTPPSnapshot],
                     exiting: Boolean,
                     search: String,
                     initialCaretPos: Int,
                     text: String,
                     allText: TextRange,
                     visibleText: TextRange)

case class History[A](past: List[A], present: A, future: List[A]) {
	def update(next: A): History[A] = {
		History(this.past,next,List.empty)
	}
	
	def advance(next: A): History[A] = {
		History(this.present :: this.past,next, List.empty)
	}
	
	def undo(): Option[History[A]] = {
		this.past.headOption
			.map(f => History(this.past.tail,f,this.present::this.future))
	}
	def redo(): Option[History[A]] = {
		this.future.headOption.map(fut => History(this.present::this.past,fut, this.future.tail))
	}
}

object DTPPState {
	def initialState(editor: Editor): DTPPState = {
		DTPPState(
			History(
				List.empty,
				UpdateSnapshot(List.empty, List.empty, "", Concrete(editor.getCaretModel.getPrimaryCaret.getOffset)),
				List.empty),
			exiting = false,
			search = "",
			editor.getCaretModel.getPrimaryCaret.getOffset,
			editor.getDocument.getCharsSequence.toString,
			EditorUtil.entireDocument(editor),
			EditorUtil.getVisibleTextRange(editor))
	}
}

sealed trait DTPPSnapshot
case class UpdateSnapshot(markers: List[SelectableMarker] = List.empty,
                          selectedMarkers: List[SelectedMarker] = List.empty,
                          search: String,
                          centerPoint: DTPPPoint) extends DTPPSnapshot
case class SelectSnapshot(markers: List[SelectableMarker] = List.empty,
                          selectedMarkers: List[SelectedMarker] = List.empty,
                          search: String,
                          centerPoint: DTPPPoint) extends DTPPSnapshot
case class AcceptSnapshot(effect: DTPPEffect) extends DTPPSnapshot

sealed trait DTPPEffect
sealed trait DoubleOverlayEffect extends DTPPEffect {
	def start: DTPPPoint
	def end: DTPPPoint
	def validatedFlatten(editor: Editor): (Int,Int) = {
		val startOffset = start.flatten(editor).offset
		val endOffset = end.flatten(editor).offset
		if(startOffset < endOffset) (startOffset,endOffset)
		else (endOffset, startOffset)
	}
}
case class JumpEffect(end: DTPPPoint, initial: DTPPPoint) extends DTPPEffect
case class DeleteEffect(start: DTPPPoint, end: DTPPPoint) extends DoubleOverlayEffect
case class CutEffect(start: DTPPPoint, end: DTPPPoint) extends DoubleOverlayEffect
case class CopyEffect(start: DTPPPoint, end: DTPPPoint) extends DoubleOverlayEffect
case class PasteEffect(start: DTPPPoint, text: String) extends DTPPEffect
case object ExitEffect extends DTPPEffect

sealed trait DTPPPoint {
	def flatten(editor: Editor): Concrete
}

case class Concrete(offset: Int) extends DTPPPoint {
	override def flatten(editor: Editor): Concrete = this
}

case class Relative(parent: DTPPPoint, extension: DTPPExtension) extends DTPPPoint {
	override def flatten(editor: Editor): Concrete = {
		val parOff = parent match {
			case rel: Relative => rel.flatten(editor).offset
			case Concrete(o) => o
		}
		val concreteExtension = extension match {
			case LineExtension(lines) =>
				val parentPosition = editor.offsetToLogicalPosition(parOff)
				val currentLine = parentPosition.line
				val nextLine = lines match {
					case pos if pos > 0 => Math.min(currentLine + pos, editor.getDocument.getLineCount)
					case neg if neg < 1 => Math.max(currentLine + neg, 0)
				}
				val currentPos = new LogicalPosition(nextLine, parentPosition.column)
				editor.logicalPositionToOffset(currentPos)
			case CharExtension(moves) =>
				parOff + moves
			case LineEnd =>
				editor.getDocument.getLineEndOffset(editor.getDocument.getLineNumber(parOff)) - parOff
			case LineStart =>
				parOff - editor.getDocument.getLineStartOffset(editor.getDocument.getLineNumber(parOff))
		}
		Concrete(parOff + concreteExtension)
	}
}


sealed trait DTPPExtension
case class LineExtension(lines: Int) extends DTPPExtension
case class CharExtension(moves: Int) extends DTPPExtension
case object LineEnd extends DTPPExtension
case object LineStart extends DTPPExtension
