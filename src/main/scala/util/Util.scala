package util

import java.awt.geom.Rectangle2D
import java.awt.{Graphics, Rectangle}
import javax.swing.JComponent

import actions._
import com.intellij.openapi.actionSystem.{AnActionEvent, CommonDataKeys}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.colors.EditorFontType
import com.intellij.ui.JBColor
import dtpp.util.EditorUtil
import state._

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * Created by dr0l3 on 4/6/17.
  */


object StartUtil {
	def createInflatersAndAddComponent(anActionEvent: AnActionEvent): mutable.MutableList[StateInflater] = {
		if(anActionEvent == null)
			mutable.MutableList.empty
		else {
			val editor = anActionEvent.getData(CommonDataKeys.EDITOR)
			val markerPanel = new MarkerPainter(editor)
			editor.getContentComponent.add(markerPanel)
			val effectExecutor = new EffectExecutor(editor)
			val scrollInflater = new ScrollInflater(editor)
			val stateInflaters = mutable.MutableList[StateInflater](markerPanel, effectExecutor, scrollInflater)
			stateInflaters
		}
	}
}

trait DTPPReducers {
	def endGame(marker: PrimaryMarker, state: DTPPState, current: SelectSnapshot): DTPPSnapshot
	
	def update(state: DTPPState, input: DTPPInput): DTPPState = {
		//clear
		val res = (input, state.history.present) match {
			case (AcceptInput(), AcceptSnapshot(_)) => state.copy(exiting = true)
			
			case (StringInput(value), current: UpdateSnapshot) =>
				val text = state.text
				val ignoredOffsets = List(state.initialCaretPos)
				val hits: List[Int] = EditorUtil.getMatchesForStringInText(value, text, ignoredOffsets.map(i => i:java.lang.Integer).asJava).asScala.map(_.toInt).toList
				val paintCenterPresent = current.centerPoint match {
					case Concrete(offset) => offset
					case _ => state.initialCaretPos
				}
				val sortedHits: List[Int] = hits.sortBy(offset => Math.abs(paintCenterPresent-offset))
				val markers = MarkerUtil.convertToDTPPMarkers(value, sortedHits, Constants.markerAlphabet)
				sortedHits.headOption.map(head => {
					val paintCenterNext = if(state.visibleText.contains(head)) paintCenterPresent else head
					val updatedSnapshot = current.copy(markers = markers, centerPoint = Concrete(paintCenterNext))
					state.copy(history = state.history.update(updatedSnapshot))
				}).getOrElse({
					val next = current.copy(markers = List.empty)
					state.copy(history = state.history.update(next))})
				
			case (StringInput(value), current: SelectSnapshot) =>
				current.markers
					.find(mar => mar.repText.toUpperCase == value.toUpperCase)
					.map {
						case marker: PrimaryMarker => endGame(marker,state, current)
						case _: SecondaryMarker => widen(current,state)}
					.map(next => state.copy(history = state.history.advance(next)))
					.getOrElse(state)
				
			case (EnterInput(), current: UpdateSnapshot) =>
				val next = SelectSnapshot(current.markers, current.selectedMarkers,current.search, current.centerPoint)
				state.copy(history = state.history.advance(next))
				
			case (EnterInput(), current: SelectSnapshot) =>
				val next = widen(current, state)
				state.copy(history = state.history.advance(next))
				
			case (ScrollUpInput(), current: UpdateSnapshot)  =>
				val nextPoint = Relative(current.centerPoint, LineExtension(15))
				val next = current.copy(centerPoint = nextPoint)
				state.copy(history = state.history.update(next))
				
			case (ScrollUpInput(), current: SelectSnapshot)  =>
				val nextPoint = Relative(current.centerPoint, LineExtension(15))
				val next = current.copy(centerPoint = nextPoint)
				state.copy(history = state.history.update(next))
			case (ScrollDownInput(), current: UpdateSnapshot)  =>
				val nextPoint = Relative(current.centerPoint, LineExtension(-15))
				val next = current.copy(centerPoint = nextPoint)
				state.copy(history = state.history.update(next))
				
			case (ScrollDownInput(), current: SelectSnapshot)  =>
				val nextPoint = Relative(current.centerPoint, LineExtension(-15))
				val next = current.copy(centerPoint = nextPoint)
				state.copy(history = state.history.update(next))
				
			case (ScrollHomeInput(), current: UpdateSnapshot)  =>
				val next = current.copy(centerPoint = Concrete(state.initialCaretPos))
				state.copy(history = state.history.update(next))
				
			case (ScrollHomeInput(), current: SelectSnapshot)  =>
				val next = current.copy(centerPoint = Concrete(state.initialCaretPos))
				state.copy(history = state.history.update(next))
				
			case (EscapeInput(), _) =>
				state.history.undo()
					.map(nextHist => state.copy(history = nextHist))
			    	.getOrElse(state.copy(exiting = true))
				
			case (AltEscapeInput(),_ ) => state.copy(exiting = true)
			
			case (_, _) =>
				state
		}
		res
	}
	
	private def widen(current: SelectSnapshot, state: DTPPState) = {
		val secMarkers = current.markers.filter {
			case _: PrimaryMarker => false
			case _: SecondaryMarker => true
		}
		val hits = secMarkers.map(marker => marker.start)
		val markerPaintCenter = current.centerPoint match {
			case Concrete(offset) => offset
			case _ => state.initialCaretPos
		}
		val sortedHits: List[Int] = hits.sortBy(offset => Math.abs(markerPaintCenter - offset))
		val markers = MarkerUtil.convertToDTPPMarkers(state.search, sortedHits, Constants.markerAlphabet)
		current.copy(markers = markers)
	}
}

case object Jump extends DTPPReducers {
	override def endGame(marker: PrimaryMarker, state: DTPPState, current: SelectSnapshot): DTPPSnapshot = {
		AcceptSnapshot(JumpEffect(marker.start, state.initialCaretPos))
	}
}

case object Delete extends DTPPReducers {
	override def endGame(marker: PrimaryMarker, state: DTPPState, current: SelectSnapshot): DTPPSnapshot = {
		current.selectedMarkers.size match {
			case 0 =>
				val selectedMarker = SelectedMarker(marker.start,marker.end,marker.text)
				UpdateSnapshot(List.empty, List(selectedMarker),"",Concrete(state.initialCaretPos))
			case 1 =>
				val start = current.selectedMarkers.head.start
				val end = marker.start
				val text =
					if(start < end) state.text.substring(start,end)
					else state.text.substring(end,start)
				AcceptSnapshot(DeleteEffect(start, end, text).validate())
			case _ => state.history.present
		}
	}
}

case object Cut extends DTPPReducers {
	override def endGame(marker: PrimaryMarker, state: DTPPState, current: SelectSnapshot): DTPPSnapshot = {
		current.selectedMarkers.size match {
			case 0 =>
				val selectedMarker = SelectedMarker(marker.start,marker.end,marker.text)
				UpdateSnapshot(List.empty, List(selectedMarker),"",Concrete(state.initialCaretPos))
			case 1 =>
				val start = current.selectedMarkers.head.start
				val end = marker.start
				val text =
					if(start < end) state.text.substring(start,end)
					else state.text.substring(end,start)
				AcceptSnapshot(CutEffect(start, end, text).validate())
			case _ => state.history.present
		}
	}
}

case object Copy extends DTPPReducers {
	override def endGame(marker: PrimaryMarker, state: DTPPState, current: SelectSnapshot): DTPPSnapshot = {
		current.selectedMarkers.size match {
			case 0 =>
				val selectedMarker = SelectedMarker(marker.start,marker.end,marker.text)
				UpdateSnapshot(List.empty, List(selectedMarker),"",Concrete(state.initialCaretPos))
			case 1 =>
				val start = current.selectedMarkers.head.start
				val end = marker.start
				AcceptSnapshot(CopyEffect(start, end))
			case _ => state.history.present
		}
	}
}

object Constants {
	def markerAlphabet = "asdfwerhjkltcvbyuiopågæøxnmz".toUpperCase()
	def updateListeners = List(ListenerType.UpdateMarkersCharListener, ListenerType.NonChar)
	def selectingListeners = List(ListenerType.SelectMarkersCharListener, ListenerType.NonChar)
	def acceptListeners = List(ListenerType.NonAccept)
	def scrollLines = 20
}

object MarkerUtil {
	def convertToDTPPMarkers(search: String, hits: List[Int], markerAlphabet: String) = {
		def inner(search: String, hits: List[Int], markerAlphabet: String, acc: List[SelectableMarker]): List[SelectableMarker] = {
			val hit = if(hits.isEmpty) return acc else hits.head
			val markerChar = markerAlphabet.head
			val nextMarker =
				if(markerAlphabet.length > 1) PrimaryMarker(hit, hit+search.length, search, markerChar.toString)
				else SecondaryMarker(hit,hit+search.length,search, markerChar.toString)
			val added = nextMarker :: acc
			val newAlphabet = if(markerAlphabet.length > 1) markerAlphabet.drop(1) else markerAlphabet
			inner(search, hits.drop(1), newAlphabet, added)
		}
		inner(search,hits,markerAlphabet,List.empty)
	}
}

object PaintUtil {
	def setupLocationAndBoundsOfPanel(editor: Editor, component: JComponent): Unit = {
		val visibleArea: Rectangle = editor.getScrollingModel.getVisibleAreaOnScrollingFinished
		val parent = editor.getContentComponent
		component.setLocation(visibleArea.getX.toInt, visibleArea.getY.toInt)
		component.setBounds(0, 0, parent.getSize().width, parent.getSize().height)
		component.invalidate()
	}
	
	def paintMarkers(markers: List[DTPPMarker], editor: Editor, markerPanel: JComponent, graphics: Graphics): Unit = {
		markers.foreach {
			case marker: PrimaryMarker => paintMarker(marker, editor, backgroundColor = JBColor.GRAY, textColor = JBColor.WHITE, replacementTextColor = JBColor.RED, markerPanel, graphics)
			case marker: SelectedMarker => paintSelectedMarker(marker, editor, backgroundColor = JBColor.BLACK, textColor = JBColor.WHITE, markerPanel, graphics)
			case marker: SecondaryMarker => paintMarker(marker, editor, backgroundColor = JBColor.GRAY, textColor = JBColor.WHITE, replacementTextColor = JBColor.BLACK, markerPanel, graphics)
		}
	}
	
	def paintMarker(marker: SelectableMarker, editor: Editor, backgroundColor: JBColor, textColor: JBColor, replacementTextColor: JBColor, markerPanel: JComponent, graphics: Graphics): Unit ={
		drawBackground(editor, backgroundColor, textColor, marker, markerPanel, graphics)
		drawMarkerChar(editor, marker, replacementTextColor, markerPanel, graphics)
	}
	
	def paintSelectedMarker(marker: SelectedMarker, editor: Editor, backgroundColor: JBColor, textColor: JBColor, markerPanel: JComponent, graphics: Graphics): Unit ={
		drawSelectedChar(editor, marker, backgroundColor, textColor, markerPanel, graphics)
	}
	
	def drawBackground(editor: Editor, backgroundColor: JBColor, textColor: JBColor, marker: SelectableMarker, markerPanel: JComponent, graphics: Graphics): Unit = {
		val font = editor.getColorsScheme getFont EditorFontType.BOLD
		val fontRect = markerPanel.getFontMetrics(font).getStringBounds(marker.text, graphics)
		graphics setColor backgroundColor
		graphics setFont font
		val x = markerPanel.getX + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.start)).getX
		val y = markerPanel.getY + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.end)).getY
		graphics.fillRect(x.toInt,y.toInt,fontRect.getWidth.toInt, fontRect.getHeight.toInt)
		if(marker.text.length > 1){
			graphics.setColor(textColor)
			val x_text = markerPanel.getX + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.start+1)).getX
			val y_text = markerPanel.getY + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.end)).getY
			val bottomYOfMarkerChar = y_text + font.getSize
			graphics.drawString(marker.text.substring(1), x_text.toInt, bottomYOfMarkerChar.toInt)
		}
	}
	
	def drawMarkerChar(editor: Editor, marker: SelectableMarker, textColor: JBColor, markerPanel: JComponent, graphics: Graphics): Unit = {
		val font = editor.getColorsScheme.getFont(EditorFontType.BOLD)
		val x = markerPanel.getX + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.start)).getX
		val y = markerPanel.getY + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.end)).getY
		val bottomYOfMarkerChar = y + font.getSize
		graphics.setColor(textColor)
		graphics.setFont(font)
		graphics.drawString(marker.repText, x.toInt, bottomYOfMarkerChar.toInt)
	}
	
	def drawSelectedChar(editor: Editor, marker: SelectedMarker,backgroundColor: JBColor, textColor: JBColor, markerPanel: JComponent, graphics: Graphics): Unit ={
		val font = editor.getColorsScheme.getFont(EditorFontType.BOLD)
		val x = markerPanel.getX + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.start)).getX
		val y = markerPanel.getY + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.start+1)).getY
		val bottomYOfMarkerChar = y + font.getSize
		graphics.setFont(font)
		val fontRect: Rectangle2D = markerPanel.getFontMetrics(font).getStringBounds(marker.text.charAt(0).toString, graphics)
		graphics.setColor(backgroundColor)
		graphics.fillRect(x.toInt,y.toInt,fontRect.getWidth.toInt, fontRect.getHeight.toInt)
		graphics.setColor(textColor)
		graphics.drawString(marker.text.charAt(0).toString, x.toInt, bottomYOfMarkerChar.toInt)
	}
}
