package util

import java.awt.{Graphics, Rectangle}
import java.awt.geom.Rectangle2D

import dtpp.util.EditorUtil
import java.util.UUID
import javax.swing.JComponent

import actions.DtppAction.{EffCreatorOneOL, EffCreatorTwoOL, UndoCreatorOneOL, UndoCreatorTwoOL}
import com.intellij.openapi.actionSystem.{AnActionEvent, CommonDataKeys}
import com.intellij.openapi.editor.{Editor, LogicalPosition}
import com.intellij.openapi.editor.colors.EditorFontType
import com.intellij.ui.JBColor
import actions._
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

object ActionUtil {
	def handleNewState(currentState: Snapshot, newState: Snapshot, state: State, stateInflaters: mutable.MutableList[StateInflater]): State = {
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
		state
	}
}

object Reducers {
	def updateString(currentState: Snapshot, input: StringInput, editor: Editor) = {
		val search = input.value
		if(search == ""){
			currentState.copy(markers = List.empty)
		} else {
			if(search == currentState.search)
				currentState
			else {
				val text = editor.getDocument.getText
				val ignoredOffsets = currentState.initialCaretPos :: currentState.selectedMarkers.map(m => m.start)
				val hits: List[Int] = EditorUtil.getMatchesForStringInText(search, text, ignoredOffsets.map(i => i:java.lang.Integer).asJava).asScala.map(_.toInt).toList
				val sortedHits: List[Int] = hits.sortBy(offset => Math.abs(currentState.markerPaintCenter-offset))
				val closestHitOrInitial = sortedHits.headOption.getOrElse(currentState.markerPaintCenter)
				val closestHitAsPoint = editor.offsetToXY(closestHitOrInitial)
				val visibleArea = editor.getScrollingModel.getVisibleArea
				val paintCenter = if(visibleArea.contains(closestHitAsPoint)) currentState.markerPaintCenter else closestHitOrInitial
				val markers = MarkerUtil.convertToMarkers(search, sortedHits, Constants.markerAlphabet)
				currentState.copy(markers = markers, search = search, markerPaintCenter = paintCenter)
			}
		}
	}
	
	def updateEnter(currentState: Snapshot, input: EnterInput, editor: Editor) ={
		currentState.copy(snapshotState = PluginState.SELECTING)
	}
	
	def updateEscape(currentState: Snapshot, input: EscapeInput, editor: Editor) = {
		commonEscape(currentState, input, editor)
	}
	
	def updateScroll(currentState: Snapshot, input: ScrollInput, editor: Editor) = {
		val dir = input.dir
		dir match {
			case ScrollDirection.UP =>
				val visibleTextRange = EditorUtil.getVisibleTextRange(editor)
				val startVisible = visibleTextRange.getStartOffset
				val oldUpper = editor.offsetToLogicalPosition(startVisible)
				val newLine = Math.max(oldUpper.line - Constants.scrollLines, 0)
				val newCenter = new LogicalPosition(newLine, oldUpper.column)
				val newOffset = editor.logicalPositionToOffset(newCenter)
				currentState.copy(markerPaintCenter = newOffset)
			case ScrollDirection.DOWN =>
				val visibleTextRange = EditorUtil.getVisibleTextRange(editor)
				val endVisible = visibleTextRange.getEndOffset
				val oldLower = editor.offsetToLogicalPosition(endVisible)
				val newLine = Math.min(oldLower.line + Constants.scrollLines, editor.getDocument.getLineCount)
				val newCenter = new LogicalPosition(newLine, oldLower.column)
				val newOffset = editor.logicalPositionToOffset(newCenter)
				currentState.copy(markerPaintCenter = newOffset)
			case ScrollDirection.HOME =>
				currentState.copy(markerPaintCenter = currentState.initialCaretPos)
		}
	}
	
	def selectStringOneOL(currentState: Snapshot,
	                      input: StringInput,
	                      editor: Editor,
	                      effectCreator: EffCreatorOneOL,
	                      undoCreator: UndoCreatorOneOL) = {
		val search = input.value
		currentState.markers
			.find(mar => mar.repText.toUpperCase() == search.toUpperCase)
	    	.map(hit => hit.mType match {
			    case MarkerType.PRIMARY =>
				    val currentPosition = editor.getCaretModel.getOffset
				    val effect = effectCreator(hit.start, editor)
				    val undoer = undoCreator(currentPosition, editor)
				    val id = UUID.randomUUID().toString
				    val tuple = (effect, undoer, id)
				    currentState.copy(effects = List(tuple),snapshotState = PluginState.ACCEPT)
			    case MarkerType.SECONDARY =>
				    val secMarkers = currentState.markers.filter(marker => marker.mType == MarkerType.SECONDARY)
				    val hits = secMarkers.map(marker => marker.start)
				    val sortedHits: List[Int] = hits.sortBy(offset => Math.abs(currentState.markerPaintCenter-offset))
				    val markers = MarkerUtil.convertToMarkers(search, sortedHits, Constants.markerAlphabet)
				    val secAndSelected = markers ::: currentState.markers.filter(marker => marker.mType == MarkerType.SELECTED)
				    currentState.copy(markers = secAndSelected)
			    case MarkerType.SELECTED =>
				    currentState
		    }).getOrElse(currentState)
	}
	
	def selectStringTwoOL(currentState: Snapshot,
	                      input: StringInput,
	                      editor: Editor,
	                      effectCreator: EffCreatorTwoOL,
	                      undoCreator: UndoCreatorTwoOL)= {
		val search = input.value
		currentState.markers
			.find(mar => mar.repText.toUpperCase == search.toUpperCase)
			.map(hit =>
			currentState.overlays match {
				case 1 => selectFirstOverlay(currentState,search,hit)
				case 2 => selectSecondOverlay(currentState,search,hit,effectCreator,undoCreator,editor)
				case _ => currentState
			}).getOrElse(currentState)
	}
	
	def selectEscape(currentState: Snapshot, input: EscapeInput, editor: Editor) = {
		commonEscape(currentState, input, editor)
	}
	
	def acceptAccept(currentState: Snapshot, input: AcceptInput, editor: Editor) = {
		currentState.copy(snapshotState = PluginState.EXIT)
	}
	
	def acceptEscape(currentState: Snapshot, input: EscapeInput, editor: Editor) = {
		commonEscape(currentState, input, editor)
	}
	
	private def commonEscape(currentState: Snapshot, input: EscapeInput, editor: Editor) = {
		if(input.modifiers == ModifierCombination.ALT){
			currentState.copy(snapshotState = PluginState.EXIT)
		} else {
			currentState.copy(snapshotState = PluginState.UNDO)
		}
	}
	
	private def selectFirstOverlay(currentState: Snapshot, search: String, hit: Marker) = {
		hit.mType match {
			case MarkerType.PRIMARY =>
				val selectMarker = hit.copy(mType = MarkerType.SELECTED, end = hit.start)
				currentState.copy(markers = List.empty, selectedMarkers = List(selectMarker), snapshotState = PluginState.UPDATE, overlays = 2, search = "")
			case MarkerType.SECONDARY =>
				val secMarkers = currentState.markers.filter(marker => marker.mType == MarkerType.SECONDARY)
				val hits = secMarkers.map(marker => marker.start)
				val sortedHits: List[Int] = hits.sortBy(offset => Math.abs(currentState.markerPaintCenter-offset))
				val markers = MarkerUtil.convertToMarkers(search, sortedHits, Constants.markerAlphabet)
				val secAndSelected = markers ::: currentState.markers.filter(marker => marker.mType == MarkerType.SELECTED)
				currentState.copy(markers = secAndSelected)
			case MarkerType.SELECTED =>
				currentState
		}
	}
	
	private def selectSecondOverlay(currentState: Snapshot, search: String, hit: Marker, effectCreator: EffCreatorTwoOL, undoCreator: UndoCreatorTwoOL, editor: Editor) ={
		hit.mType match {
			case MarkerType.PRIMARY =>
				val firstMarker = currentState.selectedMarkers.headOption
				firstMarker.map(marker => {
					val smallestOffset = Math.min(marker.start, hit.start)
					val largestOffset = Math.max(marker.start, hit.start)
					val effect = effectCreator(smallestOffset, largestOffset, editor)
					val undoer = undoCreator(smallestOffset, largestOffset, editor)
					val id = UUID.randomUUID().toString
					val tuple = (effect, undoer, id)
					currentState.copy(effects = List(tuple),snapshotState = PluginState.ACCEPT)
				}).getOrElse(currentState)
			case MarkerType.SECONDARY =>
				val sortedHits = currentState.markers
					.filterNot(marker => marker.mType == MarkerType.PRIMARY)
					.map(marker => marker.start)
			    	.sortBy(offset => Math.abs(currentState.markerPaintCenter-offset))
				val markers = MarkerUtil
					.convertToMarkers(search, sortedHits, Constants.markerAlphabet)
				val secAndSelected = markers ::: currentState.markers.filter(marker => marker.mType == MarkerType.SELECTED)
				currentState.copy(markers = secAndSelected)
			case MarkerType.SELECTED =>
				currentState
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
	def convertToMarkers(search: String, hits: List[Int], markerAlphabet: String): List[Marker] = {
		def inner(search: String, hits: List[Int], markerAlphabet: String, acc: List[Marker]): List[Marker] = {
			val hit = if(hits.isEmpty) return acc else hits.head
			val markerChar = markerAlphabet.head
			val markerType = if(markerAlphabet.length > 1) MarkerType.PRIMARY else MarkerType.SECONDARY
			val marker = Marker(hit, hit + search.length, search, markerChar.toString, markerType)
			val added = marker :: acc
			val newAlphabet = if(markerAlphabet.length > 1) markerAlphabet.drop(1) else markerAlphabet
			inner(search, hits.drop(1), newAlphabet, added)
		}
		inner(search,hits,markerAlphabet, List.empty)
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
	
	def paintMarkers(markers: List[Marker], editor: Editor, markerPanel: JComponent, graphics: Graphics): Unit = {
		markers.foreach(marker => marker.mType match {
			case MarkerType.PRIMARY => paintMarker(marker, editor, backgroundColor = JBColor.GRAY, textColor = JBColor.WHITE, replacementTextColor = JBColor.RED, markerPanel, graphics)
			case MarkerType.SELECTED => paintSelectedMarker(marker, editor, backgroundColor = JBColor.BLACK, textColor = JBColor.WHITE, markerPanel, graphics)
			case MarkerType.SECONDARY => paintMarker(marker, editor, backgroundColor = JBColor.GRAY, textColor = JBColor.WHITE, replacementTextColor = JBColor.BLACK, markerPanel, graphics)
		})
	}
	
	def paintMarker(marker: Marker, editor: Editor, backgroundColor: JBColor, textColor: JBColor, replacementTextColor: JBColor, markerPanel: JComponent, graphics: Graphics): Unit ={
		drawBackground(editor, backgroundColor, textColor, marker, markerPanel, graphics)
		drawMarkerChar(editor, marker, replacementTextColor, markerPanel, graphics, (m: Marker) => m.repText)
	}
	
	def paintSelectedMarker(marker: Marker, editor: Editor, backgroundColor: JBColor, textColor: JBColor, markerPanel: JComponent, graphics: Graphics): Unit ={
		drawSelectedChar(editor, marker, backgroundColor, textColor, markerPanel, graphics)
	}
	
	def drawBackground(editor: Editor, backgroundColor: JBColor, textColor: JBColor, marker: Marker, markerPanel: JComponent, graphics: Graphics): Unit = {
		val font = editor.getColorsScheme getFont EditorFontType.BOLD
		val fontRect = markerPanel.getFontMetrics(font).getStringBounds(marker.orgText, graphics)
		graphics setColor backgroundColor
		graphics setFont font
		val x = markerPanel.getX + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.start)).getX
		val y = markerPanel.getY + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.end)).getY
		graphics.fillRect(x.toInt,y.toInt,fontRect.getWidth.toInt, fontRect.getHeight.toInt)
		if(marker.orgText.length > 1){
			graphics.setColor(textColor)
			val x_text = markerPanel.getX + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.start+1)).getX
			val y_text = markerPanel.getY + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.end)).getY
			val bottomYOfMarkerChar = y_text + font.getSize
			graphics.drawString(marker.orgText.substring(1), x_text.toInt, bottomYOfMarkerChar.toInt)
		}
	}
	
	def drawMarkerChar(editor: Editor, marker: Marker, textColor: JBColor, markerPanel: JComponent, graphics: Graphics, stringFunc: (Marker) => String): Unit = {
		val font = editor.getColorsScheme.getFont(EditorFontType.BOLD)
		val x = markerPanel.getX + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.start)).getX
		val y = markerPanel.getY + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.end)).getY
		val bottomYOfMarkerChar = y + font.getSize
		graphics.setColor(textColor)
		graphics.setFont(font)
		graphics.drawString(stringFunc(marker), x.toInt, bottomYOfMarkerChar.toInt)
	}
	
	def drawSelectedChar(editor: Editor, marker: Marker,backgroundColor: JBColor, textColor: JBColor, markerPanel: JComponent, graphics: Graphics): Unit ={
		val font = editor.getColorsScheme.getFont(EditorFontType.BOLD)
		val x = markerPanel.getX + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.start)).getX
		val y = markerPanel.getY + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.start+1)).getY
		val bottomYOfMarkerChar = y + font.getSize
		graphics.setFont(font)
		val fontRect: Rectangle2D = markerPanel.getFontMetrics(font).getStringBounds(marker.orgText.charAt(0).toString, graphics)
		graphics.setColor(backgroundColor)
		graphics.fillRect(x.toInt,y.toInt,fontRect.getWidth.toInt, fontRect.getHeight.toInt)
		graphics.setColor(textColor)
		graphics.drawString(marker.orgText.charAt(0).toString, x.toInt, bottomYOfMarkerChar.toInt)
	}
}
