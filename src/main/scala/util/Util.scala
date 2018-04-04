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
import scala.collection.immutable.HashMap
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
	def nextOverLayOrEffect(marker: PrimaryMarker, state: DTPPState, current: SelectSnapshot): DTPPSnapshot
	def effectNow(startOffset: DTPPPoint, endOffset: DTPPPoint, current: SelectSnapshot, state: DTPPState): DTPPSnapshot
	
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
				val markers = MarkerUtil.convertToDTPPMarkers(value, sortedHits)
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
						case marker: PrimaryMarker => nextOverLayOrEffect(marker,state, current)
						case _: SecondaryMarker => widen(current,state)}
					.map(next => state.copy(history = state.history.advance(next)))
					.getOrElse(state)
			case (AltStringInput(value), current: SelectSnapshot)=> {
				current.markers
					.find(mar => mar.repText.toUpperCase == value.toUpperCase())
			    	.map {
					    case marker: PrimaryMarker =>
						    val startOffset = Concrete(marker.start)
						    val endOffset = Relative(startOffset,LineEnd)
						    effectNow(startOffset, endOffset, current,state)
					    case _: SecondaryMarker => widen(current,state)}
			    	.map(next => state.copy(history =  state.history.advance(next)))
			    	.getOrElse(state)
			}
			
			case (ShiftStringInput(value), current: SelectSnapshot)=> {
				current.markers
					.find(mar => mar.repText.toUpperCase == value.toUpperCase())
					.map {
						case marker: PrimaryMarker =>
							val startOffset = Concrete(marker.start)
							val endOffset = Relative(startOffset,LineStart)
							effectNow(startOffset, endOffset, current,state)
						case _: SecondaryMarker => widen(current,state)}
					.map(next => state.copy(history =  state.history.advance(next)))
					.getOrElse(state)
			}
			
			case (EnterInput(), current: UpdateSnapshot) =>
				val next = SelectSnapshot(current.markers, current.selectedMarkers,current.search, current.centerPoint)
				state.copy(history = state.history.advance(next))
				
			case (EnterInput(), current: SelectSnapshot) =>
				val next = widen(current, state)
				state.copy(history = state.history.advance(next))
				
			case (ScrollUpInput(), current: UpdateSnapshot)  =>
				val nextPoint = Relative(current.centerPoint, LineExtension(-15))
				val next = current.copy(centerPoint = nextPoint)
				state.copy(history = state.history.update(next))
				
			case (ScrollUpInput(), current: SelectSnapshot)  =>
				val nextPoint = Relative(current.centerPoint, LineExtension(-15))
				val next = current.copy(centerPoint = nextPoint)
				state.copy(history = state.history.update(next))
			
			case (ScrollDownInput(), current: UpdateSnapshot)  =>
				val nextPoint = Relative(current.centerPoint, LineExtension(15))
				val next = current.copy(centerPoint = nextPoint)
				state.copy(history = state.history.update(next))
				
			case (ScrollDownInput(), current: SelectSnapshot)  =>
				val nextPoint = Relative(current.centerPoint, LineExtension(15))
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
			    	.getOrElse({
							state.copy(exiting = true)
						})
				
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
		val markers = MarkerUtil.convertToDTPPMarkers(state.search, sortedHits)
		current.copy(markers = markers)
	}
}

case object Jump extends DTPPReducers {
	override def nextOverLayOrEffect(marker: PrimaryMarker, state: DTPPState, current: SelectSnapshot): DTPPSnapshot = {
		AcceptSnapshot(JumpEffect(Concrete(marker.start), Concrete(state.initialCaretPos)))
	}
	
	override def effectNow(startOffset: DTPPPoint, endOffset: DTPPPoint, current: SelectSnapshot, state: DTPPState) = {
		AcceptSnapshot(JumpEffect(startOffset, Concrete(state.initialCaretPos)))
	}
}

case object Delete extends DTPPReducers {
	override def nextOverLayOrEffect(marker: PrimaryMarker, state: DTPPState, current: SelectSnapshot): DTPPSnapshot = {
		current.selectedMarkers.size match {
			case 0 =>
				val selectedMarker = SelectedMarker(marker.start,marker.end,marker.text)
				UpdateSnapshot(List.empty, List(selectedMarker),"",Concrete(state.initialCaretPos))
			case 1 =>
				val start = current.selectedMarkers.head.start
				val end = marker.start
				AcceptSnapshot(DeleteEffect(Concrete(start), Concrete(end)))
			case _ => state.history.present
		}
	}
	
	override def effectNow(startOffset: DTPPPoint, endOffset: DTPPPoint, current: SelectSnapshot, state: DTPPState) = {
		AcceptSnapshot(DeleteEffect(startOffset, Relative(startOffset,LineEnd)))
	}
}

case object Cut extends DTPPReducers {
	override def nextOverLayOrEffect(marker: PrimaryMarker, state: DTPPState, current: SelectSnapshot): DTPPSnapshot = {
		current.selectedMarkers.size match {
			case 0 =>
				val selectedMarker = SelectedMarker(marker.start,marker.end,marker.text)
				UpdateSnapshot(List.empty, List(selectedMarker),"",Concrete(state.initialCaretPos))
			case 1 =>
				val start = current.selectedMarkers.head.start
				val end = marker.start
				AcceptSnapshot(CutEffect(Concrete(start), Concrete(end)))
			case _ => state.history.present
		}
	}
	
	override def effectNow(startOffset: DTPPPoint, endOffset: DTPPPoint, current: SelectSnapshot, state: DTPPState) = {
		AcceptSnapshot(CutEffect(startOffset,endOffset))
	}
}

case object Copy extends DTPPReducers {
	override def nextOverLayOrEffect(marker: PrimaryMarker, state: DTPPState, current: SelectSnapshot): DTPPSnapshot = {
		current.selectedMarkers.size match {
			case 0 =>
				val selectedMarker = SelectedMarker(marker.start,marker.end,marker.text)
				UpdateSnapshot(List.empty, List(selectedMarker),"",Concrete(state.initialCaretPos))
			case 1 =>
				val start = current.selectedMarkers.head.start
				val end = marker.start
				AcceptSnapshot(CopyEffect(Concrete(start), Concrete(end)))
			case _ => state.history.present
		}
	}
	
	override def effectNow(startOffset: DTPPPoint, endOffset: DTPPPoint, current: SelectSnapshot, state: DTPPState) = {
		AcceptSnapshot(CopyEffect(startOffset,endOffset))
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
	def convertToDTPPMarkers(search: String, hits: List[Int]) = {
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
		inner(search,hits,Constants.markerAlphabet,List.empty)
	}
	
	def convert2MarkersUnique(offsets: Set[Int], offset2actual: Map[Int,String], search: String): (Map[String, PrimaryMarker], Set[SecondaryMarker]) ={
		def inner(offsets: Set[Int],
		          offset2actual: Map[Int,String],
		          search: String,
		          acc: (Map[String, PrimaryMarker], Set[SecondaryMarker]),
		          markerAlphabet: String): (Map[String,PrimaryMarker], Set[SecondaryMarker]) ={
			offsets.headOption.map(offset => {
				val markerChar = markerAlphabet.head
				val nextMarker =
					if (markerAlphabet.length > 1)
						PrimaryMarker(offset, offset + search.length, offset2actual(offset), markerChar.toString)
					else SecondaryMarker(offset, offset + search.length, offset2actual(offset), markerChar.toString)
				val nextAlphabet = if (markerAlphabet.length > 1) markerAlphabet.tail else markerAlphabet
				val nextOffsets = offsets.tail
				nextMarker match {
					case m: PrimaryMarker =>
						val added = acc._1 + (markerChar.toString -> m)
						inner(nextOffsets, offset2actual, search, (added, acc._2), nextAlphabet)
					case m: SecondaryMarker =>
						val added = acc._2 + m
						inner(nextOffsets, offset2actual, search, (acc._1, added), nextAlphabet)
				}}
			).getOrElse(acc)
		}
		inner(offsets,offset2actual,search,(Map.empty,Set.empty), Constants.markerAlphabet)
	}
	
	//implicit invariant Set[Int] == Map[Int,String].keySet
	def findOffsetsInText(text: String, search: String, blacklist: Set[Int]): (Set[Int],Map[Int,String]) = {
		def threeDuplicates(offset: Int) = {
			text.charAt(offset-1) == search.charAt(0) &&
			text.charAt(offset+1) == search.charAt(0)
		}
		def withinBounds(offset: Int) = {
			offset+1 < text.length && offset > 0
		}
		//if search string is "" return empty
		//replace tabs and spaces
		val corpus = text.replace("\n"," ").replace("\t", " ").toLowerCase
		//lowercase search and text
		val offsets = mutable.Set[Int]()
		val offsetsToActualString = mutable.HashMap[Int,String]()
		var index = -1
		while(true){
			index = corpus.indexOf(search,index+1)
			if(index == -1){
				return (offsets.toSet, offsetsToActualString.toMap)
			}
			if(blacklist.contains(index)){
			
			} else if(search.length == 1 && withinBounds(index) && threeDuplicates(index)) {
			
			} else {
				offsets.add(index)
				val actual = corpus.substring(index,index + search.length)
				offsetsToActualString.put(index,actual)
			}
		}
		(Set.empty,HashMap.empty)
	}
}

case class MarkerRepo(offsets: Set[Int], offsets2actual: Map[Int,String]){
	private def markers: Map[String,DTPPMarker] = Map.empty
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
