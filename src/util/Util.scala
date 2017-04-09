package util

import java.awt.{Graphics, Rectangle}
import java.awt.geom.Rectangle2D
import javax.swing.JComponent

import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.colors.EditorFontType
import com.intellij.ui.JBColor
import main.{ListenerType, Marker, MarkerType}

/**
  * Created by dr0l3 on 4/6/17.
  */


object Constants {
	def markerAlphabet = "asdfwerhjkltcvbyuiopågæøxnmz".toUpperCase()
	def updateListeners = List(ListenerType.UpdateMarkersCharListener, ListenerType.NonChar)
	def selectingListeners = List(ListenerType.SelectMarkersCharListener, ListenerType.NonChar)
	def acceptListeners = List(ListenerType.NonAccept)
}

object MarkerUtil {
	def convertToMarkers(search: String, hits: List[Int], markerAlphabet: String, acc: List[Marker]): List[Marker] = {
		val hit = if(hits.isEmpty) return acc else hits.head
		val markerChar = markerAlphabet.head
		val markerType = if(markerAlphabet.length > 1) MarkerType.PRIMARY else MarkerType.SECONDARY
		val marker = Marker(hit, hit + search.length, search, markerChar.toString, markerType)
		val added = marker :: acc
		val newAlphabet = if(markerAlphabet.length > 1) markerAlphabet.drop(1) else markerAlphabet
		convertToMarkers(search, hits.drop(1), newAlphabet, added)
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
			case MarkerType.SECONDARY => paintMarker(marker, editor, backgroundColor = JBColor.GRAY, textColor = JBColor.WHITE, replacementTextColor = JBColor.YELLOW, markerPanel, graphics)
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
