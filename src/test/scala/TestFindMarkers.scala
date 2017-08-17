import actions.{PrimaryMarker, SecondaryMarker}
import dtpp.util.EditorUtil
import org.junit.runner.RunWith
import org.scalacheck.{Gen, Properties}
import org.scalatest.{FlatSpec, PropSpec}
import util.MarkerUtil
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import org.scalatest._

import scala.collection.JavaConverters._

@RunWith(classOf[JUnitRunner])
class TestFindMarkers extends FlatSpec {
	"some testcase" should "behave correctly" in {
		val input = "package util\n\nimport java.awt.geom.Rectangle2D\nimport java.awt.{Graphics, Rectangle}\nimport javax.swing.JComponent\n\nimport actions._\nimport com.intellij.openapi.actionSystem.{AnActionEvent, CommonDataKeys}\nimport com.intellij.openapi.editor.Editor\nimport com.intellij.openapi.editor.colors.EditorFontType\nimport com.intellij.ui.JBColor\nimport dtpp.util.EditorUtil\nimport state._\n\nimport scala.collection.JavaConverters._\nimport scala.collection.immutable.HashMap\nimport scala.collection.mutable\n\n/**\n  * Created by dr0l3 on 4/6/17.\n  */\n\n\nobject StartUtil {\n\tdef createInflatersAndAddComponent(anActionEvent: AnActionEvent): mutable.MutableList[StateInflater] = {\n\t\tif(anActionEvent == null)\n\t\t\tmutable.MutableList.empty\n\t\telse {\n\t\t\tval editor = anActionEvent.getData(CommonDataKeys.EDITOR)\n\t\t\tval markerPanel = new MarkerPainter(editor)\n\t\t\teditor.getContentComponent.add(markerPanel)\n\t\t\tval effectExecutor = new EffectExecutor(editor)\n\t\t\tval scrollInflater = new ScrollInflater(editor)\n\t\t\tval stateInflaters = mutable.MutableList[StateInflater](markerPanel, effectExecutor, scrollInflater)\n\t\t\tstateInflaters\n\t\t}\n\t}\n}\n\ntrait DTPPReducers {\n\tdef endGame(marker: PrimaryMarker, state: DTPPState, current: SelectSnapshot): DTPPSnapshot\n\t\n\tdef update(state: DTPPState, input: DTPPInput): DTPPState = {\n\t\t//clear\n\t\tval res = (input, state.history.present) match {\n\t\t\tcase (AcceptInput(), AcceptSnapshot(_)) => state.copy(exiting = true)\n\t\t\t\n\t\t\tcase (StringInput(value), current: UpdateSnapshot) =>\n\t\t\t\tval text = state.text\n\t\t\t\tval ignoredOffsets = List(state.initialCaretPos)\n\t\t\t\tval hits: List[Int] = EditorUtil.getMatchesForStringInText(value, text, ignoredOffsets.map(i => i:java.lang.Integer).asJava).asScala.map(_.toInt).toList\n\t\t\t\tval paintCenterPresent = current.centerPoint match {\n\t\t\t\t\tcase Concrete(offset) => offset\n\t\t\t\t\tcase _ => state.initialCaretPos\n\t\t\t\t}\n\t\t\t\tval sortedHits: List[Int] = hits.sortBy(offset => Math.abs(paintCenterPresent-offset))\n\t\t\t\tval markers = MarkerUtil.convertToDTPPMarkers(value, sortedHits, Constants.markerAlphabet)\n\t\t\t\tsortedHits.headOption.map(head => {\n\t\t\t\t\tval paintCenterNext = if(state.visibleText.contains(head)) paintCenterPresent else head\n\t\t\t\t\tval updatedSnapshot = current.copy(markers = markers, centerPoint = Concrete(paintCenterNext))\n\t\t\t\t\tstate.copy(history = state.history.update(updatedSnapshot))\n\t\t\t\t}).getOrElse({\n\t\t\t\t\tval next = current.copy(markers = List.empty)\n\t\t\t\t\tstate.copy(history = state.history.update(next))})\n\t\t\t\t\n\t\t\tcase (StringInput(value), current: SelectSnapshot) =>\n\t\t\t\tcurrent.markers\n\t\t\t\t\t.find(mar => mar.repText.toUpperCase == value.toUpperCase)\n\t\t\t\t\t.map {\n\t\t\t\t\t\tcase marker: PrimaryMarker => endGame(marker,state, current)\n\t\t\t\t\t\tcase _: SecondaryMarker => widen(current,state)}\n\t\t\t\t\t.map(next => state.copy(history = state.history.advance(next)))\n\t\t\t\t\t.getOrElse(state)\n\t\t\t\t\n\t\t\tcase (EnterInput(), current: UpdateSnapshot) =>\n\t\t\t\tval next = SelectSnapshot(current.markers, current.selectedMarkers,current.search, current.centerPoint)\n\t\t\t\tstate.copy(history = state.history.advance(next))\n\t\t\t\t\n\t\t\tcase (EnterInput(), current: SelectSnapshot) =>\n\t\t\t\tval next = widen(current, state)\n\t\t\t\tstate.copy(history = state.history.advance(next))\n\t\t\t\t\n\t\t\tcase (ScrollUpInput(), current: UpdateSnapshot)  =>\n\t\t\t\tval nextPoint = Relative(current.centerPoint, LineExtension(15))\n\t\t\t\tval next = current.copy(centerPoint = nextPoint)\n\t\t\t\tstate.copy(history = state.history.update(next))\n\t\t\t\t\n\t\t\tcase (ScrollUpInput(), current: SelectSnapshot)  =>\n\t\t\t\tval nextPoint = Relative(current.centerPoint, LineExtension(15))\n\t\t\t\tval next = current.copy(centerPoint = nextPoint)\n\t\t\t\tstate.copy(history = state.history.update(next))\n\t\t\tcase (ScrollDownInput(), current: UpdateSnapshot)  =>\n\t\t\t\tval nextPoint = Relative(current.centerPoint, LineExtension(-15))\n\t\t\t\tval next = current.copy(centerPoint = nextPoint)\n\t\t\t\tstate.copy(history = state.history.update(next))\n\t\t\t\t\n\t\t\tcase (ScrollDownInput(), current: SelectSnapshot)  =>\n\t\t\t\tval nextPoint = Relative(current.centerPoint, LineExtension(-15))\n\t\t\t\tval next = current.copy(centerPoint = nextPoint)\n\t\t\t\tstate.copy(history = state.history.update(next))\n\t\t\t\t\n\t\t\tcase (ScrollHomeInput(), current: UpdateSnapshot)  =>\n\t\t\t\tval next = current.copy(centerPoint = Concrete(state.initialCaretPos))\n\t\t\t\tstate.copy(history = state.history.update(next))\n\t\t\t\t\n\t\t\tcase (ScrollHomeInput(), current: SelectSnapshot)  =>\n\t\t\t\tval next = current.copy(centerPoint = Concrete(state.initialCaretPos))\n\t\t\t\tstate.copy(history = state.history.update(next))\n\t\t\t\t\n\t\t\tcase (EscapeInput(), _) =>\n\t\t\t\tstate.history.undo()\n\t\t\t\t\t.map(nextHist => state.copy(history = nextHist))\n\t\t\t    \t.getOrElse(state.copy(exiting = true))\n\t\t\t\t\n\t\t\tcase (AltEscapeInput(),_ ) => state.copy(exiting = true)\n\t\t\t\n\t\t\tcase (_, _) =>\n\t\t\t\tstate\n\t\t}\n\t\tres\n\t}\n\t\n\tprivate def widen(current: SelectSnapshot, state: DTPPState) = {\n\t\tval secMarkers = current.markers.filter {\n\t\t\tcase _: PrimaryMarker => false\n\t\t\tcase _: SecondaryMarker => true\n\t\t}\n\t\tval hits = secMarkers.map(marker => marker.start)\n\t\tval markerPaintCenter = current.centerPoint match {\n\t\t\tcase Concrete(offset) => offset\n\t\t\tcase _ => state.initialCaretPos\n\t\t}\n\t\tval sortedHits: List[Int] = hits.sortBy(offset => Math.abs(markerPaintCenter - offset))\n\t\tval markers = MarkerUtil.convertToDTPPMarkers(state.search, sortedHits, Constants.markerAlphabet)\n\t\tcurrent.copy(markers = markers)\n\t}\n}\n\ncase object Jump extends DTPPReducers {\n\toverride def endGame(marker: PrimaryMarker, state: DTPPState, current: SelectSnapshot): DTPPSnapshot = {\n\t\tAcceptSnapshot(JumpEffect(marker.start, state.initialCaretPos))\n\t}\n}\n\ncase object Delete extends DTPPReducers {\n\toverride def endGame(marker: PrimaryMarker, state: DTPPState, current: SelectSnapshot): DTPPSnapshot = {\n\t\tcurrent.selectedMarkers.size match {\n\t\t\tcase 0 =>\n\t\t\t\tval selectedMarker = SelectedMarker(marker.start,marker.end,marker.text)\n\t\t\t\tUpdateSnapshot(List.empty, List(selectedMarker),\"\",Concrete(state.initialCaretPos))\n\t\t\tcase 1 =>\n\t\t\t\tval start = current.selectedMarkers.head.start\n\t\t\t\tval end = marker.start\n\t\t\t\tval text =\n\t\t\t\t\tif(start < end) state.text.substring(start,end)\n\t\t\t\t\telse state.text.substring(end,start)\n\t\t\t\tAcceptSnapshot(DeleteEffect(start, end, text).validate())\n\t\t\tcase _ => state.history.present\n\t\t}\n\t}\n}\n\ncase object Cut extends DTPPReducers {\n\toverride def endGame(marker: PrimaryMarker, state: DTPPState, current: SelectSnapshot): DTPPSnapshot = {\n\t\tcurrent.selectedMarkers.size match {\n\t\t\tcase 0 =>\n\t\t\t\tval selectedMarker = SelectedMarker(marker.start,marker.end,marker.text)\n\t\t\t\tUpdateSnapshot(List.empty, List(selectedMarker),\"\",Concrete(state.initialCaretPos))\n\t\t\tcase 1 =>\n\t\t\t\tval start = current.selectedMarkers.head.start\n\t\t\t\tval end = marker.start\n\t\t\t\tval text =\n\t\t\t\t\tif(start < end) state.text.substring(start,end)\n\t\t\t\t\telse state.text.substring(end,start)\n\t\t\t\tAcceptSnapshot(CutEffect(start, end, text).validate())\n\t\t\tcase _ => state.history.present\n\t\t}\n\t}\n}\n\ncase object Copy extends DTPPReducers {\n\toverride def endGame(marker: PrimaryMarker, state: DTPPState, current: SelectSnapshot): DTPPSnapshot = {\n\t\tcurrent.selectedMarkers.size match {\n\t\t\tcase 0 =>\n\t\t\t\tval selectedMarker = SelectedMarker(marker.start,marker.end,marker.text)\n\t\t\t\tUpdateSnapshot(List.empty, List(selectedMarker),\"\",Concrete(state.initialCaretPos))\n\t\t\tcase 1 =>\n\t\t\t\tval start = current.selectedMarkers.head.start\n\t\t\t\tval end = marker.start\n\t\t\t\tAcceptSnapshot(CopyEffect(start, end))\n\t\t\tcase _ => state.history.present\n\t\t}\n\t}\n}\n\nobject Constants {\n\tdef markerAlphabet = \"asdfwerhjkltcvbyuiopågæøxnmz\".toUpperCase()\n\tdef updateListeners = List(ListenerType.UpdateMarkersCharListener, ListenerType.NonChar)\n\tdef selectingListeners = List(ListenerType.SelectMarkersCharListener, ListenerType.NonChar)\n\tdef acceptListeners = List(ListenerType.NonAccept)\n\tdef scrollLines = 20\n}\n\nobject MarkerUtil {\n\tdef convertToDTPPMarkers(search: String, hits: List[Int], markerAlphabet: String) = {\n\t\tdef inner(search: String, hits: List[Int], markerAlphabet: String, acc: List[SelectableMarker]): List[SelectableMarker] = {\n\t\t\tval hit = if(hits.isEmpty) return acc else hits.head\n\t\t\tval markerChar = markerAlphabet.head\n\t\t\tval nextMarker =\n\t\t\t\tif(markerAlphabet.length > 1) PrimaryMarker(hit, hit+search.length, search, markerChar.toString)\n\t\t\t\telse SecondaryMarker(hit,hit+search.length,search, markerChar.toString)\n\t\t\tval added = nextMarker :: acc\n\t\t\tval newAlphabet = if(markerAlphabet.length > 1) markerAlphabet.drop(1) else markerAlphabet\n\t\t\tinner(search, hits.drop(1), newAlphabet, added)\n\t\t}\n\t\tinner(search,hits,markerAlphabet,List.empty)\n\t}\n\t\n\t//implicit invariant Set[Int] == Map[Int,String].keySet\n\tdef findOffsetsInText(text: String, search: String, blacklist: Set[Int]): (Set[Int],Map[Int,String]) = {\n\t\tdef threeDuplicates(offset: Int) = {\n\t\t\ttext.charAt(offset-1) == search.charAt(0) &&\n\t\t\ttext.charAt(offset+1) == search.charAt(0)\n\t\t}\n\t\tdef withinBounds(offset: Int) = {\n\t\t\toffset+1 < text.length && offset > 0\n\t\t}\n\t\t//if search string is \"\" return empty\n\t\t//replace tabs and spaces\n\t\tval corpus = text.replace(\"\\n\",\" \").replace(\"\\t\", \" \").toLowerCase\n\t\t//lowercase search and text\n\t\tval offsets = mutable.Set[Int]()\n\t\tval offsetsToActualString = mutable.HashMap[Int,String]()\n\t\tvar index = -1\n\t\twhile(true){\n\t\t\tindex = corpus.indexOf(search,index+1)\n\t\t\tprintln(index)\n\t\t\tif(index == -1){\n\t\t\t\treturn (offsets.toSet, offsetsToActualString.toMap)\n\t\t\t}\n\t\t\tval prop = (search.length == 1 && withinBounds(index) && threeDuplicates(index))\n\t\t\tprintln(prop)\n\t\t\tif(!(blacklist.contains(index) || prop)){\n\t\t\t\tprintln(s\"adding $index\")\n\t\t\t\toffsets.add(index)\n\t\t\t\tval actual = corpus.substring(index,index + search.length)\n\t\t\t\toffsetsToActualString.put(index,actual)\n\t\t\t}\n\t\t}\n\t\t(Set.empty,HashMap.empty)\n\t}\n}\n\nobject PaintUtil {\n\tdef setupLocationAndBoundsOfPanel(editor: Editor, component: JComponent): Unit = {\n\t\tval visibleArea: Rectangle = editor.getScrollingModel.getVisibleAreaOnScrollingFinished\n\t\tval parent = editor.getContentComponent\n\t\tcomponent.setLocation(visibleArea.getX.toInt, visibleArea.getY.toInt)\n\t\tcomponent.setBounds(0, 0, parent.getSize().width, parent.getSize().height)\n\t\tcomponent.invalidate()\n\t}\n\t\n\tdef paintMarkers(markers: List[DTPPMarker], editor: Editor, markerPanel: JComponent, graphics: Graphics): Unit = {\n\t\tmarkers.foreach {\n\t\t\tcase marker: PrimaryMarker => paintMarker(marker, editor, backgroundColor = JBColor.GRAY, textColor = JBColor.WHITE, replacementTextColor = JBColor.RED, markerPanel, graphics)\n\t\t\tcase marker: SelectedMarker => paintSelectedMarker(marker, editor, backgroundColor = JBColor.BLACK, textColor = JBColor.WHITE, markerPanel, graphics)\n\t\t\tcase marker: SecondaryMarker => paintMarker(marker, editor, backgroundColor = JBColor.GRAY, textColor = JBColor.WHITE, replacementTextColor = JBColor.BLACK, markerPanel, graphics)\n\t\t}\n\t}\n\t\n\tdef paintMarker(marker: SelectableMarker, editor: Editor, backgroundColor: JBColor, textColor: JBColor, replacementTextColor: JBColor, markerPanel: JComponent, graphics: Graphics): Unit ={\n\t\tdrawBackground(editor, backgroundColor, textColor, marker, markerPanel, graphics)\n\t\tdrawMarkerChar(editor, marker, replacementTextColor, markerPanel, graphics)\n\t}\n\t\n\tdef paintSelectedMarker(marker: SelectedMarker, editor: Editor, backgroundColor: JBColor, textColor: JBColor, markerPanel: JComponent, graphics: Graphics): Unit ={\n\t\tdrawSelectedChar(editor, marker, backgroundColor, textColor, markerPanel, graphics)\n\t}\n\t\n\tdef drawBackground(editor: Editor, backgroundColor: JBColor, textColor: JBColor, marker: SelectableMarker, markerPanel: JComponent, graphics: Graphics): Unit = {\n\t\tval font = editor.getColorsScheme getFont EditorFontType.BOLD\n\t\tval fontRect = markerPanel.getFontMetrics(font).getStringBounds(marker.text, graphics)\n\t\tgraphics setColor backgroundColor\n\t\tgraphics setFont font\n\t\tval x = markerPanel.getX + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.start)).getX\n\t\tval y = markerPanel.getY + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.end)).getY\n\t\tgraphics.fillRect(x.toInt,y.toInt,fontRect.getWidth.toInt, fontRect.getHeight.toInt)\n\t\tif(marker.text.length > 1){\n\t\t\tgraphics.setColor(textColor)\n\t\t\tval x_text = markerPanel.getX + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.start+1)).getX\n\t\t\tval y_text = markerPanel.getY + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.end)).getY\n\t\t\tval bottomYOfMarkerChar = y_text + font.getSize\n\t\t\tgraphics.drawString(marker.text.substring(1), x_text.toInt, bottomYOfMarkerChar.toInt)\n\t\t}\n\t}\n\t\n\tdef drawMarkerChar(editor: Editor, marker: SelectableMarker, textColor: JBColor, markerPanel: JComponent, graphics: Graphics): Unit = {\n\t\tval font = editor.getColorsScheme.getFont(EditorFontType.BOLD)\n\t\tval x = markerPanel.getX + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.start)).getX\n\t\tval y = markerPanel.getY + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.end)).getY\n\t\tval bottomYOfMarkerChar = y + font.getSize\n\t\tgraphics.setColor(textColor)\n\t\tgraphics.setFont(font)\n\t\tgraphics.drawString(marker.repText, x.toInt, bottomYOfMarkerChar.toInt)\n\t}\n\t\n\tdef drawSelectedChar(editor: Editor, marker: SelectedMarker,backgroundColor: JBColor, textColor: JBColor, markerPanel: JComponent, graphics: Graphics): Unit ={\n\t\tval font = editor.getColorsScheme.getFont(EditorFontType.BOLD)\n\t\tval x = markerPanel.getX + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.start)).getX\n\t\tval y = markerPanel.getY + editor.logicalPositionToXY(editor.offsetToLogicalPosition(marker.start+1)).getY\n\t\tval bottomYOfMarkerChar = y + font.getSize\n\t\tgraphics.setFont(font)\n\t\tval fontRect: Rectangle2D = markerPanel.getFontMetrics(font).getStringBounds(marker.text.charAt(0).toString, graphics)\n\t\tgraphics.setColor(backgroundColor)\n\t\tgraphics.fillRect(x.toInt,y.toInt,fontRect.getWidth.toInt, fontRect.getHeight.toInt)\n\t\tgraphics.setColor(textColor)\n\t\tgraphics.drawString(marker.text.charAt(0).toString, x.toInt, bottomYOfMarkerChar.toInt)\n\t}\n}"
		val start = System.currentTimeMillis()
		val out = MarkerUtil.findOffsetsInText(input," ", Set.empty)
//		val out = EditorUtil.getMatchesSets("def",input,List.empty.asJava)
//		val converted = (out.first.asScala.toSet, out.second.asScala.toMap)
		val end = System.currentTimeMillis()
	}
}

@RunWith(classOf[JUnitRunner])
class SomeObject extends PropSpec with Checkers {
	val generator: Gen[(Set[Int],Set[String],String)] =  for {
		size <- Gen.choose(1, 100)
		intSet <- Gen.containerOfN[Set,Int](size,Gen.choose(0,1000))
		stringSet <- Gen.containerOfN[Set,String](size,Gen.alphaStr)
		string <- Gen.alphaStr
		if string.length > 0
		if intSet.size == stringSet.size
	} yield (intSet,stringSet, string)
	
	property("unique and non unique marker sets have the same size") {
		check(
			forAll(generator) {inp : (Set[Int], Set[String], String) => inp match {
				case (ints: Set[Int], strings: Set[String], search: String) =>
					val offsets = ints
					val offset2actual: Map[Int, String] = ints.zip(strings).toMap
					val hits = ints.toList
					val outNonUnique = MarkerUtil.convertToDTPPMarkers(search,hits)
					val outUnique = MarkerUtil.convert2MarkersUnique(offsets,offset2actual,search)
					
					outNonUnique.size == outUnique._1.size + outUnique._2.size &&
						(outNonUnique.count {
							case _: SecondaryMarker => true
							case _ => false
						} == outUnique._2.size) &&
						(outNonUnique.count {
							case _: PrimaryMarker => true
							case _ => false
						} == outUnique._1.size)
			}
			}
		)
	}
}