import java.awt.{Point, Rectangle}

import actions._
import com.intellij.openapi.editor._
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec
import state.{PluginState, Snapshot}
import util.{Reducers, StubDocument, StubEditor}

class TestReducer extends FlatSpec with MockFactory {
	"A stack" should "pop values in lifo order" in {
		var stack = List.empty[Int]
		stack = 1 :: stack
		stack = 2 :: stack
		assert(stack.size == 2)
		assert(stack.head == 2)
		assert(stack.tail.head == 1)
	}
	
	"Update with enter" should "should change state to selecting" in {
		val editor = mock[Editor]
		val state = Snapshot(List.empty,List.empty, List.empty, 1, 1, "", PluginState.UPDATE, 0)
		val input = EnterInput(ModifierCombination.NONE)
		val response = Reducers.updateEnter(state,input,editor)
		assert(response.snapshotState == PluginState.SELECTING)
	}
	
	"Update with escape" should "undo to proper state" in {
		val editor = mock[Editor]
		val state = Snapshot(List.empty, List.empty, List.empty, 1,1, "", PluginState.UPDATE,0)
		val inputNoMods = EscapeInput(ModifierCombination.NONE)
		val inputModAlt = EscapeInput(ModifierCombination.ALT)
		val inputModCtrl = EscapeInput(ModifierCombination.CTRL)
		val inputModShift = EscapeInput(ModifierCombination.SHIFT)
		
		val responseNoMod = Reducers.updateEscape(state,inputNoMods,editor)
		val responseModAlt = Reducers.updateEscape(state,inputModAlt,editor)
		val responseModCtrl = Reducers.updateEscape(state,inputModCtrl, editor)
		val responseModShift = Reducers.updateEscape(state,inputModShift, editor)
		
		assert(responseModAlt.snapshotState == PluginState.EXIT)
		assert(responseModCtrl.snapshotState == PluginState.UNDO)
		assert(responseNoMod.snapshotState == PluginState.UNDO)
		assert(responseModShift.snapshotState == PluginState.UNDO)
	}
	
	"Select with escape" should "undo to proper state" in {
		val editor = mock[Editor]
		val state = Snapshot(List.empty, List.empty, List.empty, 1,1, "", PluginState.SELECTING,0)
		val inputNoMods = EscapeInput(ModifierCombination.NONE)
		val inputModAlt = EscapeInput(ModifierCombination.ALT)
		val inputModCtrl = EscapeInput(ModifierCombination.CTRL)
		val inputModShift = EscapeInput(ModifierCombination.SHIFT)
		
		val responseNoMod = Reducers.selectEscape(state,inputNoMods,editor)
		val responseModAlt = Reducers.selectEscape(state,inputModAlt,editor)
		val responseModCtrl = Reducers.selectEscape(state,inputModCtrl, editor)
		val responseModShift = Reducers.selectEscape(state,inputModShift, editor)
		
		assert(responseModAlt.snapshotState == PluginState.EXIT)
		assert(responseModCtrl.snapshotState == PluginState.UNDO)
		assert(responseNoMod.snapshotState == PluginState.UNDO)
		assert(responseModShift.snapshotState == PluginState.UNDO)
	}
	
	"Accept with escape" should "undo to proper state" in {
		val editor = mock[Editor]
		val state = Snapshot(List.empty, List.empty, List.empty, 1,1, "", PluginState.ACCEPT,0)
		val inputNoMods = EscapeInput(ModifierCombination.NONE)
		val inputModAlt = EscapeInput(ModifierCombination.ALT)
		val inputModCtrl = EscapeInput(ModifierCombination.CTRL)
		val inputModShift = EscapeInput(ModifierCombination.SHIFT)
		
		val responseNoMod = Reducers.acceptEscape(state,inputNoMods,editor)
		val responseModAlt = Reducers.acceptEscape(state,inputModAlt,editor)
		val responseModCtrl = Reducers.acceptEscape(state,inputModCtrl, editor)
		val responseModShift = Reducers.acceptEscape(state,inputModShift, editor)
		
		assert(responseModAlt.snapshotState == PluginState.EXIT)
		assert(responseModCtrl.snapshotState == PluginState.UNDO)
		assert(responseNoMod.snapshotState == PluginState.UNDO)
		assert(responseModShift.snapshotState == PluginState.UNDO)
	}
	
	"Accept with accept" should "exit" in {
		val editor = mock[Editor]
		val state = Snapshot(List.empty, List.empty, List.empty, 1,1, "", PluginState.UPDATE,0)
		val input = AcceptInput()
		
		val response = Reducers.acceptAccept(state,input,editor)
		
		assert(response.snapshotState == PluginState.EXIT)
	}
	
	"Update with string" should "have no markers when search string is empty" in {
		val editor = mock[Editor]
		val state = Snapshot(List.empty, List.empty, List.empty, 1,1, "", PluginState.UPDATE, 0)
		val input = StringInput("",ModifierCombination.NONE)
		
		val response = Reducers.updateString(state,input,editor)
		assert(response.markers == List.empty)
	}
	
	"Update with string matching current state string" should "not change the state" in {
		val editor = mock[Editor]
		val state = Snapshot(List.empty, List.empty, List.empty, 1,1, "a", PluginState.UPDATE,0)
		val input = StringInput("a", ModifierCombination.NONE)
		
		val response = Reducers.updateString(state,input,editor)
		assert(response == state)
	}
	
	"Update with new string" should "catch all occurences" in {
		val visiblePoint = new Point(0,0)
		val text = "hello this is dog"
		val document = new StubDocument(text)
		val scrollingModel = stub[ScrollingModel]
		val visibleArea = new Rectangle(0,0,1,1)
		val editor = new StubEditor(scrollingModel,document,visiblePoint)
		val state = Snapshot(List.empty, List.empty, List.empty, 1,1, "b", PluginState.UPDATE,0)
		val input_i = StringInput("i", ModifierCombination.NONE)
		val input_d = StringInput("d", ModifierCombination.NONE)
		
		(scrollingModel.getVisibleArea _).when().returns(visibleArea)
		
		val response_i = Reducers.updateString(state,input_i,editor)
		assert(response_i.markers.size == 2)
		assert(response_i.markerPaintCenter == 1)
		
		val response_d = Reducers.updateString(state,input_d,editor)
		assert(response_d.markers.size == 1)
		assert(response_d.markerPaintCenter == 1)
	}
	
	"Update with new string and invisible marker" should "move markerpaint center" in {
		val invisiblePoint = new Point(2,2)
		val text = "hello this is dog"
		val document = new StubDocument(text)
		val scrollingModel = stub[ScrollingModel]
		val visibleArea = new Rectangle(0,0,1,1)
		val editor = new StubEditor(scrollingModel,document,invisiblePoint)
		val state = Snapshot(List.empty, List.empty, List.empty, 1,1, "b", PluginState.UPDATE,0)
		val input = StringInput("i", ModifierCombination.NONE)
		
		(scrollingModel.getVisibleArea _).when().returns(visibleArea)
		
		val response = Reducers.updateString(state,input,editor)
		
		assert(response.markerPaintCenter == 8)
	}
	
	"Update with string" should "ignore selected markers" in {
		val point = new Point(0,0)
		val text = "abcabc"
		val document = new StubDocument(text)
		val scrollingModel = stub[ScrollingModel]
		val visibleArea = new Rectangle(0,0,1,1)
		val selectedMarker = Marker(0,1,"a","f", MarkerType.SELECTED)
		val editor = new StubEditor(scrollingModel,document,point)
		val state = Snapshot(List.empty, List(selectedMarker), List.empty, 1,1, "b", PluginState.UPDATE, 0)
		val input = StringInput("a", ModifierCombination.NONE)
		
		(scrollingModel.getVisibleArea _).when().returns(visibleArea)
		
		val response = Reducers.updateString(state,input, editor)
		
		assert(response.markers.size == 1)
		assert(response.markers.head.start == 3)
	}
	
	"selectStringOneOL" should "return current state when hit is not found" in {
		val point = new Point(0,0)
		val text = "abcabc"
		val document = new StubDocument(text)
		val scrollingModel = stub[ScrollingModel]
		val editor = new StubEditor(scrollingModel,document,point)
		val state = Snapshot(List.empty, List.empty, List.empty, 1,1, "b", PluginState.SELECTING, 0)
		val input = StringInput("a", ModifierCombination.NONE)
		
		val response = Reducers.selectStringOneOL(state,input,editor,(_:Int,_: Editor) =>  () => {},(_:Int,_: Editor) => () => {})
		
		assert(response == state)
		
	}
	
	"selectStringOneOL" should "calculate effects and move to accept" in  {
		val point = new Point(0,0)
		val text = "abcabc"
		val document = new StubDocument(text)
		val scrollingModel = stub[ScrollingModel]
		val caretModel = stub[CaretModel]
		val markerChar = "b"
		val markerHit = Marker(0,1,"f",markerChar,MarkerType.PRIMARY)
		val editor = new StubEditor(scrollingModel,document,point, caretModel)
		val state = Snapshot(List(markerHit), List.empty, List.empty, 1,1, "b", PluginState.SELECTING, 0)
		val input = StringInput(markerChar, ModifierCombination.NONE)
		
		(caretModel.getOffset _).when().returns(1)
		
		val response = Reducers.selectStringOneOL(state,input,editor,(_:Int,_: Editor) =>  () => {},(_:Int,_: Editor) => () => {})
		
		assert(response.effects.size == 1)
		assert(response.snapshotState == PluginState.ACCEPT)
	}
	
	"selectStringOneOL" should "return current state when hit is already selected" in {
		val point = new Point(0,0)
		val text = "abcabc"
		val document = new StubDocument(text)
		val scrollingModel = stub[ScrollingModel]
		val caretModel = stub[CaretModel]
		val markerChar = "b"
		val markerHit = Marker(0,1,"f",markerChar,MarkerType.SELECTED)
		val editor = new StubEditor(scrollingModel,document,point, caretModel)
		val state = Snapshot(List(markerHit), List.empty, List.empty, 1,1, "b", PluginState.SELECTING, 0)
		val input = StringInput(markerChar, ModifierCombination.NONE)
		
		(caretModel.getOffset _).when().returns(1)
		
		val response = Reducers.selectStringOneOL(state,input,editor,(_:Int,_: Editor) =>  () => {},(_:Int,_: Editor) => () => {})
		
		assert(response == state)
	}
	
	"selectStringOneOL" should "calculate correct markers when secondary marker is hit" in {
		val point = new Point(0,0)
		val text = "abcabc"
		val document = new StubDocument(text)
		val scrollingModel = stub[ScrollingModel]
		val caretModel = stub[CaretModel]
		val markerChar = "b"
		val markerHit = Marker(0,1,"f",markerChar,MarkerType.SECONDARY)
		val editor = new StubEditor(scrollingModel,document,point, caretModel)
		val state = Snapshot(List(markerHit), List.empty, List.empty, 1,1, "b", PluginState.SELECTING, 0)
		val state_2markers = Snapshot(List(markerHit,Marker(0,1,"f", "g",MarkerType.SELECTED)), List.empty, List.empty, 1,1, "b", PluginState.SELECTING, 0)
		val input = StringInput(markerChar, ModifierCombination.NONE)
		
		(caretModel.getOffset _).when().returns(1)
		
		val response = Reducers.selectStringOneOL(state,input,editor,(_:Int,_: Editor) =>  () => {},(_:Int,_: Editor) => () => {})
		val response_2markers = Reducers.selectStringOneOL(state_2markers,input,editor,(_:Int,_: Editor) =>  () => {},(_:Int,_: Editor) => () => {})
		
		assert(response.markers.size == 1)
		assert(response_2markers.markers.size == 2)
	}
	
	"selectStringTwoOL" should "return current state when hit is not found" in {
		val point = new Point(0,0)
		val text = "abcabc"
		val document = new StubDocument(text)
		val scrollingModel = stub[ScrollingModel]
		val editor = new StubEditor(scrollingModel,document,point)
		val state = Snapshot(List.empty, List.empty, List.empty, 1,1, "b", PluginState.SELECTING, 1)
		val input = StringInput("a", ModifierCombination.NONE)
		
		val response = Reducers.selectStringTwoOL(state,input,editor,(_:Int,_:Int,_: Editor) =>  () => {},(_:Int,_:Int,_: Editor) => () => {})
		
		assert(response == state)
	}
	
	"selectStringTwoOL" should "return current state when weird number of overlays are found" in {
		val point = new Point(0,0)
		val text = "abcabc"
		val document = new StubDocument(text)
		val scrollingModel = stub[ScrollingModel]
		val editor = new StubEditor(scrollingModel,document,point)
		val state = Snapshot(List.empty, List.empty, List.empty, 1,1, "b", PluginState.SELECTING, 0)
		val state_3 = Snapshot(List.empty, List.empty, List.empty, 1,1, "b", PluginState.SELECTING, 3)
		val input = StringInput("a", ModifierCombination.NONE)
		
		val response = Reducers.selectStringTwoOL(state,input,editor,(_:Int,_:Int,_: Editor) =>  () => {},(_:Int,_:Int,_: Editor) => () => {})
		val response_3 = Reducers.selectStringTwoOL(state_3,input,editor,(_:Int,_:Int,_: Editor) =>  () => {},(_:Int,_:Int,_: Editor) => () => {})
		
		assert(response == state)
		assert(response_3 == state_3)
	}
	
	"selectFirstOverlay" should "return correct when primary hit" in {
		val point = new Point(0,0)
		val text = "abcabc"
		val document = new StubDocument(text)
		val scrollingModel = stub[ScrollingModel]
		val markerChar = "b"
		val markerHit = Marker(0,1,"f",markerChar,MarkerType.PRIMARY)
		val editor = new StubEditor(scrollingModel,document,point)
		val state = Snapshot(List(markerHit), List.empty, List.empty, 1,1, "b", PluginState.SELECTING, 1)
		val input = StringInput(markerChar, ModifierCombination.NONE)
		
		val response = Reducers.selectStringTwoOL(state,input,editor,(_:Int,_:Int,_: Editor) =>  () => {},(_:Int,_:Int,_: Editor) => () => {})
		
		assert(response.markers.isEmpty)
		assert(response.selectedMarkers.size == 1)
		assert(response.snapshotState == PluginState.UPDATE)
		assert(response.overlays == 2)
		assert(response.search == "")
	}
	
	"selectFirstOverlay" should "return current state when selected marker hit" in {
		val point = new Point(0,0)
		val text = "abcabc"
		val document = new StubDocument(text)
		val scrollingModel = stub[ScrollingModel]
		val markerChar = "b"
		val markerHit = Marker(0,1,"f",markerChar,MarkerType.SELECTED)
		val editor = new StubEditor(scrollingModel,document,point)
		val state = Snapshot(List(markerHit), List.empty, List.empty, 1,1, "b", PluginState.SELECTING, 1)
		val input = StringInput(markerChar, ModifierCombination.NONE)
		
		val response = Reducers.selectStringTwoOL(state,input,editor,(_:Int,_:Int,_: Editor) =>  () => {},(_:Int,_:Int,_: Editor) => () => {})
		
		assert(response == state)
	}
	
	"selectFirstOverlay" should "calculate correct markers when secondary marker is hit" in {
		val point = new Point(0,0)
		val text = "abcabc"
		val document = new StubDocument(text)
		val scrollingModel = stub[ScrollingModel]
		val caretModel = stub[CaretModel]
		val markerChar = "b"
		val markerHit = Marker(0,1,"f",markerChar,MarkerType.SECONDARY)
		val editor = new StubEditor(scrollingModel,document,point, caretModel)
		val state = Snapshot(List(markerHit), List.empty, List.empty, 1,1, "b", PluginState.SELECTING, 1)
		val state_2markers = Snapshot(List(markerHit,Marker(0,1,"f", "g",MarkerType.SELECTED)), List.empty, List.empty, 1,1, "b", PluginState.SELECTING, 1)
		val input = StringInput(markerChar, ModifierCombination.NONE)
		
		(caretModel.getOffset _).when().returns(1)
		
		val response = Reducers.selectStringTwoOL(state,input,editor,(_:Int, _:Int,_: Editor) =>  () => {},(_:Int,_:Int,_: Editor) => () => {})
		val response_2markers = Reducers.selectStringTwoOL(state_2markers,input,editor,(_:Int, _:Int,_: Editor) =>  () => {},(_:Int,_:Int,_: Editor) => () => {})
		
		assert(response.markers.size == 1)
		assert(response_2markers.markers.size == 2)
	}
	
	"selectSecondOverlay" should "create effects when primary marker is hit" in {
		val point = new Point(0,0)
		val text = "abcabc"
		val document = new StubDocument(text)
		val scrollingModel = stub[ScrollingModel]
		val caretModel = stub[CaretModel]
		val markerChar = "b"
		val markerHit = Marker(0,1,"f",markerChar,MarkerType.PRIMARY)
		val previouslySelectedMarker = Marker(1,2,"b","f",MarkerType.SELECTED)
		val editor = new StubEditor(scrollingModel,document,point, caretModel)
		val state = Snapshot(List(markerHit), List(previouslySelectedMarker), List.empty, 1,1, "b", PluginState.SELECTING, 2)
		val input = StringInput(markerChar, ModifierCombination.NONE)
		
		(caretModel.getOffset _).when().returns(1)
		
		val response = Reducers.selectStringTwoOL(state,input,editor,(_:Int, _:Int,_: Editor) =>  () => {},(_:Int,_:Int,_: Editor) => () => {})
		
		assert(response.markers.size == 1)
		assert(response.effects.size == 1)
		assert(response.snapshotState == PluginState.ACCEPT)
	}
}
