package main

import java.awt.event.{ActionEvent, InputEvent, KeyEvent, KeyListener}
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.{AbstractAction, JTextField, KeyStroke}

import com.intellij.openapi.editor.Editor
import com.intellij.ui.components.JBTextField
import util.Constants

/**
  * Created by dr0l3 on 4/7/17.
  */
object ListenerType extends Enumeration{
	type ListenerType = Value
	val NonAccept, NonChar, SelectMarkersCharListener, UpdateMarkersCharListener = Value
}

trait Listener {
	def register(): Unit
	def unregister(): Unit
	def getType: ListenerType.ListenerType
}

class Updater(val inputReceiver: DtppAction, val textField: JBTextField) extends Listener with DocumentListener{
	override def removeUpdate(event: DocumentEvent): Unit = {
		processEvent(event)
	}
	
	override def changedUpdate(event: DocumentEvent) {
		processEvent(event)
	}
	
	override def insertUpdate(event: DocumentEvent): Unit ={
		processEvent(event)
	}
	
	def processEvent(event: DocumentEvent): Unit ={
		val text = event.getDocument.getText(0, event.getDocument.getLength)
		println(s"Text: $text")
		println(s"Event: $event")
		println("eventtype: " + event.getType.toString)
		inputReceiver.receiveInput(StringInput(text, ModifierCombination.NONE))
	}
	
	override def register() {textField.getDocument.addDocumentListener(this)}
	
	override def unregister()  {textField.getDocument.removeDocumentListener(this) }
	
	override def getType = ListenerType.UpdateMarkersCharListener
}

class NonAccept(val actionExample: DtppAction, val editor: Editor) extends KeyListener with Listener{
	private var counter: Int = 0
	override def keyTyped(e: KeyEvent): Unit = {
		counter = counter +1
		if(e.getKeyChar == '\u001B'){
			actionExample.receiveInput(EscapeInput(ModifierCombination.NONE))
		} else if (counter > 2) {
			actionExample.receiveInput(AcceptInput())
		}
	}
	
	override def keyPressed(e: KeyEvent) = keyTyped(e)
	
	override def keyReleased(e: KeyEvent) = keyTyped(e)
	
	override def register() = editor.getContentComponent.addKeyListener(this)
	
	override def unregister() = editor.getContentComponent.removeKeyListener(this)
	
	override def getType = ListenerType.NonAccept
}

class NonCharListener(val textField: JBTextField, val action: DtppAction) extends Listener{
	override def register(): Unit = {
		textField.getInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "escape")
		textField.getActionMap.put("escape", new SomeAction(action, EscapeInput(ModifierCombination.NONE)))
		
		textField.getInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, InputEvent.ALT_DOWN_MASK), "escape+alt")
		textField.getActionMap.put("escape+alt", new SomeAction(action, EscapeInput(ModifierCombination.ALT)))
		
		textField.getInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "enter")
		textField.getActionMap.put("enter", new SomeAction(action, EnterInput(ModifierCombination.NONE)))
		
		textField.getInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.ALT_DOWN_MASK | InputEvent.CTRL_DOWN_MASK), "enter+alt+ctrl")
		textField.getActionMap.put("enter+alt+ctrl", new SomeAction(action, EnterInput(ModifierCombination.ALT_CTRL)))
		
		textField.getInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.CTRL_DOWN_MASK), "enter+ctrl")
		textField.getActionMap.put("enter+ctrl", new SomeAction(action, EnterInput(ModifierCombination.CTRL)))
		
		textField.getInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.ALT_DOWN_MASK), "enter+alt")
		textField.getActionMap.put("enter+alt", new SomeAction(action, EnterInput(ModifierCombination.ALT)))
		
		textField.getInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_I, InputEvent.ALT_DOWN_MASK), "i+alt")
		textField.getActionMap.put("i+alt", new SomeAction(action, ScrollInput(ScrollDirection.UP,ModifierCombination.NONE)))
		
		textField.getInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_K, InputEvent.ALT_DOWN_MASK), "k+alt")
		textField.getActionMap.put("k+alt", new SomeAction(action, ScrollInput(ScrollDirection.DOWN,ModifierCombination.NONE)))
		
		textField.getInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_H, InputEvent.ALT_DOWN_MASK), "h+alt")
		textField.getActionMap.put("h+alt", new SomeAction(action, ScrollInput(ScrollDirection.HOME,ModifierCombination.NONE)))
	}
	
	override def unregister(): Unit = {
		textField.getInputMap.remove(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0))
		textField.getInputMap.remove(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, InputEvent.ALT_DOWN_MASK))
		textField.getInputMap.remove(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0))
		textField.getInputMap.remove(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.ALT_DOWN_MASK | InputEvent.CTRL_DOWN_MASK))
		textField.getInputMap.remove(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.CTRL_DOWN_MASK))
		textField.getInputMap.remove(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.ALT_DOWN_MASK))
		textField.getInputMap.remove(KeyStroke.getKeyStroke(KeyEvent.VK_I, InputEvent.ALT_DOWN_MASK))
		textField.getInputMap.remove(KeyStroke.getKeyStroke(KeyEvent.VK_K, InputEvent.ALT_DOWN_MASK))
		textField.getInputMap.remove(KeyStroke.getKeyStroke(KeyEvent.VK_H, InputEvent.ALT_DOWN_MASK))
	}
	
	class SomeAction(action: DtppAction, internalInput: Input) extends AbstractAction{
		override def actionPerformed(actionEvent: ActionEvent): Unit = {
			action.receiveInput(internalInput)
		}
	}
	
	
	override def getType = ListenerType.NonChar
}

class SelectMarkersCharListener(textField: JTextField, action: DtppAction) extends KeyListener with Listener{
	override def keyTyped(e: KeyEvent) = {
		val inputString = String.valueOf(e.getKeyChar)
		println(e.getModifiers)
		
		if(Constants.markerAlphabet.toLowerCase.contains(inputString)){
			action.receiveInput(StringInput(inputString, ModifierCombination.NONE))
		}
	}
	
	override def keyPressed(e: KeyEvent) = {}
	
	override def keyReleased(e: KeyEvent) = {}
	
	override def register() = textField.addKeyListener(this)
	
	override def unregister() = textField.removeKeyListener(this)
	
	override def getType = ListenerType.SelectMarkersCharListener
}