package util;

import com.intellij.openapi.editor.*;
import com.intellij.openapi.editor.colors.EditorColorsScheme;
import com.intellij.openapi.editor.event.EditorMouseEventArea;
import com.intellij.openapi.editor.event.EditorMouseListener;
import com.intellij.openapi.editor.event.EditorMouseMotionListener;
import com.intellij.openapi.editor.markup.MarkupModel;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.border.Border;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;

public class StubEditor implements Editor {
    private ScrollingModel scrollingModel;
    private Document document;
    private Point point;
    private CaretModel caretModel;

    public StubEditor(ScrollingModel scrollingModel, Document document, Point point) {
        this.scrollingModel = scrollingModel;
        this.document = document;
        this.point = point;
    }

    public StubEditor(ScrollingModel scrollingModel, Document document, Point point, CaretModel caretModel) {
        this.scrollingModel = scrollingModel;
        this.document = document;
        this.point = point;
        this.caretModel = caretModel;
    }

    @NotNull
    @Override
    public Point offsetToXY(int offset){
        return point;
    }

    @NotNull
    @Override
    public Document getDocument() {
        return document;
    }

    @Override
    public boolean isViewer() {
        return false;
    }

    @NotNull
    @Override
    public JComponent getComponent() {
        return null;
    }

    @NotNull
    @Override
    public JComponent getContentComponent() {
        return null;
    }

    @Override
    public void setBorder(@Nullable Border border) {

    }

    @Override
    public Insets getInsets() {
        return null;
    }

    @NotNull
    @Override
    public SelectionModel getSelectionModel() {
        return null;
    }

    @NotNull
    @Override
    public MarkupModel getMarkupModel() {
        return null;
    }

    @NotNull
    @Override
    public FoldingModel getFoldingModel() {
        return null;
    }

    @NotNull
    @Override
    public ScrollingModel getScrollingModel() {
        return scrollingModel;
    }

    @NotNull
    @Override
    public CaretModel getCaretModel() {
        return caretModel;
    }

    @NotNull
    @Override
    public SoftWrapModel getSoftWrapModel() {
        return null;
    }

    @NotNull
    @Override
    public EditorSettings getSettings() {
        return null;
    }

    @NotNull
    @Override
    public EditorColorsScheme getColorsScheme() {
        return null;
    }

    @Override
    public int getLineHeight() {
        return 0;
    }

    @NotNull
    @Override
    public Point logicalPositionToXY(@NotNull LogicalPosition logicalPosition) {
        return null;
    }

    @Override
    public int logicalPositionToOffset(@NotNull LogicalPosition logicalPosition) {
        return 0;
    }

    @NotNull
    @Override
    public VisualPosition logicalToVisualPosition(@NotNull LogicalPosition logicalPosition) {
        return null;
    }

    @NotNull
    @Override
    public Point visualPositionToXY(@NotNull VisualPosition visualPosition) {
        return null;
    }

    @NotNull
    @Override
    public Point2D visualPositionToPoint2D(@NotNull VisualPosition visualPosition) {
        return null;
    }

    @NotNull
    @Override
    public LogicalPosition visualToLogicalPosition(@NotNull VisualPosition visualPosition) {
        return null;
    }

    @NotNull
    @Override
    public LogicalPosition offsetToLogicalPosition(int i) {
        return null;
    }

    @NotNull
    @Override
    public VisualPosition offsetToVisualPosition(int i) {
        return null;
    }

    @NotNull
    @Override
    public VisualPosition offsetToVisualPosition(int i, boolean b, boolean b1) {
        return null;
    }

    @NotNull
    @Override
    public LogicalPosition xyToLogicalPosition(@NotNull Point point) {
        return null;
    }

    @NotNull
    @Override
    public VisualPosition xyToVisualPosition(@NotNull Point point) {
        return null;
    }

    @NotNull
    @Override
    public VisualPosition xyToVisualPosition(@NotNull Point2D point2D) {
        return null;
    }

    @Override
    public void addEditorMouseListener(@NotNull EditorMouseListener editorMouseListener) {

    }

    @Override
    public void removeEditorMouseListener(@NotNull EditorMouseListener editorMouseListener) {

    }

    @Override
    public void addEditorMouseMotionListener(@NotNull EditorMouseMotionListener editorMouseMotionListener) {

    }

    @Override
    public void removeEditorMouseMotionListener(@NotNull EditorMouseMotionListener editorMouseMotionListener) {

    }

    @Override
    public boolean isDisposed() {
        return false;
    }

    @Nullable
    @Override
    public Project getProject() {
        return null;
    }

    @Override
    public boolean isInsertMode() {
        return false;
    }

    @Override
    public boolean isColumnMode() {
        return false;
    }

    @Override
    public boolean isOneLineMode() {
        return false;
    }

    @NotNull
    @Override
    public EditorGutter getGutter() {
        return null;
    }

    @Nullable
    @Override
    public EditorMouseEventArea getMouseEventArea(@NotNull MouseEvent mouseEvent) {
        return null;
    }

    @Override
    public void setHeaderComponent(@Nullable JComponent jComponent) {

    }

    @Override
    public boolean hasHeaderComponent() {
        return false;
    }

    @Nullable
    @Override
    public JComponent getHeaderComponent() {
        return null;
    }

    @NotNull
    @Override
    public IndentsModel getIndentsModel() {
        return null;
    }

    @NotNull
    @Override
    public InlayModel getInlayModel() {
        return null;
    }

    @NotNull
    @Override
    public EditorKind getEditorKind() {
        return null;
    }

    @Nullable
    @Override
    public <T> T getUserData(@NotNull Key<T> key) {
        return null;
    }

    @Override
    public <T> void putUserData(@NotNull Key<T> key, @Nullable T t) {

    }
}
