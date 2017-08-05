package util;

import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.RangeMarker;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.TextRange;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class StubDocument implements Document{
    private String text;
    public StubDocument(String text) {
        this.text = text;
    }

    @NotNull
    @Override
    public int getTextLength(){
        return text.length();
    }

    @NotNull
    @Override
    public String getText() {
        return text;
    }

    @NotNull
    @Override
    public String getText(@NotNull TextRange range) {
        int startOffset = range.getStartOffset();
        int endOffset = range.getEndOffset();
        return text.substring(startOffset,endOffset);
    }

    @NotNull
    @Override
    public CharSequence getImmutableCharSequence() {
        return text;
    }

    @Override
    public int getLineCount() {
        return 0;
    }

    @Override
    public int getLineNumber(int i) {
        return 0;
    }

    @Override
    public int getLineStartOffset(int i) {
        return 0;
    }

    @Override
    public int getLineEndOffset(int i) {
        return 0;
    }

    @Override
    public void insertString(int i, @NotNull CharSequence charSequence) {

    }

    @Override
    public void deleteString(int i, int i1) {

    }

    @Override
    public void replaceString(int i, int i1, @NotNull CharSequence charSequence) {

    }

    @Override
    public boolean isWritable() {
        return false;
    }

    @Override
    public long getModificationStamp() {
        return 0;
    }

    @NotNull
    @Override
    public RangeMarker createRangeMarker(int i, int i1, boolean b) {
        return null;
    }

    @NotNull
    @Override
    public RangeMarker createGuardedBlock(int i, int i1) {
        return null;
    }

    @Override
    public void setText(@NotNull CharSequence charSequence) {

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
