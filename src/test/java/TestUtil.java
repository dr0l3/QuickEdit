
import com.intellij.openapi.vcs.history.VcsRevisionNumber;
import dtpp.util.EditorUtil;
import org.junit.Test;

import java.util.Collections;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;


public class TestUtil {
    @Test
    public void testStringSearch(){
        String testString =
                "public class EditorUtil {\n" +
                "    public static int getCurrentPosition(Editor editor){\n" +
                "        return editor.getCaretModel().getPrimaryCaret().getOffset();\n" +
                "    }\n" +
                "\n" +
                "    public static String getTextBetweenOffsets(int startOffset, int endOffset, Editor editor){\n" +
                "        if(startOffset < endOffset){\n" +
                "            return editor.getDocument().getText(new TextRange(startOffset, endOffset));\n" +
                "        } else {\n" +
                "            return editor.getDocument().getText(new TextRange(endOffset, startOffset));\n" +
                "        }\n" +
                "    }\n" +
                "}\n";
        List<Integer> pub_offsets = EditorUtil.getMatchesForStringInText("Pub", testString, Collections.emptyList());
        List<Integer> cla_offsets = EditorUtil.getMatchesForStringInText("cla", testString, Collections.emptyList());
        List<Integer> space_offset = EditorUtil.getMatchesForStringInText(" ", testString, Collections.emptyList());
        assertThat("First pub is in offset", pub_offsets.contains(0));
        assertThat("3 pub's", pub_offsets.size() == 3);
        assertThat("First cl is in offsets",cla_offsets.contains(7));
        assertThat("1 cla in offsets", cla_offsets.size() == 1);
        assertThat("First space is at 6", space_offset.contains(6));
        assertThat("48 spaces, tabs and newlines", space_offset.size() == 48);
    }

    @Test
    public void boundaryChecks(){
        String testString = "a";
        List<Integer> offsets = EditorUtil.getMatchesForStringInText("a", testString, Collections.emptyList());
        testString = "aa";
        assertThat("one match", offsets.size() == 1);
        offsets = EditorUtil.getMatchesForStringInText("a", testString, Collections.emptyList());
        assertThat("Both a's are caught",offsets.size() == 2);
        testString = "aaa";
        offsets = EditorUtil.getMatchesForStringInText("a", testString, Collections.emptyList());
        assertThat("Start and end a is caught", offsets.size() == 2);
        testString = "aaaa";
        offsets = EditorUtil.getMatchesForStringInText("a", testString, Collections.emptyList());
        assertThat("Two matches", offsets.size() == 2);
    }

    @Test
    public void tabsAndNewlinesAreConverted(){
        String testString = "\n";
        List<Integer> offsets = EditorUtil.getMatchesForStringInText(" ", testString, Collections.emptyList());
        assertThat("one match",offsets.size() == 1);
        testString = "\t";
        offsets = EditorUtil.getMatchesForStringInText(" ", testString, Collections.emptyList());
        assertThat("one match",offsets.size() == 1);
        testString = "a b";
        offsets = EditorUtil.getMatchesForStringInText(" ", testString, Collections.emptyList());
        assertThat("one match",offsets.size() == 1);
        testString = "a\n\tf";
        offsets = EditorUtil.getMatchesForStringInText(" ", testString, Collections.emptyList());
        assertThat("two matches",offsets.size() == 2);
    }

    @Test
    public void casingIsIgnored(){
        String testString = "A";
        List<Integer> offsets = EditorUtil.getMatchesForStringInText("a", testString, Collections.emptyList());
        assertThat("one match", offsets.size() == 1);
        testString = "a";
        offsets = EditorUtil.getMatchesForStringInText("A", testString, Collections.emptyList());
        assertThat("one match", offsets.size() == 1);
    }
}
