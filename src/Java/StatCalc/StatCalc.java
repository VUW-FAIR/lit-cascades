import java.io.BufferedReader;
import java.io.FileReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.io.File;

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.stream.Stream;
import java.util.Arrays;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;

public class StatCalc {


  static boolean image = false;
  static boolean text = false;
  static boolean console = false;
  static int outW = 78;
  static int leftW = 28;
  static String hcgap = "   | ";
  static String br = "|";
  static String[] traitNames = new String[]{"O", "C", "E", "A", "N"};
  static String[] valueNames = new String[]{"ST", "SE", "CO", "OC"};
  static String[] statNames = new String[] {
      "empty", "traitValueLines", "traitLines", "valueLines",
      "characters", "traits", "values"
  };
  static String[] resultNames = new String[] {
    "  Matched",
    "  Partial",
    "Unmatched",
    "    Total"
  };
  static Map<String, String> codeMap = new HashMap<String, String>();

  static int lineMax = 15;

  public static void main(String[] args) {
    for (String s: args) {
      if (s.equalsIgnoreCase("-g")) {
        image = true;
      } else if (s.equalsIgnoreCase("-t")) {
        text = true;
      } else if (s.equalsIgnoreCase("-c")) {
        console = true;
      } else if (s.equalsIgnoreCase("-a")) {
        image = text = console = true;
      }
    }

    codeMap.put("E", statNames[0]);
    codeMap.put("TV", statNames[1]);
    codeMap.put("T", statNames[2]);
    codeMap.put("V", statNames[3]);

    if (!image && !text && !console) {
      console = true;
    }

    List<String> optionList = new ArrayList<String>();
    if (console) { optionList.add("Console"); }
    if (image) { optionList.add("Image"); }
    if (text) { optionList.add("Text"); }
    String message = "";
    for (String s : optionList) {
      message += s + ", ";
    }
    message = message.substring(0, message.length()-2);
    print("Output Format: " + message);

    StatCalc sc = new StatCalc();
    List<List<Integer>> p = sc.getPermutations(4);
    print("Permutations: " + p.size());
    for (List<Integer> l : p) {
      String s = "";
      for (Integer i : l) {
        s += i + " ";
      }
      print(s);
    }
    System.exit(1);


    File f1 = new File("../S_N_S_Kelsey.csv");
    File f2 = new File("../S_N_S_Issey.csv");
    File f3 = new File("../S_N_S_Anushka.csv");

    List<CodeLine> s1 = sc.createStats(f1);
    List<CodeLine> s2 = sc.createStats(f2);
    List<CodeLine> s3 = sc.createStats(f3);

    print("E: " + countLines(s1, lineMax, 0) + "," + countLines(s2, lineMax, 0) + "," + countLines(s3, lineMax, 0));
    print("TV: " + countLines(s1, lineMax, 1) + "," + countLines(s2, lineMax, 1) + "," + countLines(s3, lineMax, 1));
    print("T: " + countLines(s1, lineMax, 2) + "," + countLines(s2, lineMax, 2) + "," + countLines(s3, lineMax, 2));
    print("V: " + countLines(s1, lineMax, 3) + "," + countLines(s2, lineMax, 3) + "," + countLines(s3, lineMax, 3));

    CodeLineStats[] results = sc.compareStats(s1, s2, s3);

    List<String> lines = new ArrayList<String>();

    //Header
    lines.add(dup("", "=", outW));
    lines.add(dup("--- Human Coding Stats ", "-", outW));
    lines.add(dup("", "=", outW));
    lines.add(dup(("Total Lines: " + extend(s1.size(),5)), " ", outW - 1) + br);
    lines.add(dup("     HC1     ---     HC2     ---     HC3     ", "-", outW-1, true) + br);

    //Empty Lines
    printStats("Empty Lines", results, statNames[0], lines);


    //Trait and Value Lines, Trait Only Lines, Value Only Lines
    printStats("Lines with Traits and Values", results, statNames[1], lines);
    printStats("Lines with only Traits", results, statNames[2], lines);
    printStats("Lines with only Values", results, statNames[3], lines);

    /*
    //All Characters
    printStats("All Characters", new int[]{results[0].chars[0], results[1].chars[0], results[0].chars[1], results[1].chars[1]}, lines);

    //All Traits
    printStats("All Traits", new int[]{results[0].traits[0], results[1].traits[0], results[0].traits[1], results[1].traits[1]}, lines);

    //All Values
    printStats("All Values", new int[]{results[0].values[0], results[1].values[0], results[0].values[1], results[1].values[1]}, lines);

    //Kappa
    lines.add(dup("", "-", outW));
    lines.add(dup(dup("Cohen's Kappa","",leftW+14)+br,"", outW-1) + br);
    String tkappa = dup(String.format("%.3f", results[0].tstats.kappa(results[1].tstats)), "", 13, true);
    String vkappa = dup(String.format("%.3f", results[0].vstats.kappa(results[1].vstats)), "", 13, true);
    lines.add(dup("      Traits", "", outW - 16) + br + tkappa + " " + br);
    lines.add(dup("      Values", "", outW - 16) + br + vkappa + " " + br); */


    //Footer
    lines.add(dup("", "=", outW));

    if (image) {
      //Create Image
      TextToImage.createImage(lines, "Human Coding Statistics.png");
    }

    if (text) {
      //Create Text File
      try (
            FileWriter fw = new FileWriter(new File("Human Coding Statistics.txt"));
            BufferedWriter bw = new BufferedWriter(fw);
            PrintWriter pw = new PrintWriter(bw)) {
        for (String s : lines) {
          pw.println(s);
        }
      } catch (Exception e) {
          e.printStackTrace();
      }
    }

    //Print to Console
    if (console) {
      for (String s : lines) {
        System.out.println(s);
      }
    }

  }

  private static int countLines(List<CodeLine> cls, int n, int mode) {
    int count = 0;
    for (int i = 0; i < cls.size() && i < n; i++) {
      if (mode == 0) {
        if (cls.get(i).isEmpty) { count++; }
      } else if (mode == 1) {
        if (cls.get(i).hasBoth) { count++; }
      } else if (mode == 2) {
        if (cls.get(i).hasTrait && !cls.get(i).hasBoth) { count++; }
      } else if (mode == 3) {
        if (cls.get(i).hasValue && !cls.get(i).hasBoth) { count++; }
      }
    }
    return count;
  }

  private static void printStats(String title, CodeLineStats[] results, String statName, List<String> lines) {
    //Create lines for matched, partial, unmatched, and total for passed statName
    lines.add(dup(dup(title,"",leftW+2)+br,"", outW-1) + br);
    int[][] stats = new int[][] {results[0].get(statName), results[1].get(statName), results[2].get(statName)};
    for (int i = 0; i < 4; i++) {
      String leftText = dup("-","",3) + dup(resultNames[i], "", leftW-1) + br;
      String rightText = "";
      for (int j = 0; j < 3; j++) {
        String value = "" + stats[j][i];
        if (i < 3) {
          value += dup(" (" + Math.round(stats[j][i] / (stats[j][3] + .0) * 100) + "%)","",7,1);
        } else {
          value += " " + dup("","",6);
        }
        if (j < 2) {
          rightText += dup(value, "", 15, 2) + br;
        } else {
          rightText += " " + dup(value, "", 13, 2) + br;
        }
        //j < 2 ? 15 : 14
      }
      lines.add(leftText + rightText);
    }
    lines.add(dup("","",outW-1)+br);
  }

  public static String sf(double d, int n) {
    return String.format("%." + n + "f", d);
  }

  /* private static void printStats(String title, int[] resSlice, List<String> lines) {
    //Create lines for matched, unmatched, and total for passed result slice (Empty, TraitValue, etc)
    lines.add(dup(dup(title,"",leftW-2)+br,"", outW-1) + br);
    double total1 = (double) (resSlice[0] + resSlice[2]);
    double total2 = (double) (resSlice[1] + resSlice[3]);
    String matched = extend(resSlice[0],5) + " " + dup("(" +
      (Math.round(resSlice[0] / total1 * 100)) + "%)","",6) +
        hcgap + extend(resSlice[1],5) + " " + dup("(" +
        (Math.round(resSlice[1] / total2 * 100)) + "%)","",6);
    String unmatched = extend(resSlice[2],5) + " " + dup("(" +
      (Math.round(resSlice[2] / total1 * 100)) + "%)","",6) +
        hcgap + extend(resSlice[3],5) + " " + dup("(" +
        (Math.round(resSlice[3] / total2 * 100)) + "%)","",6);


    lines.add(dup("     Matched","",leftW-5)+"   |" + matched + " " + br);
    lines.add(dup("   Unmatched","",leftW-5)+"   |" + unmatched + " " + br);
    lines.add(dup("       Total","",leftW-5)+"   |" + dup(extend(resSlice[0] + resSlice[2],5),"",12) + hcgap + dup(extend(resSlice[1] + resSlice[3],5),"",12) + " |");
    lines.add(dup("","",outW-1)+br);
  }
 */
  public static String extend(Integer i, Integer s) {
    //Produces a string based on input (i) and desired size (s) in characters
    String r = "" + i;
    while (r.length() < s) {
      r = " " + r;
    }
    return r;
  }

  public static String dup(String s1, String s2, int n) {
    return dup(s1, s2, n, 0); }
  public static String dup(String s1, String s2, int n, boolean right) {
    return dup(s1, s2, n, right ? 1 : 0); }
  public static String dup(String s1, String s2, int n, int dir) {
    //Extends a string (s1) to the specified length (n) using the filler character (s2)
    //If right is true, adds characters to the left of the input string (right aligned)
    if (s1 == null) { return s1; }
    boolean alt = s1.length() % 2 != 0;
    if (s2 == null || s2.isEmpty()) { s2 = " "; }
    while (s1.length() < n) {
      if (dir == 0 || (dir == 2 && alt)) {
        s1 = s1 + s2;
      } else if (dir == 1 || (dir == 2 && !alt)) {
        s1 = s2 + s1;
      }
      alt = !alt;
    }
    return s1;
  }

  public CodeLineStats[] compareStats(List<CodeLine> cls1, List<CodeLine> cls2, List<CodeLine> cls3) {
    CodeLineStats[] stats = new CodeLineStats[] {
        new CodeLineStats(statNames), new CodeLineStats(statNames), new CodeLineStats(statNames)
    };

    List<String> tv = new ArrayList<String>();
    for (int i = 0; i < cls1.size() && i < lineMax; i++) {
      CodeLine cl1 = cls1.get(i);
      CodeLine cl2 = cls2.get(i);
      CodeLine cl3 = cls3.get(i);

      //Matched, Partial, or Unmatched for each code line:

      //-- Sentences
      //Is Empty
      int empty = compareLines(new CodeLine[] {cl1, cl2, cl3}, stats, i);

      //if (compareEmpty(cl1, cl2, cl3, stats, i) == 7) { continue; }

      //Has Trait-Values
      //Has Traits only
      //Has Values only
      //compareTraitValue(cl1, cl2, cl3, stats, i);
      tv.add(getTV(cl1) + " | " + getTV(cl2) + " | " + getTV(cl3));
      if (empty == 7) { continue; }

      //-- Code Stats (in sentences)
      //Character
      //Trait
      //Values
      compareCodes(new CodeLine[] {cl1, cl2, cl3}, stats);
    }
    for (String s : tv) {
      print(s);
    }

    return stats;
  }

  private static String getTV(CodeLine cl) {
    if (cl.isEmpty) { return " E"; }
    if (cl.hasBoth) { return "TV"; }
    if (cl.hasTrait) { return " T"; }
    if (cl.hasValue) { return " V"; }
    return " C";
  }

  public static int compareLines(CodeLine[] codeLines, CodeLineStats[] stats, int lineCount) {
    Map<String, Integer> codes = new HashMap<String, Integer>();
    print("");
    print("Line: " + (lineCount + 1));
    for (CodeLine codeLine : codeLines) {
      String code = codeLine.getCode();
      if (codes.containsKey(code)) {
        codes.put(code, codes.get(code)+1);
      } else {
        codes.put(code, 0);
      }
      print(code + " : " + codes.get(code));
    }

    for (int i = 0; i < codeLines.length; i++) {
      String code = codeLines[i].getCode();
      if (codes.containsKey(code)) {
        String statName = codeMap.get(code);
        stats[i].add(statName, codes.get(code));
      }
    }

    int value = 0;
    for (int i = 0; i < codeLines.length; i++) {
      value += (codeLines[i].isEmpty ? 1 : 0) << (codeLines.length - 1 - i);
    }
    return value;
  }

  public static void compareCodes(CodeLine[] codeLines, CodeLineStats[] stats) {
    //Get unique characters in this line
    Map<String, Integer> characters = new HashMap<String, Integer>();
    List<List<List<Code>>> codesByCharacter = new ArrayList<List<List<Code>>>();
    for (int i = 0; i < codeLines.length; i++) {
      CodeLine codeLine = codeLines[i];
      if (codeLine.codes == null) { continue; }
      for (Code code : codeLine.codes) {
        if (!characters.containsKey(code.character)) {
          characters.put(code.character, characters.size());
          ArrayList<List<Code>> codes = new ArrayList<List<Code>>();
          codes.add(new ArrayList<Code>());
          codes.add(new ArrayList<Code>());
          codes.add(new ArrayList<Code>());
          codesByCharacter.add(codes);
        }

        codesByCharacter.get(characters.get(code.character)).get(i).add(code);
        print("Added: " + code.character + "(CL" + i + ")" + ", " + code);
      }
    }



  /*   for (List<List<Code>> characterList : codesByCharacter) {
      //For each character code, try to maximise the value of matched codes
      //Try all permutations
      List<List<List<Code>>> permuations = new List<List<List<Code>>>();
      List<Integer> scores = new ArrayList<Integer>();
      for (int i = 0; i <= characterList.get(0).size(); i++) {
        for (int j = 0; j <= characterList.get(1).size(); j++) {
          for (int k = 0; k <= characterList.get(2).size(); k++) {
            scores.add();
          }
        }
      }



    }
 */



  }

  public List<List<Integer>> getPermutations(int size) {
    List<List<Integer>> permutations = new ArrayList<List<Integer>>();
    List<Integer> current = new ArrayList<Integer>(size);
    recursivePermute(permutations, current, 0, size);
    return permutations;
  }

  private void recursivePermute(List<List<Integer>> permutations, List<Integer> current, int position, int maxSize) {
    top:    
    for (int i = 0; i < maxSize; i++) {      
      for (int value : current) {
        if (i == value) { break top; }
      }
      if (current.size() > position) {
        current.set(position, i);
      } else {
        current.add(i);
      }

      if (position + 1 == maxSize) {
        permutations.add(current);
        current = cloneList(current);
      } else {
        recursivePermute(permutations, current, position + 1, maxSize);
      }
    }
  }

  private List<Integer> cloneList(List<Integer> list) {
    List<Integer> clone = new ArrayList<Integer>(list.size());
    for (Integer i : list) {
      clone.add(i);
    }
    return clone;
  }

  static int total0;
  static int total1;

  public static void compareCodes(CodeLine cl1, CodeLine cl2, CodeLine cl3, CodeLineStats[] stats) {
    //Check if each char,trait,value code is in both lines
    List<Integer> matched2 = new ArrayList<Integer>();
    List<Integer> matched3 = new ArrayList<Integer>();

    /* if (cl1.codes != null) {
      for (Code c1 : cl1.codes) {
        int bestMatch2 = 0;
        int bestMatchIdx2 = -1;
        int bestMatch3 = 0;
        int bestMatchIdx3 = -1;
        for (int i = 0; cl2.codes != null && i < cl2.codes.size(); i++) {
          Code c2 = cl2.codes.get(i);
          if (!c1.character.equals(c2.character)) {
            continue;
          }

          boolean trait = c1.traitCode != null && c2.traitCode != null &&
              c1.traitCode.equals(c2.traitCode) && c1.traitMod == c2.traitMod;
          boolean value = c1.valueCode != null && c2.valueCode != null &&
              c1.valueCode.equals(c2.valueCode) && c1.valueMod == c2.valueMod;

          int newMatch = 1;

          if (trait && value) {
            newMatch = 4;
          } else if (trait) {
            newMatch = 2;
          } else if (value) {
            newMatch = 3;
          } else {
            newMatch = 1;
          }

          if (newMatch > bestMatch) {
            bestMatch = newMatch;
            bestMatchIdx = i;
          }
        }

        if (bestMatchIdx != -1) {
          Code lastC2 = cl2.codes.get(bestMatchIdx);
          matched.add(bestMatchIdx);
          stats[0].chars[0]++;
          stats[1].chars[0]++;
          if (bestMatch == 2 || bestMatch == 4) {
            stats[0].traits[0]++;
            stats[1].traits[0]++;
            stats[0].tstats.add(c1.traitCode, 0 + (c1.traitMod ? 2 : 0));
            stats[1].tstats.add(lastC2.traitCode, 0 + (lastC2.traitMod ? 2 : 0));
            total0++;
            total1++;
          } else {
            if (c1.traitCode != null) {
              stats[0].traits[1]++;
              stats[0].tstats.add(c1.traitCode, 1 + (c1.traitMod ? 2 : 0));
              total0++;
            }
            if (lastC2.traitCode != null) {
              stats[1].traits[1]++;
              stats[1].tstats.add(lastC2.traitCode, 1 + (lastC2.traitMod ? 2 : 0));
              total1++;
            }
          }

          if (bestMatch == 3 || bestMatch == 4) {
            stats[0].values[0]++;
            stats[1].values[0]++;
            stats[0].vstats.add(c1.valueCode, 0 + (c1.valueMod ? 2 : 0));
            stats[1].vstats.add(lastC2.valueCode, 0 + (lastC2.valueMod ? 2 : 0));
          } else {
            if (c1.valueCode != null) {
              stats[0].values[1]++;
              stats[0].vstats.add(c1.valueCode, 1 + (c1.valueMod ? 2 : 0));
            }
            if (lastC2.valueCode != null) {
              stats[1].values[1]++;
              stats[1].vstats.add(lastC2.valueCode, 1 + (lastC2.valueMod ? 2 : 0));
            }
          }
        } else {
          stats[0].chars[1]++;
          if (c1.traitCode != null) {
            stats[0].traits[1]++;
            stats[0].tstats.add(c1.traitCode, 1 + (c1.traitMod ? 2 : 0));
            total0++;
          }
          if (c1.valueCode != null) {
            stats[0].values[1]++;
            stats[0].vstats.add(c1.valueCode, 1 + (c1.valueMod ? 2 : 0));
          }
        }
      }
    }

    //Check any remaining codes from cl2
    Collections.sort(matched);
    if (cl2.codes != null) {
      for (int i = 0; i < cl2.codes.size(); i++) {
        Code c2 = cl2.codes.get(i);
        if (matched.size() > 0 && matched.get(0) == i) {
          matched.remove(0);
          continue;
        }

        //These codes can't match any from cl1, so only unmatched scores need to be incremented
        stats[1].chars[1]++;
        if (c2.traitCode != null) {
          stats[1].traits[1]++;
          stats[1].tstats.add(c2.traitCode, 1 + (c2.traitMod ? 2 : 0));
          total1++;
        }
        if (c2.valueCode != null) {
          stats[1].values[1]++;
          stats[1].vstats.add(c2.valueCode, 1 + (c2.valueMod ? 2 : 0));
        }
      }
    } */
  }


  public static void print(String s) {
    System.out.println(s);
  }


  public List<CodeLine> createStats(File f) {
    List<CodeLine> sl = new ArrayList<CodeLine>();
    try (
        FileReader fr = new FileReader(f);
        BufferedReader br = new BufferedReader(fr)
      ) {
      String line;
      while ((line = br.readLine()) != null) {
        sl.add(new CodeLine(line));
      }

    } catch (Exception e) {
      e.printStackTrace();
    }
    return sl;
  }

  private static void clAdd(CodeLineStats[] stats, String name, int... values) {
    for (int i = 0; i < stats.length; i++) {
      stats[i].add(name, values[i]);
    }
  }
  private static boolean clPartial(int value) {
    if (value == 6 || value == 5 || value == 3) {
      return true;
    }
    return false;
  }

  private static int[] clPartialOnly(int[] values) {
    int[] ret = new int[4];
    for (int i = 0; i < values.length; i++) {
      if (values[i] == 0) {
        ret[3] = i;
        ret[i] = -1;
      } else {
        ret[i] = 1;
      }
    }
    return ret;
  }

  private static void clPartialAdd(int[] values1, int[] values2, int[] values3,
                                          String name1, String name2, String name3, CodeLineStats[] stats) {
    int[] partial = clPartialOnly(values1);
    String s = "";
    for (int i : partial) { s += i + ","; }
    for (int i : values1) { s += i + ","; }
    print(s);
    clAdd(stats, name1, partial[0], partial[1], partial[2]);
    if (values2[partial[3]] == 1 || values3[partial[3]] == 1) {
      clAdd(stats, values2[partial[3]] == 1 ? name2 : name3,
        partial[3] == 0 ? 0 : -1, partial[3] == 1 ? 0 : -1, partial[3] == 2 ? 0 : -1);
    }
  }

  private static boolean clUnmatched(int value) {
    if (value == 4 || value == 2 || value == 1) {
      return true;
    }
    return false;
  }

  private static int[] clUnmatchedGet(int[] values) {
    int[] ret = new int[3];
    boolean first = true;
    for (int i = 0; i < 3; i++) {
      if (values[i] == 1) {
        ret[0] = i;
      } else {
        if (first) {
          ret[1] = i;
          first = false;
        } else {
          ret[2] = i;
        }
      }
    }
    return ret;
  }

  private static void clUnmatchedAdd(int[][] values, String name1, String name2, String name3, CodeLineStats[] stats) {
    for (int i = 0; i < 3; i++) {
      if (values[0][i] == 1) {
        clAdd(stats, name1, i == 0 ? 0 : -1, i == 1 ? 0 : -1, i == 2 ? 0 : -1);
      } else if (values[1][i] == 1) {
        clAdd(stats, name2, i == 0 ? 0 : -1, i == 1 ? 0 : -1, i == 2 ? 0 : -1);
      } else if (values[2][i] == 1) {
        clAdd(stats, name3, i == 0 ? 0 : -1, i == 1 ? 0 : -1, i == 2 ? 0 : -1);
      }
    }
  }

  private class CodeLineStats {
    //A container class
    List<CodeLineStat> stats;
    Map<String, Integer> statIdMap;
    Stats tstats = new Stats(traitNames, 4);
    Stats vstats = new Stats(valueNames, 4);

    CodeLineStats(String[] names) {
      stats = new ArrayList<CodeLineStat>();
      statIdMap = new HashMap<String, Integer>();
      for (int i = 0; i < names.length; i++) {
        stats.add(new CodeLineStat());
        statIdMap.put(names[i], i);
      }
    }

    void add(String name, int id) {
      if (statIdMap.containsKey(name)) {
        CodeLineStat cls = stats.get(statIdMap.get(name));
        cls.add(id);
        print("Add: " + name + ", " + id + " - " + cls.print());
      }
    }

    int[] get(String name) {
      if (statIdMap.containsKey(name)) {
        CodeLineStat clt = stats.get(statIdMap.get(name));
        return new int[]{clt.matched, clt.partial, clt.unmatched,
                        (clt.matched + clt.partial + clt.unmatched)};
      }
      return null;
    }

    private class CodeLineStat {
      int matched = 0;
      int partial = 0;
      int unmatched = 0;
      void add(int id) {
        if (id == -1) { return; }
        if (id == 2) {
          matched++;
        } else if (id == 1) {
          partial++;
        } else if (id == 0) {
          unmatched++;
        }
      }
      String print() {
        return matched + "," + partial + "," + unmatched + ",(" + (matched + partial + unmatched) + ")";
      }
    }
  }

  private class Stats {
    String[] statNames;
    Map<String, Integer> statIdMap;
    int[][] statValues;
    int total;

    Stats(String[] statNames, int statSize) {
      this.statNames = statNames;
      this.statValues = new int[statNames.length][statSize];
      statIdMap = new HashMap<String, Integer>();
      for (int i = 0; i < statNames.length; i++) {
        statIdMap.put(statNames[i], i);
      }
    }

    void add(String name, int id) {
      if (statIdMap.containsKey(name)) {
        statValues[statIdMap.get(name)][id]++;
        total++;
      }
    }

    double kappa(Stats other) {
      int[] rowSums = sums();
      int[] colSums = other.sums();
      int rowTotal = Arrays.stream(rowSums).reduce(0, (int sum, int n) -> sum + n);
      int colTotal = Arrays.stream(colSums).reduce(0, (int sum, int n) -> sum + n);
      int total = rowTotal + colTotal;

      int[] acc = acc();
      int sigmaAcc = Arrays.stream(acc).reduce(0, (int sum, int n) -> sum + n);
      double[] diags = diagonals(rowSums, colSums, total);
      double sigmaDiag = Arrays.stream(diags).reduce(0, (double sum, double n) -> sum + n);
      String s3 = "";
      for (double d : diags) {
        s3 += String.format("%.2f",d) + ",";
      }
      double kappa = (sigmaAcc - sigmaDiag) / (total - sigmaDiag);
      return kappa;
    }

    double[] diagonals(int[] rowSums, int[] colSums, int total) {
      double[] ans = new double[rowSums.length];
      for (int i = 0; i < rowSums.length; i++) {
        ans[i] = rowSums[i] * colSums[i] / (total + .0);
      }
      return ans;
    }

    int[] acc() {
      List<Integer> ret = new ArrayList<Integer>();
      for (int i = 0; i < statValues.length; i++) {
        ret.add(statValues[i][0]);
        ret.add(statValues[i][2]);
      }
      return toIntArray(ret);
    }

    int[] sums() {
      List<Integer> ret = new ArrayList<Integer>();
      for (int i = 0; i < statValues.length; i++) {
        ret.add(statValues[i][0] + statValues[i][1]);
        ret.add(statValues[i][2] + statValues[i][3]);
      }
      return toIntArray(ret);
    }

    int[] toIntArray(List<Integer> list) {
      int[] ret = new int[list.size()];
      for (int i = 0; i < list.size(); i++) {
        ret[i] = list.get(i);
      }
      return ret;
    }
  }

  private class CodeLine {
    //Represents codes for a single sentence
    boolean isEmpty = false;
    boolean hasTrait = false;
    boolean hasValue = false;
    boolean hasBoth = false;
    List<Code> codes;

    CodeLine(String line) {
      line = line.trim();
      if (line.equals("")) {
        isEmpty = true;
        return;
      }

      codes = new ArrayList<Code>();
      String[] lineSplit = line.split(",");
      for (String codeString : lineSplit) {
        String css = codeString.replace("\"", "").trim();
        if (css.length() < 3) { continue; }
        Code code = new Code(css);
        if (!hasTrait && code.traitCode != null) {
          hasTrait = true;
        }
        if (!hasValue && code.valueCode != null) {
          hasValue = true;
        }
        codes.add(code);
      }
      hasBoth = hasTrait && hasValue;
    }

    String getCode() {
      if (isEmpty) { return "E"; }
      if (hasBoth) { return "TV"; }
      if (hasTrait) { return "T"; }
      if (hasValue) { return "V"; }
      return "C";
    }
  }

  private class Code {
    //Represents a single code for a sentence
    //  e.g. PEOPL:A+:SE-
    //     Character:Trait(Mod):Value(Mod)
    //Positive and neutral mod values -> false
    //Negative mod values -> true
    public String character;
    public String traitCode;
    public boolean traitMod;
    public String valueCode;
    public boolean valueMod;
    public String code;

    Code(String code) {
      // NAME:TRAIT:VALUE
      String[] codeSplit = code.split(":");
      character = codeSplit[0];

      if (!codeSplit[1].toUpperCase().equals("X")) {
        traitCode = codeSplit[1].substring(0,1);
        if (codeSplit[1].length() > 1) {
          traitMod = codeSplit[1].substring(1,2).equals("-");
        }
      }
      if (!codeSplit[2].toUpperCase().equals("X")) {
        valueCode = codeSplit[2].substring(0,2);
        if (codeSplit[2].length() > 2) {
          valueMod = codeSplit[2].substring(2,3).equals("-");
        }
      }
      this.code = code;
    }

    @Override
    public String toString() {
      return code + "(" + traitMod + "," + valueMod + ")";
    }
  }
}