import java.io.BufferedReader;
import java.io.FileReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.PrintWriter;

import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;
import java.nio.charset.StandardCharsets;
import java.nio.charset.Charset;

import java.util.Arrays;
import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.HashMap;
import java.util.Map.Entry;

import opennlp.tools.tokenize.Tokenizer;
import opennlp.tools.tokenize.TokenizerME;
import opennlp.tools.tokenize.TokenizerModel;
import opennlp.tools.postag.POSTaggerME;
import opennlp.tools.postag.POSModel;

public class CodingFeatures {

  final String sep = System.getProperty("file.separator");
  final String dataDirectory = "." + sep + "data" + sep;
  List<Sentence> sentences;
  Set<String> tda1710;
  Set<String> pda500;
  List<Set<String>> allport;
  Set<String> liwc;
  Map<String, List<Double>> word2vec = new HashMap<String, List<Double>>();
  Map<String, List<Double>> word2vecNorm = new HashMap<String, List<Double>>();
  List<List<String>> coderCodes = new ArrayList<List<String>>();
  List<List<Set<String>>> valueLists = new ArrayList<List<Set<String>>>();

  String[] traits = new String[] {"O+", "O-", "C+", "C-", "E+", "E-", "A+", "A-", "N+", "N-"};
  String[] values = new String[] {"SE+", "SE-", "ST+", "ST-", "CO+", "CO-", "OC+", "OC-"};

  public void start() {
    //Load files
    loadFiles();

    //Load tokeniser and tagger
    POSTaggerME tag = null;
    Tokenizer tok = null;
    try (
          InputStream tagIn = new FileInputStream(dataDirectory + "en-pos-maxent.bin");
          InputStream tokIn = new FileInputStream(dataDirectory + "en-token.bin")) {
      POSModel posModel = new POSModel(tagIn);
      TokenizerModel tokModel = new TokenizerModel(tokIn);
      tag = new POSTaggerME(posModel);
      tok = new TokenizerME(tokModel);
    } catch (Exception e) {
      e.printStackTrace();
      return;
    }

    if (tag == null || tok == null) {
      System.exit(1);
    }

    createResults(tag, tok);
  }

  private void addHeaders(List<String> headers, String... words) {
    if (words == null) { return; }
    headers.addAll(Arrays.asList(words));
  }

  private void createResults(POSTaggerME tag, Tokenizer tok) {
    //Create results and write to file
    String c = ";";

    //Headers
    List<String> allHeaders = new ArrayList<String>();
    //<Codes>, <Word Stats>, <Tags>, <Value Lists x5>, <Word2Vec>, <Traits>, Values>
    addHeaders(allHeaders, "Coded", "Coder1", "Coder2", "Coder3", "Word Count", "Average Word Length");

    //Value Lists
    allHeaders.addAll(Arrays.asList(TAGS));
    for (String s : VALUE_FILES) {
      allHeaders.add(s.replace("values_", "").replace(".csv", "").replaceAll("_", " "));
    }
    //Trait Lists
    addHeaders(allHeaders, "PDA500", "TDA1710", "ALLPORT Metaphorical Doubtful", "ALLPORT Personal Traits", "ALLPORT Social Evaluations", "ALLPORT Temporary States");

    //Value Lists Count
    for (String s : VALUE_FILES) {
      allHeaders.add(s.replace("values_", "").replace(".csv", "").replaceAll("_", " ") + " Count");
    }

    //Trait Lists Count
    addHeaders(allHeaders, "PDA500 Count", "TDA1710 Count", "ALLPORT Metaphorical Doubtful Count", "ALLPORT Personal Traits Count", "ALLPORT Social Evaluations Count", "ALLPORT Temporary States Count");

    //Word2Vec
    addHeaders(allHeaders, "w2v cosine", "w2v euclid");

    //Traits
    addHeaders(allHeaders, "O+", "O-", "C+", "C-", "E+", "E-", "A+", "A-", "N+", "N-");

    //Values
    addHeaders(allHeaders, "SE+", "SE-", "ST+", "ST-", "CO+", "CO-", "OC+", "OC-");

    String combinedHeaders = "";
    for (String s : allHeaders) {
      combinedHeaders += addQuotes(s) + c;
    }
    combinedHeaders = combinedHeaders.substring(0, combinedHeaders.length() - 1);

    long start = System.currentTimeMillis();
    //Write to file
    try (
          FileWriter fw = new FileWriter("CodingFeatures.csv");
          BufferedWriter bw = new BufferedWriter(fw);
          PrintWriter pw = new PrintWriter(bw);) {

      //Print headers
      pw.println(combinedHeaders);

      for (int i = 0; i < sentences.size(); i++) {
        Sentence sen = sentences.get(i);

        //Calculate word stats
        sen.process(tag, tok);

        //Compare to word lists
        sen.checkAgainstWordList(tda1710);
        sen.checkAgainstWordList(pda500);
        for (Set<String> s : allport) {
          sen.checkAgainstWordList(s);
        }

        //LIWC...

        //Print sentence data
        String line = "";
        //Codes
        //7 111, 6 110, 5 101, 4 100, 3 011, 2 010, 1 001, 0 000
        line +=
          (coderCodes.get(0).get(i).isEmpty() ? 0 : 1 << 2) +
          (coderCodes.get(1).get(i).isEmpty() ? 0 : 1 << 1) +
          (coderCodes.get(2).get(i).isEmpty() ? 0 : 1);
        line += c + coderCodes.get(0).get(i);
        line += c + coderCodes.get(1).get(i);
        line += c + coderCodes.get(2).get(i);

        //Word Stats
        line += c + sen.wordCount;
        line += c + sf(sen.avgWordLength, 1);

        //Tags
        for (String t : TAGS) {
          line += c;
          if (sen.tagCounts.containsKey(t)) {
            line += sen.tagCounts.get(t);
          } else {
            line += 0;
          }
        }

        //Value Lists
        int[] valueListCounts = new int[valueLists.size()];
        for (int listId = 0; listId < valueLists.size(); listId++) {
          line += c;
          int[] valueCounts = new int[10];
          for (String word : sen.tokens) {
            for (int j = 0; j < 10; j++) {
              if (valueLists.get(listId).get(j).contains(word.toLowerCase())) {
                valueCounts[j]++;
                valueListCounts[listId]++;
              }
            }
          }
          String valueString = "";
          for (int j = 0; j < 10; j++) {
            if (valueCounts[j] == 0) { continue; }
            valueString += CODES10[j] + " " + valueCounts[j] + ", ";
          }
          if (!valueString.isEmpty()) {
            line+= "\"" + valueString.substring(0, valueString.length() - 2) + "\"";
          }
        }

        //Word Lists
        //matchedWordLists
        for (int listId = 0; listId < sen.matchedWordLists.size(); listId++) {
          line += c;
          String matched = "";
          for (String word : sen.matchedWordLists.get(listId)) {
            matched += word + ", ";
          }
          if (!matched.isEmpty()) {
            line += "\"" + matched.substring(0, matched.length() - 2) + "\"";
          }
        }

        //Value Lists count
        for (int listId = 0; listId < valueListCounts.length; listId++) {
          line += c;
          line += valueListCounts[listId];
        }

        //Trait Lists count
        for (int listId = 0; listId < sen.matchedWordLists.size(); listId++) {
          line += c;
          line += sen.matchedWordLists.get(listId).size();
        }

        //Word2Vec - Average cosine sim + euclid dist
        double totalCosineSim = 0;
        double totalEuclidDist = 0;
        double countedWords = 0;
        for (int w1 = 0; w1 < sen.tokens.length; w1++) {
          if (!word2vec.containsKey(sen.tokens[w1])) { continue; }
          for (int w2 = w1 + 1; w2 < sen.tokens.length; w2++) {
            if (!word2vec.containsKey(sen.tokens[w2])) { continue; }
            totalCosineSim += cosineSim(word2vec.get(sen.tokens[w1]), word2vec.get(sen.tokens[w2]));
            totalEuclidDist += euclidDist(word2vecNorm.get(sen.tokens[w1]), word2vecNorm.get(sen.tokens[w2]));
            countedWords++;
          }
        }

        if (countedWords > 0) {
          line += c + sf(totalCosineSim / countedWords, 2);
          line += c + sf(totalEuclidDist / countedWords, 2);
        } else {
          line += c + 0 + c + 0;
        }

        //Traits + Values
        Map<String, Integer> traitMap = new HashMap<String, Integer>();
        Map<String, Integer> valueMap = new HashMap<String, Integer>();
        String[] codeList = new String[] {
          coderCodes.get(0).get(i), coderCodes.get(1).get(i), coderCodes.get(2).get(i)
        };
        for (String codes : codeList) {
          String[] splitCodes = codes.trim().replaceAll("\"","").split(",");
          for (String code : splitCodes) {
            if (code.trim().isEmpty()) { continue; }
            String[] splitCode = code.split(":");
            String trait = splitCode[1];
            if (!trait.equals("X")) {
              if (trait.length() == 1) {
                trait += "+";
              }
              traitMap.put(trait, traitMap.containsKey(trait) ? traitMap.get(trait) + 1 : 1);
              //print("Trait: " + splitCode[1] + " > " + trait);
            }
            String value = splitCode[2];
            if (!value.equals("X")) {
              if (value.length() == 2) {
                value += "+";
              }
              valueMap.put(value, valueMap.containsKey(value) ? valueMap.get(value) + 1 : 1);
              //print("Value: " + splitCode[2] + " > " + value);
            }
          }
        }

        for (String s: traits) {
          int count = 0;
          if (traitMap.containsKey(s)) {
            count = traitMap.get(s);
          }
          line += c + count;
        }
        for (String s : values) {
          int count = 0;
          if (valueMap.containsKey(s)) {
            count = valueMap.get(s);
          }
          line += c + count;
        }

        //Print this line
        if (i < sentences.size() - 1) {
          pw.println(line);
        } else {
          pw.print(line);
        }
      }
    } catch (Exception e) {
      e.printStackTrace();
      return;
    }
    print("Time to write to file: " + (System.currentTimeMillis() - start));
  }

  private String addQuotes(String s) {
    return s.contains(" ") ? "\"" + s + "\"" : s;
  }

  private double cosineSim(List<Double> v1, List<Double> v2) {
    double sigAB = 0;
    double sigA2 = 0;
    double sigB2 = 0;
    for (int i = 0; i < v1.size(); i++) {
      sigAB += v1.get(i) * v2.get(i);
      sigA2 += Math.pow(v1.get(i), 2);
      sigB2 += Math.pow(v2.get(i), 2);
    }
    return (sigAB / (Math.sqrt(sigA2) * Math.sqrt(sigB2)));
  }

  private double euclidDist(List<Double> v1, List<Double> v2) {
    double total = 0;
    for (int i = 0; i < v1.size(); i++) {
      total += Math.pow(v2.get(i) - v1.get(i), 2);
    }
    return Math.sqrt(total);
  }

  //Suppressing Collection.add warning
  @SuppressWarnings("unchecked")
  private boolean loadFiles() {
    //Load sentences
    sentences = loadSentences("sentences.csv");

    //Load word lists
    LineOperation lop = (c, s) -> {
      c.add(s.toLowerCase());
    };

    pda500 = loadWordFile("pda500.txt", lop);
    tda1710 = loadWordFile("pda1710.csv", lop);

    allport = new ArrayList<Set<String>>();
    allport.add(loadWordFile("Metaphorical Doubtful.txt"));
    allport.add(loadWordFile("Personal Traits.txt"));
    allport.add(loadWordFile("Social Evaluations.txt"));
    allport.add(loadWordFile("Temporary States.txt"));

    //Human coding data
    coderCodes.add(loadCodes("S_N_S_Kelsey.csv"));
    coderCodes.add(loadCodes("S_N_S_Issey.csv"));
    coderCodes.add(loadCodes("S_N_S_Anushka.csv"));
    //Waiting on third coder data

    //10 Values data
    for (String s : VALUE_FILES) {
      valueLists.add(loadValues(s));
    }

    //word2vec
    loadWord2Vec("word2vec.out");

    //liwc =
    return true;
  }

  @SuppressWarnings("unchecked")
  private List<Set<String>> loadValues(String fname) {
    List<Set<String>> results = new ArrayList<Set<String>>();
    for (int i = 0; i < CODES10.length; i++) {
      results.add(new HashSet<String>());
    }

    LineOperation lop = (sets, line) -> {
      String[] split = line.split(",");
      for (int i = 0; i < split.length; i++) {
        if (split[i].isEmpty()) { continue; }
        ((List<Set<String>>)(sets)).get(i).add(split[i]);
      }
    };
    loadFile(fname, results, lop, true);
    return results;
  }

  @SuppressWarnings("unchecked")
  private List<Sentence> loadSentences(String fname) {
    List<Sentence> sentences = new ArrayList<Sentence>();
    LineOperation lop = (c, s) -> {
      c.add(new Sentence(s));
    };
    loadFile(fname, sentences, lop, true);
    return sentences;
  }

  @SuppressWarnings("unchecked")
  private List<String> loadCodes(String fname) {
    List<String> lines = new ArrayList<String>();
    LineOperation lop = (c, s) -> {
      c.add(s);
    };
    loadFile(fname, lines, lop, false);
    return lines;
  }

  @SuppressWarnings("unchecked")
  private Set<String> loadWordFile(String fname) {
    return loadWordFile(fname, null); }
  @SuppressWarnings("unchecked")
  private Set<String> loadWordFile(String fname, LineOperation lop) {
    Set<String> words = new HashSet<String>();
    if (lop == null) {
      lop = (c, s) -> c.add(s);
    }
    loadFile(fname, words, lop, true);
    return words;
  }

  @SuppressWarnings("unchecked")
  private void loadWord2Vec(String fname) {
    LineOperationMap lop = (map, s) -> {
      String[] split = s.split(" ");
      List<Double> vector = new ArrayList<Double>();
      for (int i = 1; i < split.length; i++) {
        try {
            vector.add(Double.parseDouble(split[i]));
        } catch (Exception e) {
          e.printStackTrace();
        }
      }
      if (vector.size() > 0) {
        map.put(split[0], vector);
      }
    };
    loadFileWithMap(fname, word2vec, lop);

    //Create normalised vectors
    for (Map.Entry<String, List<Double>> entry : word2vec.entrySet()) {
      List<Double> vector = entry.getValue();
      List<Double> normVector = new ArrayList<Double>();
      double mag = calcMagnitude(vector);
      for (Double v : vector) {
        normVector.add(v / mag);
      }
      word2vecNorm.put(entry.getKey(), normVector);
    }
  }

  private double calcMagnitude(List<Double> vector) {
    double sum = 0;
    for (Double v : vector) {
      sum += v * v;
    }
    return Math.sqrt(sum);
  }

  private void loadFile(String fname, Collection c, LineOperation lop, boolean dataDir) {
    File f = new File((dataDir ? dataDirectory : "../") + fname);
    try (
          FileReader fr = new FileReader(f);
          BufferedReader br = new BufferedReader(fr);) {
      String line;
      while ((line = br.readLine()) != null) {
        lop.op(c, line);
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  private void loadFileWithMap(String fname, Map m, LineOperationMap lop) {
    File f = new File(dataDirectory + fname);
    try (
          FileReader fr = new FileReader(f);
          BufferedReader br = new BufferedReader(fr);) {
      String line;
      while ((line = br.readLine()) != null) {
        lop.op(m, line);
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  interface LineOperation {
    public void op (Collection c, String line);
  }

  interface LineOperationMap {
    public void op (Map m, String line);
  }

  public static void main(String[] args) {
    new CodingFeatures().start();
  }

  private class Sentence {
    String fullText;
    String[] tokens;
    String[] tags;
    int wordCount;
    int tda1710Count;
    int pda500Count;
    int[] allportCount;
    int liwcCount;
    double avgWordLength;
    List<Set<String>> matchedWordLists = new ArrayList<Set<String>>();
    List<Integer> validTags = new ArrayList<Integer>();
    Map<String, Integer> tagCounts = new HashMap<String, Integer>();

    Sentence(String line) {
      fullText = line;
    }

    void process(POSTaggerME tagger, Tokenizer tokker) {
      tokens = tokker.tokenize(fullText);
      tags = tagger.tag(tokens);
      countWords();
      countTags();
    }

    void checkAgainstWordList(Set<String> wordList) {
      Set<String> matched = new HashSet<String>();
      for (String s : tokens) {
        if (wordList.contains(s)) {
          matched.add(s);
        }
      }
      matchedWordLists.add(matched);
    }

    void countWords() {
      //Count total words and average word length
      int sumLength = 0;
      for (int i = 0; i < tokens.length; i++) {
        String t = tokens[i];
        //Exclude special cases
        if (t.equals("CHAPTER")) {
          continue;
        }

        //Exclude numbers
        if (t.matches("[0-9]")) {
          continue;
        }

        //Exclude punctuation
        if (t.matches("[,.!?\"']")) {
          continue;
        } else if (t.startsWith("'")) {
          continue;
        }

        sumLength += t.length();
        wordCount++;
        validTags.add(i);
      }
      avgWordLength = sumLength / (wordCount + .0);
    }

    void countTags() {
      for (int i = 0; i < validTags.size(); i++) {
        String tag = tags[validTags.get(i)];

        //Group tags
        /* if (tag.equals("JJ") || tag.equals("JJR") || tag.equals("JJS")) {
          tag = "ADJ";
        } else if (tag.equals("NN") || tag.equals("NNS")) {
          tag = "NN";
        } else if (tag.equals("NNP") || tag.equals("NNPS")) {
          tag = "NNP";
        } else if (tag.equals("PRP") || tag.equals("PRP$")) {
          tag = "PRO";
        } else if (tag.equals("RB") || tag.equals("RBR") || tag.equals("RBS")) {
          tag = "ADV";
        } else if (tag.equals("VB") || tag.equals("VBD") || tag.equals("VBG") || tag.equals("VBN") ||
            tag.equals("VBP") || tag.equals("VBZ")) {
          tag = "VRB";
        } else if (tag.equals("WDT") || tag.equals("WP") || tag.equals("WP$") || tag.equals("WRB")) {
          tag = "WH";
        } */

        if (tagCounts.containsKey(tag)) {
          tagCounts.put(tag, (tagCounts.get(tag)) + 1);
        } else {
          tagCounts.put(tag, 1);
        }
      }
    }


  }

  public static String sf(Double d, int n) {
    return String.format("%." + n + "f", d);
  }

  public static void print(String s) {
    System.out.println(s);
  }

  private final String[] TAGS = new String[] { "CC","CD","DT","EX","FW","IN","JJ","JJR","JJS","LS","MD","NN","NNS","NNP","NNPS","PDT","POS","PRP","PRP$","RB","RBR","RBS","RP","SYM","TO","UH","VB","VBD","VBG","VBN","VBP","VBZ","WDT","WP","WP$","WRB" };
  private final String[] CODES10 = new String[] { "AC", "BE", "CO", "HE", "PO", "SE", "SD", "ST", "TR", "UN" };
  private final String[] VALUE_FILES = new String[] { "values_COCA1990_2012.csv", "values_COCA2016_17.csv", "values_MTurk.csv", "values_PersonalBlogs.csv", "values_PoliticalBlogs.csv", "values_Refined.csv" };
}