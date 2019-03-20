import java.io.BufferedReader;
import java.io.FileReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.io.File;

import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;
import java.nio.charset.StandardCharsets;
import java.nio.charset.Charset;

import java.util.Arrays;
import java.util.Set;
import java.util.HashSet;



public class BM {
  
  static String[] stopWordsList = new String[] {"i", "me", "my", "myself", "we", "us", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "would", "should", "could", "ought", "i'm", "you're", "he's", "she's", "it's", "we're", "they're", "i've", "you've", "we've", "they've", "i'd", "you'd", "he'd", "she'd", "we'd", "they'd", "i'll", "you'll", "he'll", "she'll", "we'll", "they'll", "isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't", "can't", "cannot", "couldn't", "mustn't", "let's", "that's", "who's", "what's", "here's", "there's", "when's", "where's", "why's", "how's", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very"};
  static Set<String> stopWords = new HashSet<String>(Arrays.asList(stopWordsList));

  public static void main(String[] args) {
    BM bm = new BM();
    String dir = ".";
    if (args.length > 0) {
      dir = args[0];
    }
    try (DirectoryStream<Path> dirStream = Files.newDirectoryStream(Paths.get(dir), "*.txt");
            FileWriter fw = new FileWriter("merged_file");
            BufferedWriter bw = new BufferedWriter(fw);
            PrintWriter out = new PrintWriter(bw)) {
        for (Path path : dirStream) {
          try (BufferedReader br = Files.newBufferedReader(path, StandardCharsets.UTF_8)) {
            String line;
            while ((line = br.readLine()) != null) {
              //To Lower Case
              line = line.toLowerCase();
              
              //Remove puncutation
              line = line.replaceAll("[^a-zA-z '-]", "");
              line = line.replaceAll(" - ", " ");
              
              //Remove stop words
              String finalLine = "";
              for (String s : line.split(" ")) {
                if (stopWords.contains(s)) {
                  continue;
                }
                finalLine += s + " ";
              }
              if (finalLine.length() > 1) {
                finalLine = finalLine.substring(0, finalLine.length() - 1);
              }
              
              out.println(finalLine);
            }
          }
          System.out.println("Done: " + path.toString());
        }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

}