package FBP;

import java.io.IOException;

import parser.Parser;
import spreadsheet.SpreadsheetCreator;

public class FBPmain {

  public static void main(String[] args) throws IOException {
    SpreadsheetCreator SSCreator;
    Parser parser;
    String url = "http://www.espn.com/college-football/playbyplay?gameId=400763495";
    
    parser = new Parser(url);
    System.out.println("Title= " + parser.getTitle());
    
  }
}
