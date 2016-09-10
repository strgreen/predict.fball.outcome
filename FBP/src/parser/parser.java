package parser;

import java.io.IOException;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class parser {
  public parser (String url) throws IOException{
    Document doc = Jsoup.connect(url).get();
    
  }
}

