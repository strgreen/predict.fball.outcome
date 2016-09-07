package parser;

import java.io.IOException;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class Parser extends IOException {
  String url;
  String title;
  Document doc;
  
 
  public Parser (String url) throws IOException{
    doc = Jsoup.connect(url).get();
    title = doc.title();
  }


  public String getTitle() {
    return title;
  }


  public void setTitle(String title) {
    this.title = title;
  }


  public Document getDoc() {
    return doc;
  }


  public void setDoc(Document doc) {
    this.doc = doc;
  }
  
  
  
  
}
