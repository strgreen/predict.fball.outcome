package fbp;

import java.io.IOException;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import play.Play;

public class FBPmain {

  public static void main(String[] args) throws IOException {
    
    Play play = new Play();
    String url = "http://www.espn.com/college-football/playbyplay?gameId=400869187";
    
    
    Document doc = Jsoup.connect(url).get();
    
    Element test = doc.getElementById("gamepackage-play-by-play");
    
    Elements inputElements = test.getElementsByTag("li");  
    for (Element inputElement : inputElements) {  
        String key = inputElement.attr("name");  
        String value = inputElement.attr("value");  
        System.out.println("Param name: "+key+" \nParam value: "+value);  
    }  
    int testInt = 0;
  }

}
