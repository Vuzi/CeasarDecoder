package fr.vuzi.decoder

import scala.collection.mutable.Map;
import java.lang.StringBuilder;

class CeasarDecoder(base: String) {
  
  val _base : Map[Char, Double] = getFrequence(base);
  
  /**
   * Get the frequence of all the letters contained in the given sequence
   */
  private def getFrequence(s: String):Map[Char, Double] = {
      val occurences:Map[Char, Int] = Map();
      val frequences:Map[Char, Double] = Map();
      var i:Int = 0;
      
      // For each character
      s.map( char => {
        // If it is a letter
        if(char.isLetter || char == ' ') {
          val c = char.toLower; // Convert to lower case  
          i += 1;
          occurences += (c -> {
            1 + occurences.getOrElse(c, 0); // +1 to the current frequence
          });
        }
      });
      
      occurences.map( value => {
        frequences += (value._1 -> (value._2 / i.doubleValue()));
      });
      
      return frequences;
  }
  
  /**
   * Decrypt the given string using the provided value
   */
  def decrypt(s:String, dec:Int):String = {
    val decrypted = new StringBuilder();
    
    // For each character
    s.map( char => {
      // If it is a letter
      if(char.isLetter) {
        val c = (char.toLower + dec).toChar;
        if(c > 'z') {
          decrypted.append((c - 'z' + 'a' - 1).toChar);
        } else
          decrypted.append(c.toChar);
      } else
        decrypted.append(char);
    });
    
    return decrypted.toString;
  }
  
  /**
   * Compute the entropy between the given string and the base
   * string of the object
   */
  private def getEntropy(s:String):Double = {
    var entropy:Double = 0D;
    var i:Int = 0;
  
    // For each character
    s.map( char => {
      // If it is a letter
      if(char.isLetter) {
        val c = char.toLower; // Convert to lower case  
        i += 1;
        
        entropy += Math.log(_base.getOrElse(c, 0D));
      }
    });
    
    return  - entropy / Math.log(2D) / i;
  }
  
  
  /**
   * Decode the given string
   */
  def decode(toDecrypt: String, result:String => Unit) {
    // Get all the entropies
    val decryptedEntropies:Map[Double,String] = Map();
   
    for(i <- 0 until 26) {
      val decrypted = this.decrypt(toDecrypt, i);
      val entropy = this.getEntropy(decrypted);
      
      println("[" + i + "] " + decrypted + " : " + entropy);
      
      decryptedEntropies += (entropy -> decrypted);
    }
    
    // Return the result with the minimum entropy
    result(decryptedEntropies.minBy(_._1)._2);
  }
  
}