package fr.vuzi.decoder

import scala.collection.mutable.Map;
import java.lang.StringBuilder;

class CeasarDecoder(base: String) {

  val _base : Map[Char, Double] = getFrequence(base);

  /**
   * Get the frequence of all the letters contained in the given sequence
   */
  def getFrequence(s: String):Map[Char, Double] = {

    // Compute the number of letter and occurences
    val (i, occurences) = s.foldLeft[(Int, Map[Char, Int])](0, Map.empty) {
      case ((i, acc), char) if char.isLetter || char == ' ' =>   // If the char is a letter or a space
        val c = char.toLower;
        (i + 1, acc += (c -> (acc.get(c).getOrElse(0) + 1)))
      case (acc, char) => acc
    }
    
    // Compute the frequence
    return occurences.foldLeft[Map[Char, Double]](Map.empty) {
      (frequences, occ) => {
        frequences += (occ._1 -> occ._2 / i.doubleValue())
      }
    }
    
  }

  /**
   * Decrypt the given string using the provided value
   */
  def decrypt(s:String, dec:Int):String = s.foldLeft("") {
    case (acc, char) if char.isLetter =>
      (char.toLower + dec).toChar match {
        case c if c > 'z' =>
          acc + (c - 'z' + 'a' - 1).toChar // Greater than z, force back to the start of the alphabet
        case c =>
          acc + c.toChar  // Append the char
      }
    case (acc, char) => acc + char // Not a letter, ignore and append to
  }

  /**
   * Compute the entropy between the given string and the base
   * string of the object
   */
  private def getEntropy(s:String):Double = {
    val (i, entropy) = s.foldLeft[(Int, Double)]((0, 0D)) {
      case ((i, entropy), char) if char.isLetter => (
        i+1,                                                   // Number of letter to decode
        entropy + Math.log(_base.getOrElse(char.toLower, 0D))  // Total entropy 
      )
      case ((i, entropy), char) => (i, entropy)                // Ignore if not a letter
    };
      
    return -entropy / Math.log(2D) / i; // Compute entropy, then return it
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