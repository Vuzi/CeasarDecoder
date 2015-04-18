package fr.vuzi.decoder

object CeasarDecoder {
  /**
   * Get the frequence of all the letters contained in the given sequence
   */
  def getFrequence(s: String):Map[Char, Double] = {
    s.foldLeft[(Int, Map[Char, Int])](0, Map.empty) {
      case ((i, acc), char) if char.isLetter || char == ' ' =>
        val c = char.toLower;
        (i + 1, acc + (c -> (acc.get(c).getOrElse(0) + 1)))
      case (acc, char) => acc
    } match {
      case (i, occurences) => occurences.foldLeft[Map[Char, Double]](Map.empty) {
        case (acc, (char, value)) => (acc + (char -> value / i.doubleValue()))
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
          acc + (c - 'z' + 'a' - 1).toChar
        case c =>
          acc + c.toChar
      }
    case (acc, char) => acc + char
  }

  /**
   * Compute the entropy between the given string and the base
   * string of the object
   */
  private def getEntropy(s:String, base: Map[Char, Double]):Double = {
    s.foldLeft[(Int, Double)]((0, 0D)) {
      case ((i, entropy), char) if char.isLetter => (
        i+1,
        entropy + Math.log(base.getOrElse(char.toLower, 0D))
      )
      case (entropy, _) => entropy
    } match {
      case (i, entropy) => - entropy / Math.log(2D) / i
    }
  }

  /**
   * Decode the given string
   */
  def decode(toDecrypt: String, base: String)(result:String => Unit) {
    val _base = getFrequence(base);
    (0 to 26).foldLeft[Map[Double,String]](Map()) { (decryptedEntropies, i) =>
      val decrypted = decrypt(toDecrypt, i);
      val entropy = getEntropy(decrypted, _base);
      println("[" + i + "] " + decrypted + " : " + entropy);
      (decryptedEntropies + (entropy -> decrypted))
    } match { case decryptedEntropies =>
      result(decryptedEntropies.minBy(_._1)._2);
    }
  }
}