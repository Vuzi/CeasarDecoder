package fr.vuzi.main

import fr.vuzi.decoder.CeasarDecoder;

/**
 * @author Vuzi
 */

object App {

  /**
   * Entry point
   */
  def main(args: Array[String]) {

    if(args.length < 1) {
      println("Usage : prog <sentence to break> [<frequence file>]");
      System.exit(1);
    }

    val sentence:String = args(0);
    val frequence:String = {
      if(args.length >= 2) {
        io.Source.fromFile(args(1)).mkString
      } else {
        "It was impossible to dispute the dictatorial commands of my uncle. I yielded with a groan. On payment of a fee, a verger gave us the key. He, for one, was not partial to the ascent. My uncle at once showed me the way, running up the steps like a schoolboy. I followed as well as I could, though no sooner was I outside the tower, than my head began to swim. There was nothing of the eagle about me. The earth was enough for me, and no ambitious desire to soar ever entered my mind. Still things did not go badly until I had ascended 150 steps, and was near the platform, when I began to feel the rush of cold air. I could scarcely stand, when clutching the railings, I looked upwards. The railing was frail enough, but nothing to those which skirted the terrible winding staircase, that appeared, from where I stood, to ascend to the skies."+
        "Towards evening the schooner doubled Cape Skagen, the northernmost part of Denmark, crossed the Skagerrak during the night�skirted the extreme point of Norway through the gut of Cape Lindesnes, and then reached the Northern Seas. Two days later we were not far from the coast of Scotland, somewhere near what Danish sailors call Peterhead, and then the Valkyrie stretched out direct for the Faroe Islands, between Orkney and Shetland. Our vessel now felt the full force of the ocean waves, and the wind shifting, we with great difficulty made the Faroe Isles. On the eighth day, the captain made out Myganness, the westernmost of the isles, and from that moment headed direct for Portland, a cape on the southern shores of the singular island for which we were bound."+
        "The voyage offered no incident worthy of record. I bore it very well, but my uncle to his great annoyance, and even shame, was remarkably seasick! This mal de mer troubled him the more that it prevented him from questioning Captain Bjarne as to the subject of Sneffels, as to the means of communication, and the facilities of transport. All these explanations he had to adjourn to the period of his arrival. His time, meanwhile, was spent lying in bed groaning, and dwelling anxiously on the hoped�for termination of the voyage. I didn't pity him."+
        "On the eleventh day we sighted Cape Portland, over which towered Mount Myrdals Yokul, which, the weather being clear, we made out very readily. The cape itself is nothing but a huge mount of granite standing naked and alone to meet the Atlantic waves. The Valkyrie kept off the coast, steering to the westward. On all sides were to be seen whole \"schools\" of whales and sharks. After some hours we came in sight of a solitary rock in the ocean, forming a mighty vault, through which the foaming waves poured with intense fury. The islets of Westman appeared to leap from the ocean, being so low in the water as scarcely to be seen until you were right upon them. From that moment the schooner was steered to the westward in order to round Cape Reykjanes, the western point of Iceland."+
        "In three hours my tour was complete. The general impression upon my mind was sadness. No trees, no vegetation, so to speak�on all sides volcanic peaks�the huts of turf and earth�more like roofs than houses. Thanks to the heat of these residences, grass grows on the roof, which grass is carefully cut for hay. I saw but few inhabitants during my excursion, but I met a crowd on the beach, drying, salting and loading codfish, the principal article of exportation. The men appeared robust but heavy; fair-haired like Germans, but of pensive mien�exiles of a higher scale in the ladder of humanity than the Eskimos, but, I thought, much more unhappy, since with superior perceptions they are compelled to live within the limits of the Polar Circle."+
        "Sometimes they gave vent to a convulsive laugh, but by no chance did they smile. Their costume consists of a coarse capote of black wool, known in Scandinavian countries as the \"vadmel,\" a broad-brimmed hat, trousers of red serge, and a piece of leather tied with strings for a shoe�a coarse kind of moccasin. The women, though sad-looking and mournful, had rather agreeable features, without much expression. They wear a bodice and petticoat of somber vadmel. When unmarried they wear a little brown knitted cap over a crown of plaited hair; but when married, they cover their heads with a colored handkerchief, over which they tie a white scarf.";
      }
    }

    CeasarDecoder.decode(sentence, frequence) { result =>
      println("Decripted sentence : " + result)
    }
  }

}