package com.kyub.scala.strings
import org.scalatest._
import com.kyub.scala.bank.LevenshteinStringSimilarity
import com.kyub.scala.bank.NeedlemanStringSimilarity



class SimilarityTest extends FlatSpec with Matchers  {
  
  "Levenshtein Distance of 'a' and 'a' " should " be 0" in {
    LevenshteinStringSimilarity.charDistance('a', 'a') should be (0)
    
  }
  
   "Levenshtein Distance of 'a' and 'x' " should " be 1" in {
    LevenshteinStringSimilarity.charDistance('a', 'x') should be (1)
    
  }
  
  "Levenshtein Distance of 'sam chapman' and 'sam john chapman' " should " be 5" in {    
    LevenshteinStringSimilarity.evalDistance("sam chapman", "sam john chapman") should be (5)
  }
  
  "Needleman Distance of 'sam chapman' and 'sam john chapman' with gap cost of 2 " should " be 7" in { 
    NeedlemanStringSimilarity.evalDistance("sam chapman", "sam john chapman",2) should be (7)
    
  }

}