package com.kyub.scala.bank

object LevenshteinStringSimilarity {
  
  def charDistance(c:Char,d:Char): Int = c match {
											  case `d` =>0;
											  case _ => 1
											}
	
  def evalDistance(stringA:String, stringB:String):Int = {
    val strAasList = ( stringA toList )
    val strBasList = ( stringB toList )
    val myMatrix = Array.ofDim[Int](strAasList.size,strBasList.size) 
    
    strAasList.zipWithIndex foreach {case (value,index) => myMatrix(index)(0) = index}
    strBasList.zipWithIndex foreach {case (value,index) => myMatrix(0)(index) = index}    
       
    strAasList.zipWithIndex foreach { case (valueA,indexA) if  indexA > 0 =>
      									strBasList.zipWithIndex foreach {  case (valueB,indexB) if  indexB > 0 => 
      									    myMatrix(indexA)(indexB) = List( myMatrix( indexA - 1)( indexB     ) + 1,    						  // insert
      									    						  myMatrix( indexA    )( indexB - 1 ) + 1,   						  // delete
      									    						  myMatrix( indexA - 1)( indexB - 1 ) +  charDistance(valueA,valueB)   // sub
		            		 				).min
      									case _ => Nil
      									}
      								case _ => Nil
    }
    
     
    
    
    return myMatrix(strAasList.size-1)(strBasList.size-1)
  }
  

}