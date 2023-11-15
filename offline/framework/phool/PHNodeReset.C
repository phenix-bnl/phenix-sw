//  Implementation of class PHNodeReset
//  Author: Matthias Messer

#include "PHNodeReset.h" 
#include "PHDataNode.h"
#include "PHIODataNode.h" 
#include "PHTable.hh"
#include "PHObject.h"

// Here goes the object independance, we need to call different reset
// functions (and do hard casts on the object type, not good if the
// object is not of the type we think it is.  Currently we have the
// old wrapped tables (PHTable) - need SetRowCount(0) and the new
// PHObjects - need Reset()
void PHNodeReset::perform(PHNode* node)
{
  if ( node->getResetFlag() != True ) return;

   if (node->getType() == "PHDataNode")
     {  
       if (node->getObjectType() == "PHTable")
	 {
 	  (((PHDataNode<PHTable>*)node)->getData())->SetRowCount(0);
	 }
       if (node->getObjectType() == "PHObject")
	 {
	  (((PHDataNode<PHObject>*)node)->getData())->Reset();
         }
     }
   else if (node->getType() == "PHIODataNode")
     {
       if (node->getObjectType() == "PHTable")
	 {
 	   (((PHIODataNode<PHTable>*)node)->getData())->SetRowCount(0);
	 }
       if (node->getObjectType() == "PHObject")
	 {
	   (((PHDataNode<PHObject>*)node)->getData())->Reset();
	 }
     }
}
