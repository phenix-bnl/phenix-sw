// #include "PHNodeHelper.h"
// #include "PHCompositeNode.h"
// #include "PHNodeIterator.h"
// #include "PHIODataNode.h"
// #include "PHTable.hh"

// //_____________________________________________________________________________
// template<class T>
// T*
// PHNodeHelper<T>::getTable(const char* tableName, PHCompositeNode* from)
// {
//   assert(from!=0);
//   PHNodeIterator iter(from);

//   PHIODataNode<PHTable>* node = 
//     static_cast<PHIODataNode<PHTable>*>(iter.findFirst("PHIODataNode",
// 						       tableName));

//   T* rv=0;

//   if ( node ) 
//     {
//       rv = static_cast<T*>(node->getData());
//     }
//   return rv;
// }
