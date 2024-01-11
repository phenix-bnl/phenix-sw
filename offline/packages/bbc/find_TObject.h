#ifndef FIND_TOBJECT_H
#define FIND_TOBJECT_H

#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "TObject.h"


//====================================================================
// find_TObject() .... find TObject node, and cast it in proper type
//====================================================================
template <class T>
T* find_TObject(PHCompositeNode *top, char* name) {
  PHString node_type("PHIODataNode"), node_name(name);
  PHNodeIterator itop(top);
  PHNode *node = itop.findFirst(node_type, node_name);
  if(node) {
    PHIODataNode<TObject>* IoNode = (PHIODataNode<TObject>*)node;
    T* wrapper = dynamic_cast<T*>(IoNode->getData());
    if(wrapper) return wrapper;
  }
  //here comes means the search or cast failed
  cout << "find_TObject:: could not find ("<< name << ")" << endl;
  return NULL;
}

#endif
