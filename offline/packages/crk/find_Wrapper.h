#ifndef FIND_WRAPPER_H
#define FIND_WRAPPER_H

#include <iostream>
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"


//====================================================================
// find_Wrapper() .... find wrapper node, and cast it in proper type
//====================================================================
template <class T>
T* find_Wrapper(PHCompositeNode *top, const char* name) {
  PHString node_type("PHIODataNode"), node_name(name);
  PHNodeIterator itop(top);
  PHNode *node = itop.findFirst(node_type, node_name);
  if(node) {
    PHIODataNode<PHTable>* IoNode = (PHIODataNode<PHTable>*)node;
    T* wrapper = dynamic_cast<T*>(IoNode->getData());
    if(wrapper) return wrapper;
  }
  //here comes means the search or cast failed
  std::cout << "find_wrapper:: could not find ("<< name << ")" << std::endl;
  return NULL;
}

#endif
