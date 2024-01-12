//////////////////////////////////////////////////////////////////
//
// Utility class: DumpPHIOArray
// Author: S.Kelly 
// Date: 8/14/03
// Description: Dump the contents of PHIOArray
//              
//////////////////////////////////////////////////////////////////

#ifndef __DUMPPHIOARRAY_H__
#define __DUMPPHIOARRAY_H__

#include <DumpObject.h>
#include <PHIODataNode.h>
#include <PHNode.h>
#include <PHIOArray.hh>
#include <PHNodeDump.h>
#include <TClonesArray.h>

#include <iostream>
#include <string>


template<class Value> class DumpPHIOArray : public DumpObject {

 public:

  DumpPHIOArray(const std::string &NodeName): DumpObject(NodeName){
    return ;
  }
  
    int process_Node(PHNode *myNode){
    typedef PHIODataNode<PHIOArray> MyNode_t;    
    PHIOArray *array = NULL;
    MyNode_t *thisNode = static_cast <MyNode_t*> (myNode);
    
    if (thisNode) array = thisNode->getData();      
    
    if(array){
      *fout << "isValid(): " << array->isValid() << std::endl;
      dump(array,fout);
    }
    return 0;
  }

 private:  

  void dump(PHIOArray* array, std::ostream* os){
    TClonesArray* clones_array = array->get_array();
    for(int i = 0; i<clones_array->GetEntries();++i){
      Value* obj = static_cast<Value*>(clones_array->At(i));
      obj->print(*os);
    }      
  }
  
};

#endif
