#ifndef __MUTRGDATAPROCESSUTIL__
#define __MUTRGDATAPROCESSUTIL__

#include <string>
#include <vector>
#include <map>
#include "getClass.h"

class MutrgHit;
class MutrgHitArray;

namespace MutrgDataProcessUtil{
  // obj1 will be overwritten by obj2 in this function
  template <class T>
  int SetMutrgObject(T *&obj1,T *obj2,bool flag_delete=false);

  template <class T>
  T* RegMutrgObject(T *obj,PHCompositeNode *node,
		    const char *name,const char *rename="");

  void CountMultiplicity(const MutrgHitArray *mutrg_hits,
			 std::map<unsigned int,std::vector<int> > &nhit);

  void Clustering(const MutrgHitArray *mutrg_hits,unsigned int mask,
		  std::vector<std::vector<MutrgHit*> > &clusters);

  int Clustering(MutrgHitArray *mutrg_hits,int max_cluster_size);

  void WarnDefFuncCall(const char *class_name,const char *func_name);
};

///////////////////////////////////////////////////////////////////

template <class T>
int MutrgDataProcessUtil::SetMutrgObject(T *&obj1,T *obj2,bool flag_delete){
  if(flag_delete){
    delete obj1;
    obj1=NULL;
  }

  if(obj1){
    printf("Warning - MutrgDataProcessUtil::SetMutrgObject : ");
    printf("%s is already set\n",obj1->ClassName());
    return -1;
  }

  obj1=obj2;
  return 0;
}

///////////////////////////////////////////////////////////////////

template <class T>
T* MutrgDataProcessUtil::RegMutrgObject(T *obj,PHCompositeNode *node,
					const char *name,const char *rename){
  T *obj_org=findNode::getClass<T>(node,name);

  if(obj_org){
    if(std::string(rename)==""){
      printf("Error : MutrgDataProcessUtil::RegMutrgObject : ");
      printf("%s already exists in node.\n",name);
      return NULL;
    }
    else{
      PHNodeIterator iter(node);
      iter.findFirst(name)->setName(rename);
    }
  }

  if(!obj){
    printf("Error - MutrgDataProcessUtil::RegMutrgObject : ");
    printf("Input object is NULL. Create or Set at first.\n");
    return NULL;
  }

  PHIODataNode<T> *mutrg_node=new PHIODataNode<T>(obj,name,"PHObject");
  node->addNode(mutrg_node);

  return obj;
}

///////////////////////////////////////////////////////////////////

#endif /* __MUTRGDATAPROCESSUTIL__ */
