#include <CopySvxHits.h>
#include "setIntflag.h"

#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>
#include <Fun4AllReturnCodes.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <useInt.h>

#include <iostream>

using namespace std;


CopySvxHits::CopySvxHits(const std::string &name): SubsysReco(name),
m_checkData(false)
{
  return;
}

//-------------------------------------------------------------------------------------------

int CopySvxHits::InitRun(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

//-------------------------------------------------------------------------------------------

int CopySvxHits::process_event(PHCompositeNode *topNode)
{
  if(m_checkData){
    bool check = checkData(topNode);
    return check ? EVENT_OK : ABORTEVENT;
  }

#ifdef useIntflag
  VariableArrayInt *svxarray = findNode::getClass<VariableArrayInt>(topNode, "SvxHit_VarArray");
#else
  VariableArray    *svxarray = findNode::getClass<VariableArray>(topNode, "SvxHit_VarArray");
#endif
  if (!svxarray) return EVENT_OK;

  //-----------------------------
  // to copy ID from the original object, create destination object here
  static int init=0;
  if (init==0){
    PHNodeIterator iter(topNode);
    PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

    PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(svxarray, "SvxHitAll_VarArray", "PHObject");
    dstNode->addNode(PHObjectIONode);

    init=1;
  }

  return EVENT_OK;
}

//-------------------------------------------------------------------------------------------

int CopySvxHits::End(PHCompositeNode *topNode)
{
  return EVENT_OK;
}


int CopySvxHits::checkData(PHCompositeNode *topNode)
{
#ifdef useIntflag
  VariableArrayInt *svxarray    = findNode::getClass<VariableArrayInt>(topNode, "SvxHit_VarArray");
  VariableArrayInt *svxallarray = findNode::getClass<VariableArrayInt>(topNode, "SvxHitAll_VarArray");
#else
  VariableArray    *svxarray    = findNode::getClass<VariableArray>(topNode, "SvxHit_VarArray");
  VariableArray    *svxallarray = findNode::getClass<VariableArray>(topNode, "SvxHitAll_VarArray");
#endif

  if (!svxarray||!svxallarray) return ABORTEVENT;

  unsigned int svx_size    = svxarray->get_array_size();
  unsigned int svxall_size = svxallarray->get_array_size();
  bool result_all = (svx_size==svxall_size);
  if(!result_all) cout<<"size is different: "<<svx_size<<" "<<svxall_size<<endl;

  cout<<"size : "<<svx_size<<" "<<svxall_size<<endl;

  for(unsigned int idata=0; idata< svx_size; idata++){
    bool result = (svxarray->get_array()[idata]==svxallarray->get_array()[idata]);
    result_all &= result;

    if(!result) 
      cout<<"data is different: "<<svxarray->get_array()[idata]
                            <<" "<<svxallarray->get_array()[idata]
                            <<" : "<<idata<<endl;
  }
  cout<<(result_all ? "passed" : "failed")<<endl;

  return result_all;
}

