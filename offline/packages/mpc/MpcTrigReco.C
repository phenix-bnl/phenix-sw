#include <MpcTrigReco.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>
#include <mpc2x2ContainerV1.h>
#include <mpc2x2ContentV1.h>
#include <mpc4x4ContainerV1.h>
#include <mpc4x4ContentV1.h>


#include <getClass.h>
#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <MpcMap.h>
#include <MpcTriggerMapping.h>
#include <MpcCalib.h>
#include <algorithm>
//#include <recoConsts.h>
#include <vector>

#include <iostream>
#include <fstream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;

namespace {
const double MPCTOWMIN = 0.05;
}

MpcTrigReco::MpcTrigReco(const std::string& name) : SubsysReco(name) {}

int MpcTrigReco::EndRun(const int runnumber)
{
  return EVENT_OK;
}

int MpcTrigReco::Init(PHCompositeNode *topNode)
{
  int iret = CreateNodeTree(topNode);
  return iret;
}

int MpcTrigReco::InitRun(PHCompositeNode *topNode)
{
  mpccalib = findNode::getClass<MpcCalib>(topNode,"MpcCalib");
  mpcmap = findNode::getClass<MpcMap>(topNode,"MpcMap");
  //instantiates and places on topNode
  mpctrigmap = MpcTriggerMapping::instance(topNode);
  


  if (!mpcmap || !mpccalib || !mpctrigmap)
    {

      cout << PHWHERE << "Unable to get MpcMap, MpcTriggerMapping, or MpcCalib, is Node missing?" << endl;
      return ABORTRUN;
    }
  mpctrigmap->Print();
  
  return EVENT_OK;
}

int MpcTrigReco::process_event(PHCompositeNode *topNode)
{
  mpctow = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  if (!mpctow)
    {
      cout << PHWHERE << "Unable to get mpcTowerContainer, is Node missing?" << endl;
      return ABORTRUN;
    }

  mpc2x2 = findNode::getClass<mpc2x2Container>(topNode,"mpc2x2Container");
  if (!mpc2x2)
    {
      cout << PHWHERE << "Unable to get mpc2x2Container, is Node missing?" << endl;
      return ABORTRUN;
    }

  mpc4x4 = findNode::getClass<mpc4x4Container>(topNode,"mpc4x4Container");
  if (!mpc4x4)
    {
      cout << PHWHERE << "Unable to get mpc4x4Container, is Node missing?" << endl;
      return ABORTRUN;
    }


  vector<mpc2x2ContentV1*> v2x2(144,(mpc2x2ContentV1*) 0);
  vector<mpc4x4ContentV1*> v4x4(144,(mpc4x4ContentV1*) 0);

  int ntow = mpctow->size();
  for(int itow=0;itow<ntow;++itow)
    {

      
      mpcTowerContent* tow = mpctow->getTower(itow);
      int ich = tow->get_ch();
      float tow_e = tow->get_energy();
      if(tow_e < MPCTOWMIN) continue;
      
      int gridx = mpcmap->getGridX( ich );
      //      int gridy = mpcmap->getGridY( ich );
      //      int arm = (ich>=288)?1:0;
      
      if ( gridx == -1 )
        {
          //cout << "ERROR, Non-existent channel " << ich << endl;
          continue;
        }
      
      int anum = mpcmap->getAsic(ich);
      int mnum = mpcmap->getMondo(ich);
      int mondo = 6*anum+mnum;
      if(mondo < 0 || mondo > 143)
	{
	  cout << PHWHERE << "Unexpected error with mondo index...exiting event\n";
	  return ABORTRUN;
	}
      
      mpc2x2ContentV1* m2x2 = 0;
      if(v2x2.at(mondo) == 0) 
	{
	  m2x2 = new mpc2x2ContentV1(mondo,anum,mnum);
	  v2x2.at(mondo) = m2x2;
	}
      else
	{
	  m2x2 = v2x2.at(mondo);
	}
      
      if(!m2x2->set_next_ch(ich) || !m2x2->set_next_e(tow_e))
	{
	  cout << "could not set channel/energy..unexpected error...aborting event\n";
	  return ABORTEVENT;
	}
      float led_val = mpccalib->get_led(ich);
      float tow_e2 = tow_e;
      if(led_val > 0.0) tow_e2 = tow_e2/led_val;
      
      if(!m2x2->set_next_e2(tow_e2))
	{
	  cout << "could not set energy2..unexpected error...aborting event\n";
	  return ABORTEVENT;
	}
    }
  
  for(int i2x2=0;i2x2<144;++i2x2)
    {
      mpc2x2ContentV1* m2x2 = v2x2.at(i2x2);
      if(m2x2 == 0) continue;
      
      //otherwise we fill the 2x2 container object with this
      m2x2->calc_esum();
      m2x2->calc_esum2();
      
      mpc2x2->add2x2(*m2x2);
      
    }



  for(int i4x4=0;i4x4<144;++i4x4)
    {
      unsigned int sz = mpctrigmap->get_size(i4x4);
      if(sz <= 2) continue;  //these should already be summed
      
      //create new 4x4 object;
      mpc4x4ContentV1* m4x4 = new mpc4x4ContentV1(i4x4);
      
      for(unsigned int itr=0;itr<sz;++itr)
	{
	  int mondo = mpctrigmap->get_2x2id(i4x4,itr);
	  mpc2x2ContentV1* m2x2 = v2x2.at(mondo);
	  if( m2x2 != 0)
	    {
	      m4x4->set_next_2x2(mondo);
	      m4x4->set_next_e( m2x2->get_esum());
	      m4x4->set_next_e2( m2x2->get_esum2());
	    }
	}
      //add the 4x4 to the node tree
      if(m4x4->get_n2x2() > 0)
	{
	  m4x4->calc_esum();
	  m4x4->calc_esum2();
	  v4x4.at(i4x4) = m4x4;
	  if(m4x4->get_esum() > 1)
	    mpc4x4->add4x4(*m4x4);
	}
      else
	{
	  delete m4x4;
	  m4x4 = 0;
	}
    }
  

  for(int i2x2=0;i2x2<144;++i2x2)
    {
      mpc2x2ContentV1* m2x2 = v2x2.at(i2x2);
      if(m2x2 == 0) continue;
      
      //otherwise we delete the 2x2 object
      delete m2x2;
      v2x2.at(i2x2) = 0;
      
    }


  for(int i4x4=0;i4x4<144;++i4x4)
    {
      mpc4x4ContentV1* m4x4 = v4x4.at(i4x4);
      if(m4x4 == 0) continue;
      
      //otherwise we delete the 4x4 object
      delete m4x4;
      v4x4.at(i4x4) = 0;
      
    }
  


  if(verbosity > 2) {
    PrintEvent(topNode);
    cout << "size 2x2: " << mpc2x2->size() << endl;
    cout << "size 4x4: " << mpc4x4->size() << endl;
  }
  return 0;
}

void MpcTrigReco::PrintEvent(PHCompositeNode* topNode) const
{
  mpc2x2Container* mpc2x2 = findNode::getClass<mpc2x2Container>(topNode,"mpc2x2Container");
  if (!mpc2x2)
    {
      cout << PHWHERE << "Unable to get mpc2x2Container, is Node missing?" << endl;
      return;
    }
  
  for(unsigned int i2x2=0;i2x2<mpc2x2->size();++i2x2)
    {
      mpc2x2Content* m2x2 = mpc2x2->get2x2(i2x2);
      m2x2->print(std::cout);
    }



  mpc4x4Container* mpc4x4 = findNode::getClass<mpc4x4Container>(topNode,"mpc4x4Container");
  if (!mpc4x4)
    {
      cout << PHWHERE << "Unable to get mpc4x4Container, is Node missing?" << endl;
      return;
    }
  
  for(unsigned int i4x4=0;i4x4<mpc4x4->size();++i4x4)
    {
      mpc4x4Content* m4x4 = mpc4x4->get4x4(i4x4);
      m4x4->print(std::cout);
    }


}



int MpcTrigReco::CreateNodeTree(PHCompositeNode *topNode)
{
  PHCompositeNode *dstNode;
  PHNodeIterator iter(topNode);
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }

  mpc2x2Container *mpc2x2ctr = new mpc2x2ContainerV1();
  TString nodename = "mpc2x2Container";
  PHObjectNode_t *Mpc2x2Node = new PHObjectNode_t(mpc2x2ctr, nodename.Data(), "PHObject");
  dstNode->addNode(Mpc2x2Node);


  mpc4x4Container *mpc4x4ctr = new mpc4x4ContainerV1();
  nodename = "mpc4x4Container";
  PHObjectNode_t *Mpc4x4Node = new PHObjectNode_t(mpc4x4ctr, nodename.Data(), "PHObject");
  dstNode->addNode(Mpc4x4Node);

  return 0;
}


