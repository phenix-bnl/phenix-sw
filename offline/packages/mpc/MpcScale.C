#include <iostream>
#include <fstream>

#include <MpcScale.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContainerV1.h>
#include <mpcTowerContent.h>
#include <getClass.h>
#include <MpcMap.h>
#include <Fun4AllServer.h>

#include <PHNodeIterator.h>
#include <PHIODataNode.h>
#include <PHCompositeNode.h>
#include <PHObject.h>

#include <iostream>

using namespace std;
using namespace findNode;


MpcScale::MpcScale(const std::string &name, float scaleS, float scaleN): SubsysReco(name) 
{

  fScale[0] = (scaleS > 0)? scaleS: 1.0;
  fScale[1] = (scaleN > 0)? scaleN: 1.0;

}

int MpcScale::SetScale(float scale, int arm) 
{
  if(arm < 0 || arm > 1) return 0;
  fScale[arm] = (scale > 0)? scale: 1.0;
  return 1;
}


MpcScale::~MpcScale()
{
}
 
int MpcScale::Init(PHCompositeNode *topNode)
{
  return 0;
}

int MpcScale::End(PHCompositeNode *topNode)
{
  return 0;
}

int MpcScale::InitRun(PHCompositeNode *topNode)
{
  //  mpcmap = MpcMap::instance();
  mpcmap = findNode::getClass<MpcMap>(topNode, "MpcMap");
  return 0;
}


//top Node is the real
int MpcScale::process_event(PHCompositeNode *topNode)
{
  mpcTowerContainer *mpctow = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");

  if (!mpctow)
    {
      cout << PHWHERE << "Unable to get mpcTowerContainer(s), is Node missing?" << endl;
      cout << "mpctow: " << mpctow << endl;
      return 0;
    }
  //*mpctow*= fScale;
  mpctow->scale(fScale[0],0);
  mpctow->scale(fScale[1],1);

  return 0;
}

void MpcScale::print(mpcTowerContainer *tow) {
   int ntow = tow->size();
   cout << "Size of tower container is: " << ntow << endl;
   cout << "Printing Energies\n";
   for (int itow=0;itow<ntow;itow++)
     {
       mpcTowerContent *tower = tow->getTower(itow);
       int tow_ch = tower->get_ch();               // tower channel (FEM numbering)
       int gridx = mpcmap->getGridX( tow_ch );
       
       if ( gridx == -1 )
	 {
	   cout << "ERROR, Non-existent channel " << tow_ch << endl;
	   continue;
	 }
       
       float e = tower->get_energy();
       cout << "tower: " << tow_ch << ", Energy: " << e << endl;
       
     }
   return;
}
 
