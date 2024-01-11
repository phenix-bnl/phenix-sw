#include <MpcMakeDeadReco.h>
#include <recoConsts.h>
#include <getClass.h>
#include <Fun4AllReturnCodes.h>
#include <MpcMap.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>

#include <iostream>
#include <fstream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;

MpcMakeDeadReco::MpcMakeDeadReco(const std::string &name): SubsysReco(name)
{
  mpcmap = 0;
}

int MpcMakeDeadReco::InitRun(PHCompositeNode *topNode)
{
  // Reset deadmap
/*
  for (int ich=0; ich<576; ich++)
    {
      _deadmap[ich] = 0;
    }
*/
  for (int iarm=0; iarm<2; iarm++)
    {
      for (int ix=0; ix<18; ix++)
        {
          for (int iy=0; iy<18; iy++)
            {
              _deadmap[iarm][ix][iy] = 0;
            }
        }
    }

  // mpcmap doesn't get updated here?
  //mpcmap = MpcMap::instance();

  ifstream infile( _deadmapfname.c_str() );
  int arm, xpos, ypos;
  while ( infile >> arm >> xpos >> ypos )
    {
      //cout << "MAKEDEADRECO: " << arm << "\t" << xpos << "\t" << ypos << endl;

/*
      int feech576 = mpcmap->getFeeCh(xpos,ypos,arm);
      if ( feech576<0 || feech576>575 )
        {
          std::cout << "ERROR " << feech576 << "\t" << xpos << "\t" << ypos << endl;
          continue;
        }
*/

      //_deadmap[feech576] = 1;
      _deadmap[arm][xpos][ypos] = 1;
      //std::cout << "WARNMAP " << feech576 << "\t" << xpos << "\t" << ypos << endl;
      std::cout << "WARNMAP " << arm << "\t" << xpos << "\t" << ypos << endl;
    }
  infile.close();

  return 0;
}

int MpcMakeDeadReco::process_event(PHCompositeNode *topNode)
{
  mpcTowerContainer *mpctower = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  if ( mpctower==0 )
    {
      cout << "MpcMakeDead: mpcTowerContainer NOT found, is Node missing?" << endl;
      return EVENT_OK;
    }

  mpcmap = MpcMap::instance();

  int ntower = mpctower->size();
  for (int itow=0; itow<ntower; itow++)
    {
      mpcTowerContent *tower = mpctower->getTower( itow );
      int fee576ch = tower->get_ch();
      int arm = mpcmap->getArm(fee576ch);
      int xpos = mpcmap->getGridX(fee576ch);
      int ypos = mpcmap->getGridY(fee576ch);
      //if ( _deadmap[fee576ch] )
      if ( _deadmap[arm][xpos][ypos]==1 || xpos<0 || ypos<0 )
        {
          tower->set_energy( 0. );
        }
    }

  return EVENT_OK;
}

