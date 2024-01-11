// $Id: PadGetGEA.cc,v 1.9 2007/11/13 20:25:40 hpereira Exp $
/*!
  \file PadGetGEA.cc
  \brief fill staff tables from PISAHit object
  \author  Hugo Pereira
  \version $Revision: 1.9 $
  \date $Date: 2007/11/13 20:25:40 $
*/

#include "pcghitWrapper.h"
#include <PadPISAHit.h>
#include <getClass.h>
#include <sstream>

using namespace std;

//______________________________________________________________________
long PadGetGEA(PHCompositeNode* topNode)
{
  
  PadPISAHit* pad_pisa_hit(0);
  int rows(0);
  
  for( int i_pc = 0; i_pc < 3; i_pc ++ )
  { 
    
    // load wrappers
    // create node name
    ostringstream nodename;
    nodename << "pc" << i_pc+1 << "ghit";
    pcghitWrapper* w = findNode::getClass<pcghitWrapper>(topNode, nodename.str().c_str());
    if( !w )
    {
      cout << "PadGetGEA - could not find node " << nodename .str() << endl;
      return -1;
    } 
    
    // retrieve hits and counts
    switch( i_pc )
    {
      case 0:
      {
        pad_pisa_hit = PadPISAHit::GetPC1HitEvt();
        rows = PadPISAHit::GetPC1Count();
        break;
      }
      
      case 1:
      {
        pad_pisa_hit = PadPISAHit::GetPC2HitEvt();
        rows = PadPISAHit::GetPC2Count();
        break;
      }
     
      case 2:
      {
        pad_pisa_hit = PadPISAHit::GetPC3HitEvt();
        rows = PadPISAHit::GetPC3Count();
        break;
      }
    
      default: return -1;
    
    }
        
    // fill staff table
    w->SetMaxRowCount(rows);
    for( int i = 0; i < rows; i++ )
    {
      w->set_mctrack(i, pad_pisa_hit[i].GetMctrack() );
      w->set_dedx(i, pad_pisa_hit[i].GetDedx());
      w->set_tof(i, pad_pisa_hit[i].GetTof());
      w->set_pathLength(i, pad_pisa_hit[i].GetPathLength());
      w->set_sector(i, pad_pisa_hit[i].GetSector());
      w->set_arm(i, pad_pisa_hit[i].GetIarm());
      w->set_id(i, i);
      for (int j = 0; j < 3;j++)
      {
        w->set_xyzinloc(j, i, pad_pisa_hit[i].get_xyzinloc(j));
        w->set_xyzoutloc(j, i, pad_pisa_hit[i].get_xyzoutloc(j));
        w->set_xyzinglo(j, i, pad_pisa_hit[i].get_xyzinglo(j));
      }
      
    }
    w->SetRowCount(rows);
  }
  
  return 0;
  
}
