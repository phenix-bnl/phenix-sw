// $Id: DchGetGEA.cc,v 1.11 2007/11/13 20:25:39 hpereira Exp $
/*!
  \file DchGetGEA.cc
  \brief fill staff tables from PISAHit object
  \author  Hugo Pereira
  \version $Revision: 1.11 $
  \date $Date: 2007/11/13 20:25:39 $
*/

#include "dcghitWrapper.h"
#include <DchPISAHit.h>
#include <getClass.h>

using namespace std;

//______________________________________________________________________
long DchGetGEA(PHCompositeNode* topNode)
{
  
  // load wrapper
  dcghitWrapper* w = findNode::getClass<dcghitWrapper>(topNode, "dcghit");
  if( !w )
  {
    cout << "DchGetGEA - could not find node dcghit" << endl;
    return -1;
  }
  
  // retrieve Bbc hits
  DchPISAHit *event = DchPISAHit::GetDchHitEvt();
  Int_t rows = DchPISAHit::GetDchCount();  
  w->SetMaxRowCount(rows);
  
  for( int i = 0; i < rows; i++ )
  {
    w->set_tof( i, event[i].GetTof());
    w->set_id( i, i);
    w->set_pathLength( i,  event[i].GetPathLength());
    w->set_plane( i,  event[i].GetPlane());
    w->set_cell( i,  event[i].GetCell());
    w->set_arm( i,  event[i].GetIarm());
    w->set_mctrack( i, event[i].GetMctrack() );
    for (int j = 0; j < 3;j++)
    {
      w->set_xyzinloc(j,  i,  event[i].get_xyzinloc(j));
      w->set_xyzoutloc(j,  i,  event[i].get_xyzoutloc(j));
      w->set_xyzinglo(j,  i,  event[i].get_xyzinglo(j));
    }
  }
  
  w->SetRowCount( rows );
  
  return 0;
  
}
