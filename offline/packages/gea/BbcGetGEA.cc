// $Id: BbcGetGEA.cc,v 1.8 2007/11/13 20:25:39 hpereira Exp $
/*!
  \file BbcGetGEA.cc
  \brief fill staff tables from PISAHit object
  \author  Hugo Pereira
  \version $Revision: 1.8 $
  \date $Date: 2007/11/13 20:25:39 $
*/

#include "bbcghitWrapper.h"
#include <BbcPISAHit.h>
#include <getClass.h>

using namespace std;

//______________________________________________________________________
long BbcGetGEA(PHCompositeNode* topNode)
{
  
  // load wrapper
  bbcghitWrapper* w = findNode::getClass<bbcghitWrapper>(topNode, "bbcghit");
  if( !w )
  {
    cout << "BbcGetGEA - could not find node bbcghit" << endl;
    return -1;
  }
  
  // retrieve Bbc hits
  BbcPISAHit *event = BbcPISAHit::GetBbcHitEvt();
  Int_t rows = BbcPISAHit::GetBbcCount();  
  w->SetMaxRowCount(rows);
  
  for( int i = 0; i < rows; i++ )
  {
    w->set_del(i, event[i].GetDel());
    w->set_pmt(i, event[i].GetPmt());
    w->set_len(i, event[i].GetLen());
    w->set_tof(i, event[i].GetTof());
    w->set_pid(i, event[i].GetPid());
    w->set_mctrack( i, event[i].GetMctrack() );
    
    for (int j = 0; j < 3;j++)
    {
      w->set_pos(j, i, event[i].get_pos(j));
      w->set_mom(j, i, event[i].get_mom(j));
    }
  }
  
  w->SetRowCount( rows );
  
  return 0;

}
