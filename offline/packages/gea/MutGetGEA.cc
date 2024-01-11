// $Id: MutGetGEA.cc,v 1.7 2007/11/13 20:25:39 hpereira Exp $
/*!
  \file MutGetGEA.cc
  \brief fill staff tables from PISAHit object
  \author  Hugo Pereira
  \version $Revision: 1.7 $
  \date $Date: 2007/11/13 20:25:39 $
*/


#include <MutPISAHit.h>
#include "mumhitsWrapper.h"
#include <getClass.h>

#include <PHPoint.h>

using namespace std;

//_______________________________________________________________
long MutGetGEA(PHCompositeNode* topNode)
{
  
  MutPISAHit *event = MutPISAHit::GetMutHitEvt();
  Int_t rows = MutPISAHit::GetMutCount();  
  
  mumhitsWrapper* w = findNode::getClass<mumhitsWrapper>(topNode,"mumhits");
  if( !w )
  {
    cout << "MutGetGEA - could not find node mumhits" << endl;
    return -1;
  }
    
  w->SetMaxRowCount(rows);
  MUMHITS_ST* mumhits = w->TableData();
  
  for(int i=0; i<rows; i++) 
  {
    
    mumhits[i].t    = event[i].GetT();
    mumhits[i].e    = event[i].GetE();
    mumhits[i].x[0] = event[i].GetX();
    mumhits[i].x[1] = event[i].GetY();
    mumhits[i].x[2] = event[i].GetZ();
    mumhits[i].p[0] = event[i].GetPx();
    mumhits[i].p[1] = event[i].GetPy();
    mumhits[i].p[2] = event[i].GetPz();
    mumhits[i].track = event[i].GetMctrack();
    mumhits[i].pid   = event[i].GetPid();
    mumhits[i].plane = event[i].GetPlane();
    
  }
  
  w->SetRowCount(rows);

  return 0;

}
