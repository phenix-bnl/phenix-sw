// $Id: rootAncPri.cc,v 1.9 2013/01/22 19:49:54 pinkenbu Exp $

/*!
  \file   rootAncPri.cc
  \brief  PISA primary information evaluation ntuple
  \author Charlie Maguire, Hugo Pereira
  \version $Revision: 1.9 $
  \date    $Date: 2013/01/22 19:49:54 $
*/

#include <iostream>
#include <cstdlib>
#include <boost/array.hpp>

#include "rootAnc.h"
#include "PISAEvent.h"
#include "PISARun.h"
#include "PriPISAHit.h"
#include "geantMass.h"
#include "pisaMass.h"

using namespace std;

//____________________________________________________________________
float SQUARE( float x ) { return x*x; }
static const float DEG_TO_RAD = M_PI/180.0;

//____________________________________________________________________
void PISARun::AncPri( const int& ancflag, PISAEvent::pointer pisa_event)
{

  static int icall = 0;
  static TFile *AncPriFile = 0;
  static TNtuple *AncPriNtuple = 0;
  PISAEventHeader *EventHeader = pisa_event->GetHeader();

  if(ancflag == 1) 
  {

    // Check that the file has been opened
    if(icall == 0 || AncPriFile == 0)
    {
      cerr << "PISARun::AncPri - bad call with ancflag = 1" << endl;
      exit(1);
    }
    AncPriFile->Write();
    AncPriFile->Close();
    return;
    
  }  

  const int NTPL_PARAM = 15;
  boost::array< float, NTPL_PARAM > evt_ntpl;
  float etotal = -999.;
  if(icall == 0)
  {

    // NTUPLE files
    AncPriFile = new TFile("ancpri.root", "recreate", "PRIMARY PISA NTUPLE");
    AncPriNtuple = new TNtuple(
      "AncPri", "Primary Particle Parameters",
      "TRACK:EVTRACK:NFILE:IDPART:PX:PY:PZ:"
      "ETOTAL:RAPIDITY:PXREACPL:PYREACPL:"
      "ZVERTEX:REACPLANE:PISAEVENT:EVENT");

  }
  
  icall++;
  evt_ntpl[9] = EventHeader->GetZvertex();

  evt_ntpl[NTPL_PARAM - 1] = icall;
  evt_ntpl[NTPL_PARAM - 2] = EventHeader->GetEvent();
  evt_ntpl[NTPL_PARAM - 3] = EventHeader->GetReactionPlaneAngle();
  float reacPlaneRadian = (EventHeader->GetReactionPlaneAngle())*DEG_TO_RAD;
  float cosRCP = cos(reacPlaneRadian);
  float sinRCP = sin(reacPlaneRadian);
  evt_ntpl[NTPL_PARAM - 4] = EventHeader->GetZvertex();

  int priRows = PriPISAHit::GetPriCount();
  PriPISAHit *prihit = PriPISAHit::GetPriHitEvt();

  for(int iRow=0; iRow<priRows; iRow++)
  {
    evt_ntpl[0] = prihit->GetTrue_track();
    evt_ntpl[1] = prihit->GetEvttrack();
    evt_ntpl[2] = prihit->GetNfile();
    int idpart =  prihit->GetIdpart();
    evt_ntpl[3] = idpart;
    evt_ntpl[4] = prihit->GetPx();
    evt_ntpl[5] = prihit->GetPy();
    float pxLab = evt_ntpl[4];
    float pyLab = evt_ntpl[5];
    evt_ntpl[6] = prihit->GetPz();
    evt_ntpl[7] = -999.0;
    evt_ntpl[8] = -999.0;

    if(idpart>0 && idpart<50) 
    {
      
      etotal = sqrt(
        SQUARE( evt_ntpl[4] ) + 
        SQUARE( evt_ntpl[5] ) +
        SQUARE( evt_ntpl[6] ) +
        SQUARE( geantMass[idpart-1] ) );
      
    }

    if(idpart==51 || idpart==52) 
    {

      // Non decaying Kaons
      etotal = sqrt(
        SQUARE( evt_ntpl[4] ) + 
        SQUARE( evt_ntpl[5] ) +
        SQUARE( evt_ntpl[6] ) +
        SQUARE( geantMass[10] ) );

    }
    
    if(idpart>=PISA_PART_MIN && idpart<=PISA_PART_MAX) {
      etotal = sqrt(
        SQUARE( evt_ntpl[4] ) + 
        SQUARE( evt_ntpl[5] ) +
        SQUARE( evt_ntpl[6] ) +
        SQUARE( pisaMass[idpart-PISA_PART_MIN] ) );
    }

    evt_ntpl[7] = etotal;
    evt_ntpl[8] = 0.5*log((etotal + evt_ntpl[6])/(etotal - evt_ntpl[6]));

    float pxReacPlane = pxLab*cosRCP + pyLab*sinRCP;
    float pyReacPlane = -pxLab*sinRCP + pyLab*cosRCP;
    
    evt_ntpl[9]  = pxReacPlane;
    evt_ntpl[10] = pyReacPlane;

    AncPriNtuple->Fill( &evt_ntpl[0] );
    prihit++;

  } // loop over iRow

  return;
}
