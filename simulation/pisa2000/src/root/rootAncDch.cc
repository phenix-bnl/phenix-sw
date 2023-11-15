#include <iostream>
#include <cstdlib>
#include "PISARun.h"
#include "PISAEventHeader.h"
#include "DchPISAHit.h"

using namespace std;

//_______________________________________________________
void PISARun::AncDch( const int& ancflag, PISAEvent::pointer pisa_event)
{
  // Event header information not yet used
  static int icall = 0;
  static TFile *AncDchFile = 0;
  static TNtuple *AncDchNtuple = 0;

  if(ancflag == 1) {

    // Check that the file has been opened
    if(icall == 0 || AncDchFile == 0){
      cout << "PISARun::AncDch - bad call with ancflag = 1" << endl;
      exit(1);
    }  
    AncDchFile->Write();
    AncDchFile->Close();
    return;
  }

  const int NTPL_PARAM = 23;
  float evt_ntpl[NTPL_PARAM];
  if(icall == 0){

    // NTUPLE files
    AncDchFile = new TFile("ancdch.root", "recreate", "DC PISA NTUPLE");
    AncDchNtuple = new TNtuple(
      "AncDch", "DC Ancestors",
      "TRACK:NFILE:PTOT:PTHETA:PPHI:"
      "R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"
      "ITPARENT:IDPARENT:IDPART:IHIT:NHIT:"
      "PLANE:XINGLOB:YINGLOB:ZINGLOB:TOF:" 
      "PATHLENG:IARM:EVENT_Z0:EVENT" );
    
  } 

  icall++;
  evt_ntpl[NTPL_PARAM-1] = icall;

  // retrieve pisa event header
  PISAEventHeader *EventHeader = pisa_event->GetHeader();
  evt_ntpl[NTPL_PARAM-2] = EventHeader->GetZvertex();

  int dcRows;
  int iRow;

  int true_track;
  int nfile;
  int error;
  float ptot;
  float ptheta;
  float pphi;
  float r_vertex;
  float z_vertex;
  float theta_vertex;
  float phi_vertex;
  int itparent;
  int idparent;
  int idpart;

  dcRows = DchPISAHit::GetDchCount();
  DchPISAHit *dcghit = DchPISAHit::GetDchHitEvt();
  
  for (iRow = 0; iRow < dcRows ; iRow++) 
  {
  
    true_track = dcghit[iRow].GetMctrack();
    
    dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
      &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
      &itparent, &idparent, &idpart);  
    
    evt_ntpl[0] = float(true_track);
    evt_ntpl[1] = float(nfile);
    evt_ntpl[2] = ptot;
    evt_ntpl[3] = ptheta;
    evt_ntpl[4] = pphi;
    evt_ntpl[5] = r_vertex;
    evt_ntpl[6] = z_vertex;
    evt_ntpl[7] = theta_vertex;
    evt_ntpl[8] = phi_vertex;
    evt_ntpl[9] = float(itparent);
    evt_ntpl[10] = float(idparent);
    evt_ntpl[11] = float(idpart);
    evt_ntpl[12] = iRow;
    evt_ntpl[13] = dcRows;
    evt_ntpl[14] = float(dcghit[iRow].GetPlane());
    evt_ntpl[15] = dcghit[iRow].GetXing();
    evt_ntpl[16] = dcghit[iRow].GetYing();
    evt_ntpl[17] = dcghit[iRow].GetZing();
    evt_ntpl[18] = 1000.*(dcghit[iRow].GetTof());   // put in ns
    evt_ntpl[19] = dcghit[iRow].GetPathLength();
    evt_ntpl[20] = dcghit[iRow].GetIarm();

    AncDchNtuple->Fill(evt_ntpl);

  } // loop over Dch hits

  return;
}




