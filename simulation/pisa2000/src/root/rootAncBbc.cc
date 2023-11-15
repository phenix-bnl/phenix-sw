#include <iostream>
#include <cstdlib>

#include "PISARun.h"
#include "PISAEventHeader.h"
#include "BbcPISAHit.h"

//___________________________________________________________________
void PISARun::AncBbc( const int& ancflag, PISAEvent::pointer pisa_event)
{
  static int icall = 0;
  static TFile *AncBbcFile = 0;
  static TNtuple *AncBbcNtuple = 0;

  PISAEventHeader *EventHeader = pisa_event->GetHeader();

  if(ancflag == 1) 
  {
    //
    // Check that the file has been opened
    //
    if(icall == 0 || AncBbcFile == 0){
      std::cerr << "\n rootAncBbc <E> bad call with ancflag = 1" << std::endl;
      std::exit(1);
    }  // safety check
    AncBbcFile->Write();
    AncBbcFile->Close();
    return;
  }  // check for close output file flag

  const int NTPL_PARAM = 25;
  float evt_ntpl[NTPL_PARAM];
  if(icall == 0){
    //
    // NTUPLE files
    //
    AncBbcFile = new TFile("ancbbc.root", "recreate", "BBC PISA NTUPLE");
    AncBbcNtuple = new TNtuple("AncBbc", "BBC Ancestors",
			       "TRACK:NFILE:PTOT:PTHETA:PPHI:"//
			       "R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"//
			       "ITPARENT:IDPARENT:IDPART:IHIT:NHIT:"// 
			       "ITORIGIN:IDORIGIN:TOF:PMT:"// 
			       "PTOT_PRI:PTHE_PRI:PPHI_PRI:PTOT_BBC:"//
			       "Z0_EVENT:B_IMPACT:EVENT");

  }  // initialization
  icall++;

  int bbcRows;
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

  int itorigin;
  int idorigin;
  float ptot_pri;
  float pthet_pri;
  float pphi_pri;

  int nstore;
  int istore;
  const int MAXSTORE = 100000;
  int trueStore[MAXSTORE];
  int kstore;

  int lastTrack = -1;

  evt_ntpl[NTPL_PARAM - 1] = icall;
  nstore = 0;
  bbcRows = BbcPISAHit::GetBbcCount();
  BbcPISAHit *bbcghit = BbcPISAHit::GetBbcHitEvt();
  for (iRow = 0; iRow < bbcRows ; iRow++) {

    true_track = bbcghit[iRow].GetMctrack();
    if(true_track == lastTrack)
      continue;  // don't store same particle successively

    lastTrack = true_track;

    if(true_track < 1)
      continue;  // this is an error condition

    kstore = 0;
    for (istore = 0; istore < nstore; istore++){
      if(true_track == trueStore[istore]){
	kstore = 1;
	break;
      } // check if track was previously stored
    }
    if(kstore == 1)
      continue;   // this track was previously stored

    trueStore[nstore++] = true_track;  // store track not already stored

    dio_ptrkorigin(&true_track, &nfile, &error, &ptot_pri, &pthet_pri, &pphi_pri,
		  &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		  &itorigin, &idorigin, &idpart);  
/*
*  NOTE: idpart is primary particle id (=idorigin), not the input particle id.
*/
      
    dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		  &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		  &itparent, &idparent, &idpart);  

    evt_ntpl[NTPL_PARAM - 2] = EventHeader->GetImpactParameter();
    evt_ntpl[NTPL_PARAM - 3] = EventHeader->GetZvertex();
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
    evt_ntpl[13] = bbcRows;
    evt_ntpl[14] = itorigin;
    evt_ntpl[15] = idorigin;
    evt_ntpl[16] = bbcghit[iRow].GetTof();
    evt_ntpl[17] = bbcghit[iRow].GetPmt();
    evt_ntpl[18] = ptot_pri;
    evt_ntpl[19] = pthet_pri;
    evt_ntpl[20] = pphi_pri;
    evt_ntpl[21] = sqrt(bbcghit[iRow].GetPx()*bbcghit[iRow].GetPx() +
			bbcghit[iRow].GetPy()*bbcghit[iRow].GetPy() +
			bbcghit[iRow].GetPz()*bbcghit[iRow].GetPz());

    AncBbcNtuple->Fill(evt_ntpl);

  }  // loop over Bbc rows

}


