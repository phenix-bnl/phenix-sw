#include <iostream>
#include <cstdlib>
#include "rootAnc.h"
#include "PISAEventHeader.h"
#include "CrkPISAHit.h"

void rootAncCrk(Int_t ancflag, PISAEventHeader *EventHeader)
{

  static int icall = 0;
  static TFile *AncCrkFile = 0;
  static TNtuple *AncCrkNtuple = 0;

  if(ancflag == 1) {
    //
    // Check that the file has been opened
    //
    if(icall == 0 || AncCrkFile == 0){
      std::cerr << "\n rootAncCrk <E> bad call with ancflag = 1" << std::endl;
      std::exit(1);
    }  // safety check
    AncCrkFile->Write();
    AncCrkFile->Close();
    return;
  }  // check for close output file flag

  if(icall == 0){
    //
    // NTUPLE files
    //
    AncCrkFile = new TFile("anccrk.root", "recreate", "DC PISA NTUPLE");
    AncCrkNtuple = new TNtuple("AncCrk", "RICH Ancestors",
			       "TRACK:NFILE:PTOT:PTHETA:PPHI:"//
			       "R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"//
			       "ITPARENT:IDPARENT:IDPART:IHIT:NHIT:"// 
			       "ITORIGIN:IDORIGIN:HITCOUNT:"//
			       "Z0_EVENT:B_IMPACT:EVENT");
  }  // initialization
  icall++;

  int crkRows;
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

  int nstore;
  int istore;
  const int MAXSTORE = 100000;
  int trueStore[MAXSTORE];
  int trueTimes[MAXSTORE];
  int kstore;

  const int NTPL_PARAM = 20;
  float evt_ntpl[NTPL_PARAM];
  nstore = 0;

  crkRows = CrkPISAHit::GetCrkCount();
  CrkPISAHit *crkghit = CrkPISAHit::GetCrkHitEvt();
  for (iRow = 0; iRow < crkRows ; iRow++) {
    true_track = crkghit[iRow].GetMctrack();  // RICH stores parent of Cerenkov photon here
    if(true_track < 1)
      continue ;  // this is an error condition

    kstore = 0;
    for (istore = 0; istore < nstore; istore++){
      if(true_track == trueStore[istore]){
	kstore = 1;
	break;
      } // check if track was previously stored
    }
    if(kstore == 1){
      trueTimes[istore]++;
    }
    if(kstore == 0){
      trueStore[nstore] = true_track;  // store track not already stored
      trueTimes[nstore++] = 1;
    }
  }
  for (istore = 0; istore < nstore; istore++) {
    true_track = trueStore[istore];

    dio_ptrkorigin(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
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
    evt_ntpl[12] = istore;
    evt_ntpl[13] = nstore;
    evt_ntpl[14] = itorigin;
    evt_ntpl[15] = idorigin;
    evt_ntpl[16] = trueTimes[istore];

    AncCrkNtuple->Fill(evt_ntpl);

  }  // loop over istore

  return ;

}
