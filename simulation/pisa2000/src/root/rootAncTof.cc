#include <iostream>
#include <cstdlib>
#include "rootAnc.h"
#include "PISAEventHeader.h"
#include "TofPISAHit.h"

void rootAncTof(Int_t ancflag, PISAEventHeader *EventHeader)
{

  static int icall = 0;
  static TFile *AncTofFile = 0;
  static TNtuple *AncTofNtuple = 0;

  if(ancflag == 1) {
    //
    // Check that the file has been opened
    //
    if(icall == 0 || AncTofFile == 0){
      std::cerr << "\n rootAncTof <E> bad call with ancflag = 1" << std::endl;
      std::exit(1);
    }  // safety check
    AncTofFile->Write();
    AncTofFile->Close();
    return;
  }  // check for close output file flag

  const int NTPL_PARAM = 29;
  float evt_ntpl[NTPL_PARAM];
  if(icall == 0){
    //
    // NTUPLE files
    //
    AncTofFile = new TFile("anctof.root", "recreate", "TOF PISA NTUPLE");
    AncTofNtuple = new TNtuple("AncTof", "TOF Ancestors",
                               "TRACK:NFILE:PTOT:PTHETA:PPHI:"//
			       "R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"//
			       "ITPARENT:IDPARENT:IDPART:IHIT:NHIT:"//
			       "ITORIGIN:IDORIGIN:HITCOUNT:THETA:PHI:"//,
			       "MCTOF:SUBVOL:PANEL:COLUMN:PSLAT:SLATSEQ:MCTOFDIS:"//
			       "Z0_EVENT:B_IMPACT:EVENT");


  }  // initialization
  icall++;

  int tofRows(0);
  int iRow(0);

  int true_track(0);
  int nfile(0);
  int error(0);
  float ptot(0);
  float ptheta(0);
  float pphi(0);
  float r_vertex(0);
  float z_vertex(0);
  float theta_vertex(0);
  float phi_vertex(0);
  int itparent(0);
  int idparent(0);
  int idpart(0);

  int itorigin(0);
  int idorigin(0);

  float theta(0);
  float phi(0);
  float xglobal, yglobal, zglobal, mctofdis;
  const float DEGRAD = 57.295779513;

  int nstore(0);
  int istore(0);
  const int MAXSTORE = 100000;
  int trueStore[MAXSTORE];
  int trueTimes[MAXSTORE];
  int kstore;

  evt_ntpl[NTPL_PARAM - 1] = icall;
  nstore = 0;
  tofRows = TofPISAHit::GetTofCount();
  TofPISAHit *tofghit = TofPISAHit::GetTofHitEvt();

  for (iRow = 0; iRow < tofRows ; iRow++) {

    true_track = tofghit[iRow].GetMctrack();
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
      trueTimes[istore]++;             // increment hit count of previous store
   }

   if(kstore == 0){
     trueStore[nstore] = true_track;  // store track not already stored
     trueTimes[nstore++] = 1;         // initialize hit count
   }
  } // initial loop over iRow

  /*
  for (istore = 0; istore < nstore; istore++) {
    true_track = trueStore[istore];
  */

  //
  // Change to storing all hits
  //
  for (iRow = 0; iRow < tofRows ; iRow++) {

   true_track = tofghit[iRow].GetMctrack();
    if(true_track < 1)
      continue ;  // this is an error condition

    for(Int_t jstore=0; jstore < nstore; jstore++) {
      if(true_track==trueStore[jstore]) {
	istore = jstore;
	break;
      }
    }

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

    xglobal = tofghit[iRow].GetXm();
    yglobal = tofghit[iRow].GetYm();
    zglobal = tofghit[iRow].GetZm();
    mctofdis = sqrt(xglobal*xglobal + yglobal*yglobal + zglobal*zglobal);
    theta = DEGRAD*acos(zglobal/mctofdis);
    phi = DEGRAD*atan2(yglobal, xglobal);
    if( phi < -90.0 )
      phi = 360.0 + phi;
    evt_ntpl[17] = theta;
    evt_ntpl[18] = phi;

    evt_ntpl[19] = tofghit[iRow].GetTof();
    evt_ntpl[20] = tofghit[iRow].GetSubvol();
    evt_ntpl[21] = tofghit[iRow].GetPanel();
    evt_ntpl[22] = tofghit[iRow].GetColumn();
    evt_ntpl[23] = tofghit[iRow].GetPslat();
    evt_ntpl[24] = tofghit[iRow].GetSlat_seq();
    evt_ntpl[25] = mctofdis;

    AncTofNtuple->Fill(evt_ntpl);

  }  // loop over istore

  return;
}


