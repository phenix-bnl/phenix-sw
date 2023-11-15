#include <iostream>
#include <cstdlib>
#include "rootAnc.h"
#include "PISAEventHeader.h"
#include "EmcPISAHit.h"
#include "PadPISAHit.h"
#include "TofPISAHit.h"

void rootAncEmcPad(Int_t ancflag, PISAEventHeader *EventHeader)
{

  static int icall = 0;
  static TFile *AncEmcPadFile = 0;
  static TNtuple *AncEmcPadNtuple = 0;

  if(ancflag == 1) {
    //
    // Check that the file has been opened
    //
    if(icall == 0 || AncEmcPadFile == 0){
      std::cerr << "\n rootAncEmcPad <E> bad call with ancflag = 1" << std::endl;
      std::exit(1);
    }  // safety check
    AncEmcPadFile->Write();
    AncEmcPadFile->Close();
    return;
  }  // check for close output file flag

  const int NTPL_PARAM = 28;
  float evt_ntpl[NTPL_PARAM];
  if(icall == 0){
    //
    // NTUPLE files
    //
    AncEmcPadFile = new TFile("ancEmcPad.root", "recreate", "EMC/PAD PISA NTUPLE");
    AncEmcPadNtuple = new TNtuple("AncEmcPad", "EmcPad Ancestors",
                               "TRACK:NFILE:PTOT:PTHETA:PPHI:"//
			       "R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"//
			       "ITPARENT:IDPARENT:IDPART:IHIT:NHIT:"//
			       "ITORIGIN:IDORIGIN:IPC123:THETAPC3:PHIPC3:"//,
                               "WALL:TYPE:NUMED:INDEX1:INDEX2:TOFHIT:"//,
			       "Z0_EVENT:B_IMPACT:EVENT");


  }  // initialization
  icall++;

  int emcRows;

  int iPC;
  float xglobal, yglobal, zglobal;
  float theta;
  float phi;
  const float DEGRAD = 57.295779513;

  int iRow;

  const int maxpart = 10000;
  int pctrack[maxpart][3];
  int tofTrack[maxpart];
  int kpart;
  int kpart1;
  int kpart2;
  int kpart3;
  int ipc123; /* IPC123 = JKL: L=0/1 for PC1; K = 0/1 for PC2; J = 0/1 for PC3 */

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
  int kstore;

  int lastTrack = -1;

  Int_t pcRows = PadPISAHit::GetPadCount();
  Int_t pcRow[3];
  kpart1 = pcRow[0]  = PadPISAHit::GetPC1Count();
  kpart2 = pcRow[1]  = PadPISAHit::GetPC2Count();
  kpart3 = pcRow[2]  = PadPISAHit::GetPC3Count();

  PadPISAHit *pc1ghit = PadPISAHit::GetPC1HitEvt();
  PadPISAHit *pc2ghit = PadPISAHit::GetPC2HitEvt();
  PadPISAHit *pc3ghit = PadPISAHit::GetPC3HitEvt();
  PadPISAHit *pcghit = 0;

  int tofRows = TofPISAHit::GetTofCount();
  TofPISAHit *tofghit = TofPISAHit::GetTofHitEvt();
  if(tofRows > maxpart) {
    std::cout << "\n Too many tofRows " << std::endl;
    tofRows = maxpart;
  }
  for(int iTof=0; iTof<tofRows; iTof++) {
    tofTrack[iTof] = tofghit[iTof].GetMctrack();
  }

  for(iPC=0; iPC<3; iPC++){
    if(pcRow[iPC] > maxpart) {
      std::cerr << "\n rootAncEmcPad <E> for PC" << iPC+1;
      std::cerr << " there are too many hits " << pcRow[iPC] << std::endl;
      std::exit(1);
    }  // safety check
  }  // loop over 3 PC values


  for(iPC = 1; iPC < 4 ; iPC++){
    switch (iPC) {
    case 1: {pcghit = pc1ghit; pcRows = kpart1; break;}
    case 2: {pcghit = pc2ghit; pcRows = kpart2; break;}
    case 3: {pcghit = pc3ghit; pcRows = kpart3; break;}
    }
    for (iRow = 0; iRow < pcRows ; iRow++) {
      true_track = pcghit[iRow].GetMctrack();
      pctrack[iRow][iPC-1] = true_track;
    }
  }

  evt_ntpl[NTPL_PARAM - 1] = icall;
  nstore = 0;
  emcRows = EmcPISAHit::GetEmcCount();
  EmcPISAHit *emcghit = EmcPISAHit::GetEmcHitEvt();
  for (iRow = 0; iRow < emcRows ; iRow++) {
    true_track = emcghit[iRow].GetMctrack();

    if(true_track == lastTrack)
      continue;  // don't store same incident particle successively

    lastTrack = true_track;
    if(true_track < 1)
      continue;  // this is an error condition

    kstore = 0;
    for (istore = 0; istore < nstore; istore++){
      if(true_track == trueStore[istore]){
	kstore = 1;
	break;
      } // check if track was previously stored
    } // loop over istore

    if(kstore == 1)
      continue;   // this track was previously stored

    trueStore[nstore++] = true_track;  // store track not already stored
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
    evt_ntpl[12] = iRow;
    evt_ntpl[13] = emcRows;
    evt_ntpl[14] = itorigin;
    evt_ntpl[15] = idorigin;

    ipc123 = 0;
    theta = -999.0;
    phi = -999.0;

    int tofHit = 0;
    if(idpart != 1){
      for(kpart=0; kpart<tofRows; kpart++) {
	if(true_track == tofTrack[kpart]) {
	  tofHit = 1;
	  break;
	} // check if track number is in TOF hits
      } // loop over TOF hits

      for (kpart = 0; kpart < kpart1; kpart++){
        if(true_track == pctrack[kpart][0]){
	   ipc123 = 1;
	   break;
         }
      }
      for (kpart = 0; kpart < kpart2; kpart++){
        if(true_track == pctrack[kpart][1]){
	   ipc123 = ipc123 + 10;
	   break;
         }
      }
      for (kpart = 0; kpart < kpart3; kpart++){
        if(true_track == pctrack[kpart][2]){
	 ipc123 = ipc123 + 100;

         xglobal = pc3ghit[kpart].GetXing();
         yglobal = pc3ghit[kpart].GetYing();
         zglobal = pc3ghit[kpart].GetZing();
         theta = DEGRAD*acos(zglobal/sqrt(xglobal*xglobal +
                             yglobal*yglobal + zglobal*zglobal));
         phi = DEGRAD*atan2(yglobal, xglobal);
         if(phi < -90.0)
           phi = 360.0 + phi;

	 break;
        }
      }
    }

    evt_ntpl[16] = ipc123;
    evt_ntpl[17] = theta;
    evt_ntpl[18] = phi;
    evt_ntpl[19] = emcghit[iRow].GetWall();
    evt_ntpl[20] = emcghit[iRow].GetItype();
    evt_ntpl[21] = emcghit[iRow].GetNumed();
    evt_ntpl[22] = emcghit[iRow].GetIndex1();
    evt_ntpl[23] = emcghit[iRow].GetIndex2();

    evt_ntpl[24] = tofHit;

    AncEmcPadNtuple->Fill(evt_ntpl);


  } // loop over EMCal rows

}
