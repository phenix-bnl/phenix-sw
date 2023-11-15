#include <iostream>
#include <cstdlib>
#include "rootAnc.h"
#include "PISAEventHeader.h"
#include "TecPISAHit.h"

void rootAncTec(Int_t ancflag, PISAEventHeader *EventHeader)
{

  static int icall = 0;
  static TFile *AncTecFile = 0;
  static TNtuple *AncTecNtuple = 0;

  if(ancflag == 1) {
    //
    // Check that the file has been opened
    //
    if(icall == 0 || AncTecFile == 0){
      std::cerr << "\n rootAncTec <E> bad call with ancflag = 1" << std::endl;
      std::exit(1);
    }  // safety check
    AncTecFile->Write();
    AncTecFile->Close();
    return;
  }  // check for close output file flag

  const int NTPL_PARAM = 24;
  float evt_ntpl[NTPL_PARAM];
  if(icall == 0){
    //
    // NTUPLE files

    AncTecFile = new TFile("anctec.root", "recreate", "Tec PISA NTUPLE");
    AncTecNtuple = new TNtuple("AncTec", "Tec Ancestors",
                              "TRACK:NFILE:PTOT:PTHETA:PPHI:"//  4
			       "R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"// 8
			       "ITPARENT:IDPARENT:IDPART:IHIT:NHIT:"// 13
			       "ITORIGIN:IDORIGIN:HITCOUNT:THETA:PHI:DEDX:TOF:"// 20
			       "Z0_EVENT:B_IMPACT:EVENT"); // 23

  } // initialization 
  icall++;

  int tecRows;
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
  int trueRow[MAXSTORE];
  int kstore;
  float theta;
  float phi;
  float xglobal, yglobal, zglobal;
  const float DEGRAD = 57.295779513;
  
  evt_ntpl[NTPL_PARAM - 1] = icall;
  nstore = 0;
  tecRows = TecPISAHit::GetTecCount();;
  TecPISAHit *tecghit = TecPISAHit::GetTecHitEvt();  
  
  for (iRow = 0; iRow < tecRows ; iRow++) {
    true_track = tecghit[iRow].GetMctrack();
    if(true_track < 1)
      continue;  // this is an error condition
   
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
      trueRow[nstore] = iRow;
      trueTimes[nstore++] = 1;
    }
  
  }
  for (istore = 0; istore < nstore; istore++) {
    true_track = trueStore[istore];
    if(true_track < 1)
      continue;   // error condition

    dio_ptrkorigin(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		  &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		  &itorigin, &idorigin, &idpart);  
/*
*  NOTE: idpart is primary particle id (=idorigin), not the input particle id.
*/
      
    dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		  &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		  &itparent, &idparent, &idpart);  

    evt_ntpl[NTPL_PARAM - 2] =EventHeader->GetImpactParameter(); 
    evt_ntpl[NTPL_PARAM - 3] =EventHeader->GetZvertex(); 
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
    evt_ntpl[13] = tecRows;
    evt_ntpl[14] = itorigin;
    evt_ntpl[15] = idorigin;

    iRow = trueRow[istore];
    xglobal = tecghit[iRow].GetXing();
    yglobal = tecghit[iRow].GetYing();
    zglobal = tecghit[iRow].GetZing();
    theta = DEGRAD*acos(zglobal/sqrt(xglobal*xglobal +
                        yglobal*yglobal + zglobal*zglobal));
    phi = DEGRAD*atan2(yglobal, xglobal);

    if(phi < -90.0)
      phi = 360.0 + phi;

    evt_ntpl[16] = trueTimes[istore];
    evt_ntpl[17] = theta;
    evt_ntpl[18] = phi;
    evt_ntpl[19] = tecghit[iRow].GetDeDx();
    evt_ntpl[20] = tecghit[iRow].GetTof();
   AncTecNtuple->Fill(evt_ntpl);
  }
  
  return ;

}
