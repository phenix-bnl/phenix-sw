#include <iostream>
#include <cstdlib>
#include "rootAnc.h"
#include "PISAEventHeader.h"
#include "PadPISAHit.h"
#include "SvxPISAHit.h"

void rootAncSvx(Int_t ancflag, PISAEventHeader *EventHeader)
{

  static int icall = 0;
  static TFile *AncSvxFile = 0;
  static TNtuple *AncSvxNtuple = 0;

  if(ancflag == 1) {
    //
    // Check that the file has been opened
    //
    if(icall == 0 || AncSvxFile == 0){
      std::cerr << "\n rootAncSvx <E> bad call with ancflag = 1" << std::endl;
      std::exit(1);
    }  // safety check
    AncSvxFile->Write();
    AncSvxFile->Close();
    return;
  }  // check for close output file flag

  const int NTPL_PARAM = 59;
  float evt_ntpl[NTPL_PARAM];
  if(icall == 0){
    //
    // NTUPLE files
    //
    AncSvxFile = new TFile("ancsvx.root", "recreate", "SVX PISA NTUPLE");
    AncSvxNtuple = new TNtuple("AncSvx", "SVX Ancestors",
                               "TRACK:NFILE:PTOT:PTHETA:PPHI:"//
			       "R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"//
			       "ITPARENT:IDPARENT:IDPART:ITORIGIN:IDORIGIN:"//
			       "IHIT:LAYER:THETA:PHI:"//
			       "XGLOBAL:YGLOBAL:ZGLOBAL:PMOMX:PMOMY:PMOMZ:"//
			       "IPC123:PC1THETA:PC1PHI:PC1RDIST:PC1ZDIST:"//
                               "PC2THETA:PC2PHI:PC2RDIST:PC2ZDIST:"//
			       "PC3THETA:PC3PHI:PC3RDIST:PC3ZDIST:DELE:"//
			       "XLOCIN:YLOCIN:ZLOCIN:XLOCOUT:YLOCOUT:ZLOCOUT:"//
                               "HITVOL0:HITVOL1:HITVOL2:HITVOL3:HITVOL4:HITVOL5:"//
                               "HITVOL6:HITVOL7:HITVOL8:"//
			       "NHIT:X0_EVENT:Y0_EVENT:Z0_EVENT:B_IMPACT:EVENT");

  }  // initialization
  icall++;

  int svxRows;

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

  float theta;
  float phi;
  float xglobal, yglobal, zglobal;
  const float DEGRAD = 57.295779513;

  int iRow;
  int iPC;
  const int maxpart = 10000;
  int pctrack[maxpart][3];
  int kpart;
  int kpart1;
  int kpart2;
  int kpart3;
  int ipc123; /* IPC123 = JKL: L=0/1 for PC1; K = 0/1 for PC2; J = 0/1 for PC3 */

  Int_t pcRows = PadPISAHit::GetPadCount();
  Int_t pcRow[3];
  kpart1 = pcRow[0]  = PadPISAHit::GetPC1Count();
  kpart2 = pcRow[1]  = PadPISAHit::GetPC2Count();
  kpart3 = pcRow[2]  = PadPISAHit::GetPC3Count();

  //std::cout << "\n kpart1 = " << kpart1;
  //std::cout << ",  kpart2 = " << kpart2;
  //std::cout << ",  kpart3 = " << kpart3;
  //std::cout << std::endl;

  PadPISAHit *pc1ghit = PadPISAHit::GetPC1HitEvt();
  PadPISAHit *pc2ghit = PadPISAHit::GetPC2HitEvt();
  PadPISAHit *pc3ghit = PadPISAHit::GetPC3HitEvt();
  PadPISAHit *pcghit = 0;

  for(iPC=0; iPC<3; iPC++){
    if(pcRow[iPC] > maxpart) {
      std::cerr << "\n rootAncSvx <E> for PC" << iPC+1;
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
  evt_ntpl[NTPL_PARAM - 2] = EventHeader->GetImpactParameter();
  evt_ntpl[NTPL_PARAM - 3] = EventHeader->GetZvertex();
  evt_ntpl[NTPL_PARAM - 4] = EventHeader->GetYvertex();
  evt_ntpl[NTPL_PARAM - 5] = EventHeader->GetXvertex();

  svxRows = SvxPISAHit::GetSvxCount();
  SvxPISAHit *svxghit = SvxPISAHit::GetSvxHitEvt();

  evt_ntpl[NTPL_PARAM - 6] = svxRows;
  for (iRow = 0; iRow < svxRows ; iRow++) {

    true_track = svxghit[iRow].GetMctrack();
    if(true_track < 1)
      continue ;  // this is an error condition

    dio_ptrkorigin(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		  &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		  &itorigin, &idorigin, &idpart);  
/*
*  NOTE: idpart is primary particle id (=idorigin), not the input particle id.
*/
      
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
    evt_ntpl[12] = float(itorigin);
    evt_ntpl[13] = float(idorigin);
    evt_ntpl[14] = iRow;
    evt_ntpl[15] = svxghit[iRow].GetLayer();

    xglobal = svxghit[iRow].GetX();
    yglobal = svxghit[iRow].GetY();
    zglobal = svxghit[iRow].GetZ();

    theta = DEGRAD*acos(zglobal/sqrt(xglobal*xglobal +
                        yglobal*yglobal + zglobal*zglobal));
    phi = DEGRAD*atan2(yglobal, xglobal);
    if( phi < -90.0 )
      phi = 360.0 + phi;
    evt_ntpl[16] = theta;
    evt_ntpl[17] = phi;

    evt_ntpl[18] = xglobal;
    evt_ntpl[19] = yglobal;
    evt_ntpl[20] = zglobal;
    evt_ntpl[21] = svxghit[iRow].GetPx();
    evt_ntpl[22] = svxghit[iRow].GetPy();
    evt_ntpl[23] = svxghit[iRow].GetPz();

    for(Int_t iFill=25; iFill<37; iFill++) {
      evt_ntpl[iFill] = -9999.0;
    } // initialize PC1/PC2/PC3 correlation to no match condition

    ipc123 = 0;
    theta = -999.0;
    phi = -999.0;
    
    if(idpart != 1){
      for (kpart = 0; kpart < kpart1; kpart++){
        if(true_track == pctrack[kpart][0]){
	  ipc123 = 1;
	  xglobal = pc1ghit[kpart].GetXing();
	  yglobal = pc1ghit[kpart].GetYing();
	  zglobal = pc1ghit[kpart].GetZing();
	  theta = DEGRAD*acos(zglobal/sqrt(xglobal*xglobal +
					    yglobal*yglobal + zglobal*zglobal));
	  phi = DEGRAD*atan2(yglobal, xglobal);
	  if(phi < -90.0)
	    phi = 360.0 + phi;

	  evt_ntpl[25] = theta; 
	  evt_ntpl[26] = phi; 
	  evt_ntpl[27] = sqrt(xglobal*xglobal + yglobal*yglobal);
	  evt_ntpl[28] = zglobal;

	  break;
	}
      }
      for (kpart = 0; kpart < kpart2; kpart++){
        if(true_track == pctrack[kpart][1]){
	  ipc123 = ipc123 + 10;
	  xglobal = pc2ghit[kpart].GetXing();
	  yglobal = pc2ghit[kpart].GetYing();
	  zglobal = pc2ghit[kpart].GetZing();
	  theta = DEGRAD*acos(zglobal/sqrt(xglobal*xglobal +
					    yglobal*yglobal + zglobal*zglobal));
	  phi = DEGRAD*atan2(yglobal, xglobal);
	  if(phi < -90.0)
	    phi = 360.0 + phi;

	  evt_ntpl[29] = theta; 
	  evt_ntpl[30] = phi; 
	  evt_ntpl[31] = sqrt(xglobal*xglobal + yglobal*yglobal);
	  evt_ntpl[32] = zglobal;

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

	  evt_ntpl[33] = theta; 
	  evt_ntpl[34] = phi; 
	  evt_ntpl[35] = sqrt(xglobal*xglobal + yglobal*yglobal);
	  evt_ntpl[36] = zglobal;

	 break;
        }  // check on true_track
      }  // loop over kpart
    }  // check on not being a photon

    evt_ntpl[24] = ipc123;
    evt_ntpl[37] = 1000.0*svxghit[iRow].GetDele();  // store in units of MeV

    //
    // New additions from Valdimir
    //
    evt_ntpl[38] = svxghit[iRow].GetXLI();
    evt_ntpl[39] = svxghit[iRow].GetYLI();
    evt_ntpl[40] = svxghit[iRow].GetZLI();
    evt_ntpl[41] = svxghit[iRow].GetXLO();
    evt_ntpl[42] = svxghit[iRow].GetYLO();
    evt_ntpl[43] = svxghit[iRow].GetZLO();
    evt_ntpl[44] = svxghit[iRow].GetHitVol0();
    printf("root_AncSvx.cc: vol0= %f",evt_ntpl[44]);
    evt_ntpl[45] = svxghit[iRow].GetHitVol1();
    evt_ntpl[46] = svxghit[iRow].GetHitVol2();
    evt_ntpl[47] = svxghit[iRow].GetHitVol3();
    evt_ntpl[48] = svxghit[iRow].GetHitVol4();
    evt_ntpl[49] = svxghit[iRow].GetHitVol5();
    evt_ntpl[50] = svxghit[iRow].GetHitVol6();
    evt_ntpl[51] = svxghit[iRow].GetHitVol7();
    evt_ntpl[52] = svxghit[iRow].GetHitVol8();

    AncSvxNtuple->Fill(evt_ntpl);

  }  // loop over iRow

  return;
}
