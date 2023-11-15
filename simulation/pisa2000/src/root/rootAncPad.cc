#include <iostream>
#include <cstdlib>
#include "rootAnc.h"
#include "PISAEventHeader.h"
#include "PadPISAHit.h"
#include "TfwPISAHit.h"
#include "TofPISAHit.h"
#include "AerPISAHit.h"
#include <fstream>

#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "TStopwatch.h"
#include "TDirectory.h"
#include "TRandom.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"

void rootAncPad(Int_t ancflag, PISAEventHeader *EventHeader)
{

  //
  // Event header information not yet used
  //
  const int maxpart = 10000;
  int pctrack[maxpart][3];
  int kpart;
  int kpart1;
  int kpart2;
  int kpart3;
  int ipc123Base(0);
  int ipc123; /* IPC123 = JKL: L=0/1 for PC1; K = 0/1 for PC2; J = 0/1 for PC3 */

  int evttrack;
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

  float theta1;
  float theta3;
  float phi1(0);
  float phi3(0);
  float delth13;
  float delph13;

  int   trackmin=0;    // initialized after bug reportMar 2015 HvH 
  int   idmin;
  int   torigmin;
  float delmin;
  float realmin;
  
  int itorigin;
  int idorigin;
  float ptotpri;
  float pthetpri;
  float pphipri;
  float z0vertex;

  int iPC;
  float theta;
  float phi;
  float deltheta;
  float z0proj;
  float rglobal1, rglobal3;
  float xglobal1, yglobal1, zglobal1;
  float xglobal3, yglobal3, zglobal3;
  float xglobal, yglobal, zglobal;
  const float DEGRAD = 57.295779513;

  static int icall = 0;
  const int NTPL_PARAM = 50;
  float evt_ntpl[NTPL_PARAM];

  const int NTPL2_PARAM = 69;
  float evt_ntpl2[NTPL2_PARAM];

  //
  // Initialization
  //
  static TFile *AncPadFile = 0;
  static TNtuple *AncPadNtuple = 0;

  static TFile *AncPadFile2 = 0;
  static TNtuple *AncPadNtuple2 = 0;

  if(ancflag == 1) {
    //
    // Check that the file has been opened
    //
    if(icall == 0 || AncPadFile == 0){
      std::cerr << "\n rootAncPad <E> bad call for first NTUPLE with ancflag = 1" << std::endl;
      std::exit(1);
    }  // safety check
    AncPadFile->Write();
    AncPadFile->Close();

    if(icall == 0 || AncPadFile2 == 0){
      std::cerr << "\n rootAncPad <E> bad call for second NTUPLE with ancflag = 1" << std::endl;
      std::exit(1);
    }  // safety check
    AncPadFile2->Write();
    AncPadFile2->Close();

    return;
  }  // check for close output file flag

  static Int_t iRandomize = 0;
  static Float_t pc1RandomX = 0;
  static Float_t pc1RandomZ = 0;
  static Float_t pc3RandomX = 0;
  static Float_t pc3RandomZ = 0;
  static Float_t pc1Efficiency = 1.0;
  static Float_t pc3Efficiency = 1.0;

  if(icall == 0){
    //
    // NTUPLE files
    //
    AncPadFile = new TFile("ancpad.root", "recreate", "PAD PISA NTUPLE");
    AncPadNtuple = new TNtuple("AncPad", "PAD Ancestors",
			       "TRACK:NFILE:PTOT:PTHETA:PPHI:"//   4
                               "R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"// 8
                               "ITPARENT:IDPARENT:IDPART:IHIT:NHIT:"//  13
                               "IPC:THETA:PHI:ZGLOBAL:PTOT_PRI:PTHE_PRI:"//  19
			       "PPHI_PRI:Z0ORIGIN:ETRACK:ZLOCAL1:ZLOCAL2:"// 24
			       "XGLOBAL:XLOCAL1:XLOCAL2:"// 27
			       "YGLOBAL:YLOCAL1:YLOCAL2:IPC123:ITORIGIN:"// 32
			       "IDORIGIN:SECTOR:DELTH13:DELPH13:"// 36
			       "REALMIN:DELMIN:IDMIN:TORIGMIN:TRACKMIN:"// 41
                               "Z0PROJ:DELTHETA:TOFPC:PATHPC:BETAPC:"// 46
                               "Z0_EVENT:B_IMPACT:EVENT"); // 49

    AncPadFile2 = new TFile("ancpad2.root", "recreate", "PC3 Correlations NTUPLE");
    AncPadNtuple2 = new TNtuple("AncPad2", "PC3 CORRELATIONS",
				"TRACK:NFILE:PTOT:PTHETA:PPHI:"//   4
				"R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"// 8
				"ITPARENT:IDPARENT:IDPART:IHIT:NHIT:"//  13
				"IPC:THETA:PHI:ZGLOBAL:PTOT_PRI:PTHE_PRI:"//  19
				"PPHI_PRI:Z0ORIGIN:ETRACK:ZLOCAL1:ZLOCAL2:"// 24
				"XGLOBAL:XLOCAL1:XLOCAL2:"// 27
				"YGLOBAL:YLOCAL1:YLOCAL2:IPC123:ITORIGIN:"// 32
				"IDORIGIN:SECTOR:DELTH13:DELPH13:"// 36
				"REALMIN:DELMIN:IDMIN:TORIGMIN:TRACKMIN:"// 41
				"Z0PROJ:DELTHETA:TOFPC:PATHPC:BETAPC:"// 46, end of single particle data in PC3
	                        "TOFSAME:" // 47, same particle in TOF (azimuthal angle indicates East or West)
				"PC3SIST:PC3STHET:PC3SPHI:PC3STOF:"// 51, start of sister particle data
				"PC3SPART:PC3SPTOT:PC3SPTHET:PC3SPPHI:"// 55
                                "TOFSTOF:TOFSTHET:TOFSPHI:TOFSPART:"// 59, TOF could be TOF-East or TOF-West
				"TOFSPTOT:TOFSPTHET:TOFSPPHI:"// 62
                                "PC1SIST:AERSAME:AERSIST:" // 65, sister in PC1 : same in AER : sister in AER
				"Z0_EVENT:B_IMPACT:EVENT"); // 68
    
    std::ifstream padRandom("padRandom.txt");
    if( padRandom.fail() ) {
      std::cout << "rootAncPad <I>: no PC randomization requested"
		<< std::endl;
    }
    else {
      iRandomize = 1;
      gRandom->SetSeed();

      std::cout << "rootAncPad <I>: PC randomization has been requested";

      padRandom >> pc1RandomX >> pc1RandomZ;
      padRandom >> pc3RandomX >> pc3RandomZ;
      padRandom >> pc1Efficiency >> pc3Efficiency;

      std::cout << "\n pc1RandomX = " << pc1RandomX << " cm";
      std::cout << ",  pc1RandomZ = " << pc1RandomZ << " cm";
      std::cout << "\n pc3RandomX = " << pc3RandomX << " cm";
      std::cout << ",  pc3RandomZ = " << pc3RandomZ << " cm";
      std::cout << "\n pc1Efficiency = " << pc1Efficiency;
      std::cout << ",  pc3Efficiency = " << pc3Efficiency;
      std::cout << std::endl;

      padRandom.close();
    } // check on existence of padRandom.txt file

  }  // check on initialization

  icall++;

  AerPISAHit *aerghit = AerPISAHit::GetAerHitEvt();
  Int_t aerRows = AerPISAHit::GetAerCount();

  TfwPISAHit *tfwghit = TfwPISAHit::GetTfwHitEvt();
  Int_t tfwRows = TfwPISAHit::GetTfwCount();

  TofPISAHit *tofghit = TofPISAHit::GetTofHitEvt();
  Int_t tofRows = TofPISAHit::GetTofCount();

  Int_t pcRows = PadPISAHit::GetPadCount();
  Int_t pcRow[3];
  kpart1 = pcRow[0]  = PadPISAHit::GetPC1Count();
  kpart2 = pcRow[1]  = PadPISAHit::GetPC2Count();
  kpart3 = pcRow[2]  = PadPISAHit::GetPC3Count();

  PadPISAHit *pc1ghit = PadPISAHit::GetPC1HitEvt();
  PadPISAHit *pc2ghit = PadPISAHit::GetPC2HitEvt();
  PadPISAHit *pc3ghit = PadPISAHit::GetPC3HitEvt();

  for(iPC=0; iPC<3; iPC++){
    if(pcRow[iPC] > maxpart) {
      std::cerr << "\n rootAncPad <E> for PC" << iPC+1;
      std::cerr << " there are too many hits " << pcRow[iPC] << std::endl;
      std::exit(1);
    }  // safety check
  }  // loop over 3 PC values

  Int_t iRow;
  PadPISAHit *pcghit = PadPISAHit::GetPadHitEvt();
  for (iRow = 0; iRow < pcRows ; iRow++) {
    true_track = pcghit[iRow].GetMctrack();
    iPC = pcghit[iRow].GetIpc();
    switch (iPC) {
      case 1:{pctrack[iRow][0] = true_track; break;}
      case 2:{pctrack[iRow - pcRow[0]][1] = true_track; break;}
      case 3:{pctrack[iRow - pcRow[0] - pcRow[1]][2] = true_track; break;}
      default:{ std::cerr << "\n rootAncPad <E> invalid PCx number " << std::endl; std::exit(1);}
    }
  } // loop over all PC rows;


  evt_ntpl[NTPL_PARAM - 1] = icall;
  evt_ntpl[NTPL_PARAM - 2] = EventHeader->GetImpactParameter();
  evt_ntpl[NTPL_PARAM - 3] = EventHeader->GetZvertex();

  evt_ntpl2[NTPL2_PARAM - 1] = icall;
  evt_ntpl2[NTPL2_PARAM - 2] = EventHeader->GetImpactParameter();
  evt_ntpl2[NTPL2_PARAM - 3] = EventHeader->GetZvertex();

  for(iPC = 1; iPC < 4 ; iPC++){
    switch (iPC) {
    case 1: {pcghit = pc1ghit; pcRows = pcRow[iPC-1]; ipc123Base =   1; break;}
    case 2: {pcghit = pc2ghit; pcRows = pcRow[iPC-1]; ipc123Base =  10; break;}
    case 3: {pcghit = pc3ghit; pcRows = pcRow[iPC-1]; ipc123Base = 100; break;}
    }
    for (iRow = 0; iRow < pcRows ; iRow++) {
      true_track = pcghit[iRow].GetMctrack();
      ipc123 = ipc123Base;
      delth13 = -999;
      delph13 = -999;
      theta1 = -999.;
      theta3 = -999.;
      switch (iPC) {
      case 1:
	for (kpart = 0; kpart < kpart2; kpart++){
	  if(true_track == pctrack[kpart][1]){
	    ipc123 = ipc123 + 10;
	    break;
	  }
	}  // loop over PC2 rows for PC1
	for (kpart = 0; kpart < kpart3; kpart++){
	  if(true_track == pctrack[kpart][2]){
	    xglobal3 = pc3ghit[kpart].GetXing();
	    yglobal3 = pc3ghit[kpart].GetYing();
	    zglobal3 = pc3ghit[kpart].GetZing();
            float zdiff3 = zglobal3;
	    theta3 = DEGRAD*acos(zdiff3/sqrt(xglobal3*xglobal3 +
	                                     yglobal3*yglobal3 + zdiff3*zdiff3));

	    phi3 = DEGRAD*atan2(yglobal3, xglobal3);
	    if(phi3 < -90.0)
	      phi3 = 360.0 + phi3;

	    ipc123 = ipc123 + 100;
	    break;
	  }
	} // loop over PC3 rows for PC1
	break;
      case 2:
	for (kpart = 0; kpart < kpart1; kpart++){
	  if(true_track == pctrack[kpart][0]){
	    ipc123 = ipc123 + 1;
	    break;
	  }
	} // loop over PC1 rows for PC2
	for (kpart = 0; kpart < kpart3; kpart++){
	  if(true_track == pctrack[kpart][2]){
	    ipc123 = ipc123 + 100;
	    break;
	  }
	} // loop over PC3 rows for PC2
        break;
     case 3:
	for (kpart = 0; kpart < kpart1; kpart++){
	  if(true_track == pctrack[kpart][0]){
	    xglobal1 = pc1ghit[kpart].GetXing();
	    yglobal1 = pc1ghit[kpart].GetYing();
	    zglobal1 = pc1ghit[kpart].GetZing();
	    theta1 = DEGRAD*acos(zglobal1/sqrt(xglobal1*xglobal1 +
					       yglobal1*yglobal1 + zglobal1*zglobal1));
	    phi1 = DEGRAD*atan2(yglobal1, xglobal1);
	    if(phi1 < -90.0)
	      phi1 = 360.0 + phi1;

	    ipc123 = ipc123 + 1;
	    break;
	  }
	}  // loop over PC1 rows for PC3
	for (kpart = 0; kpart < kpart2; kpart++){
	  if(true_track == pctrack[kpart][1]){
	    ipc123 = ipc123 + 10;
	    break;
	  }
	}  // loop over PC2 rows for PC3
        break;

      } // check on which PCx

      dio_TrueTrackToEtrack(&true_track, &evttrack, &nfile);

      dio_ptrkorigin(&true_track, &nfile, &error, 
		     &ptotpri, &pthetpri, &pphipri,
	  	     &r_vertex, &z0vertex, &theta_vertex, &phi_vertex, 
		     &itorigin, &idorigin, &idpart);

      if(evttrack < 1) {
	if(itorigin > 0) {
	  //
	  // Something wrong with the true_track to event track (track number in event input file) in some cases??
	  // Try again to find the event track number using the origin track number
	  //
	  dio_TrueTrackToEtrack(&itorigin, &evttrack, &nfile);
	  if(evttrack < 1) {
	    //
	    // Still unable to get a valid event track number
	    //
	    std::cerr << "\n rootAncPad <E> : evttrack invalid " << evttrack << ", itorigin = " << itorigin << " at event " << icall << std::endl;
	  } // check on valid evttrack number
	  
	} // check on valid origin track number
	else {
	  std::cerr << "\n rootAncPad <E> : evttrack invalid " << evttrack << ", itorigin = " << itorigin << " at event " << icall << std::endl;
	}

      } // check on valid event track number

      bool foundOrigin = true;
      if(itorigin == -999)
        foundOrigin = false;

      dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		    &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		    &itparent, &idparent, &idpart);

      /*
      static int kPrint = 0;
      const int kPrintMax = 100;
      if(idparent != 0 || itparent == -true_track) {
	//
	// special diagnostic code used when dio_ptrkorigin not working for HYDJET events
	// problem resolved with changes in the getOscarRoot.f file (set parent id = 0, set multi_xyz = 0)
	//
	const int maxGeneration = 50;
	int true_trackCopy = itparent;
	if(true_trackCopy == 0) {
	  std::cerr << "\n\n true_trackCopy = 0" << std::endl << std::endl;
	  exit(1);
	}
	if(true_trackCopy < 0) {
	  true_trackCopy = -true_trackCopy;
	}
	if(iPC == 1 && kPrint < kPrintMax) {
	  std::cout << "\n\n Ancestry search for true_track " << true_track;
	  std::cout << ", particle ID " << idpart;
	  std::cout << ", with parent track " << itparent << std::endl;
	  kPrint++;
	}
	for(int iGeneration=0; iGeneration<maxGeneration; iGeneration++) {

	  float r_vertexPri;
	  float z0vertexPri;
	  int idpartPri;
	  
	  dio_ptrkstack(&true_trackCopy, &nfile, &error, 
			&ptotpri, &pthetpri, &pphipri,
			&r_vertexPri, &z0vertexPri, &theta_vertex, &phi_vertex, 
			&itorigin, &idorigin, &idpartPri);

	  if(iPC == 1 && kPrint < kPrintMax) { 
	    std::cout << "\n Generation = " << iGeneration + 1;
	    std::cout << " has true_trackCopy " << true_trackCopy;
	    std::cout << ", particle ID " << idpartPri;
	    std::cout << ", with parent track " << itorigin;
	    std::cout << ", and parent ID " << idorigin;
	  }
	  if(idorigin == 0 || itorigin == -true_trackCopy) {
	    idorigin = idpartPri;
	    if(idorigin < 0)
	      idorigin = 0;
	    itorigin = true_trackCopy;
	    foundOrigin = true;
	    if(iPC == 1 && kPrint < kPrintMax) { 
	      std::cout << "\n Found origin particle " << idorigin;
	      std::cout << ", and track number " << itorigin << std::endl;
	    }
	    break;
	  }
	  if(error != 0) {
	    if(kPrint < kPrintMax) {
	      std::cout << "\n Received error condition in rootAncPad" << std::endl;
	      kPrint++;
	    }
	    break;
	  }
	  true_trackCopy = itorigin;
	  if(true_trackCopy < 0) {
	    true_trackCopy = -true_trackCopy;
	  }
	  if(iPC == 1 && kPrint < kPrintMax) {
	    std::cout << "\n Setting new true_trackCopy " << true_trackCopy << std::endl;
	  }
	} // loop over 10 generations if needed
      }
      else {
	foundOrigin = true;
	idorigin = idpart;
	if(idorigin < 0)
	  idorigin = 0;
	itorigin = true_track;
	ptotpri = ptot;
	pthetpri = ptheta;
	pphipri = pphi;
      }
      */

      evt_ntpl[0] = float(true_track);
      if(!foundOrigin)
	evt_ntpl[0] = -evt_ntpl[0];

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
      evt_ntpl[13] = pcRows;
      evt_ntpl[14] = iPC;
      xglobal = pcghit[iRow].GetXing();
      yglobal = pcghit[iRow].GetYing();
      zglobal = pcghit[iRow].GetZing();

      //
      // Randomize coordinates
      //
      if(iRandomize == 1) {
	Float_t xRndm;
	Float_t zRndm;
	gRandom->Rannor(xRndm,zRndm);
	if(iPC==1) {
	  xglobal += xRndm*pc1RandomX;
	  zglobal += zRndm*pc1RandomZ;
	}
	else {
	  xglobal += xRndm*pc3RandomX;
	  zglobal += zRndm*pc3RandomZ;
	}
      } // check on randomization request (not set up for PC2 yet)

      float zdiff = zglobal;
      theta = DEGRAD*acos(zdiff/sqrt(xglobal*xglobal +
                          yglobal*yglobal + zdiff*zdiff));
      phi = DEGRAD*atan2(yglobal, xglobal);

      if(phi < -90.0)
        phi = 360.0 + phi;

      evt_ntpl[15] = theta;
      evt_ntpl[16] = phi;
      evt_ntpl[17] = zglobal;

      evt_ntpl[18] = ptotpri;
      evt_ntpl[19] = pthetpri;
      evt_ntpl[20] = pphipri;
      evt_ntpl[21] = z0vertex;
      evt_ntpl[22] = evttrack;
      evt_ntpl[23] = pcghit[iRow].GetZin();
      evt_ntpl[24] = pcghit[iRow].GetZout();
      evt_ntpl[25] = xglobal;
      evt_ntpl[26] = pcghit[iRow].GetXin();
      evt_ntpl[27] = pcghit[iRow].GetXout();
      evt_ntpl[28] = yglobal;
      evt_ntpl[29] = pcghit[iRow].GetYin();
      evt_ntpl[30] = pcghit[iRow].GetYout();
      evt_ntpl[31] = ipc123;
      evt_ntpl[32] = itorigin;
      evt_ntpl[33] = idorigin;
      evt_ntpl[34] = pcghit[iRow].GetSector();

      torigmin = -999999;
      idmin = -999;
      realmin = -999.0;
      delmin = -999.0;
      z0proj = -999.0;
      deltheta = -999.0;
      
      if(iPC==1&&ipc123>=101){
	delth13 = theta3 - theta;
	delph13 = phi3 - phi;
	realmin = fabs(delth13) + fabs(delph13);
      }

      delth13 = -999.0;
      delph13 = -999.0;

      if(iPC==1){
	delmin = +999.0;
	for (kpart = 0; kpart < kpart3; kpart++){
	  if(iRandomize == 1) {
	    //
	    // Check on efficiency for PC3 hit
	    //
	    Float_t randomEfficiency = gRandom->Rndm(1);
	    if(randomEfficiency>pc3Efficiency)
	      continue; // skip this particle

	  } // check on randomization
	  
	  xglobal3 = pc3ghit[kpart].GetXing();
	  yglobal3 = pc3ghit[kpart].GetYing();
	  zglobal3 = pc3ghit[kpart].GetZing();
	  //
	  // Randomize coordinates
	  //
	  if(iRandomize == 1) {
	    Float_t x3Rndm;
	    Float_t z3Rndm;
	    gRandom->Rannor(x3Rndm,z3Rndm);
	    xglobal3 += x3Rndm*pc3RandomX;
            zglobal3 += z3Rndm*pc3RandomZ;
	  } // check on whether randomization has been requested

	  theta3 = DEGRAD*acos(zglobal3/sqrt(xglobal3*xglobal3 +
					     yglobal3*yglobal3 + zglobal3*zglobal3));
	  phi3 = DEGRAD*atan2(yglobal3, xglobal3);
	  if(phi3 < -90.0)
	     phi3 = 360.0 + phi3;

	  if(fabs(theta3 - theta) + fabs(phi3 - phi) < delmin) {
	    delmin = fabs(theta3 - theta) + fabs(phi3 - phi);
	    trackmin = pc3ghit[kpart].GetMctrack();
	    dio_ptrkorigin(&trackmin, &nfile, &error, 
	    		   &ptotpri, &pthetpri, &pphipri,
	  	           &r_vertex, &z0vertex, &theta_vertex, &phi_vertex, 
		           &itorigin, &idorigin, &idpart);
	    torigmin = itorigin;
	    dio_ptrkstack(&trackmin, &nfile, &error, 
		          &ptotpri, &pthetpri, &pphipri,
	  	          &r_vertex, &z0vertex, &theta_vertex, &phi_vertex, 
		          &itorigin, &idorigin, &idpart);
	    idmin = idpart;

	    rglobal1 = sqrt(xglobal*xglobal + yglobal*yglobal);
	    rglobal3 = sqrt(xglobal3*xglobal3 + yglobal3*yglobal3);
	    float zrslope = (zglobal3 - zglobal)/(rglobal3 - rglobal1);
            z0proj = zglobal3 - zrslope*rglobal3;

            float theta3Rad = theta3/DEGRAD;
            float theta1Rad = theta/DEGRAD;
            float phi3Rad = phi3/DEGRAD;
            float phi1Rad = phi/DEGRAD;

	    float fact = sin(theta1Rad)*sin(theta3Rad)*cos(phi3Rad - phi1Rad) +
                         cos(theta1Rad)*cos(theta3Rad);

            if(fabs(fact)<=1.0)
               deltheta = DEGRAD*acos(fact);
            else
               deltheta = +999.0;

	    delth13 = theta3 - theta;
	    delph13 = phi3 - phi;

	  } // check on absolute Theta and Phi differences
	} // loop on kpart for PC3
      } // check on IPC = 1

      if(iPC==3&&ipc123>=101){
	delth13 = theta - theta1;
	delph13 = phi - phi1;
	realmin = fabs(delth13) + fabs(delph13);
      }

      if(iPC==3){
	delmin = +999.0;
	for (kpart = 0; kpart < kpart1; kpart++){
	  xglobal1 = pc1ghit[kpart].GetXing();
	  yglobal1 = pc1ghit[kpart].GetYing();
	  zglobal1 = pc1ghit[kpart].GetZing();
          float zdiff1 = zglobal1;
	  theta1 = DEGRAD*acos(zdiff1/sqrt(xglobal1*xglobal1 +
					   yglobal1*yglobal1 + zdiff1*zdiff1));
	  phi1 = DEGRAD*atan2(yglobal1, xglobal1);
	  if(phi1 < -90.0)
	     phi1 = 360.0 + phi1;

	  if(fabs(theta - theta1) + fabs(phi - phi1) < delmin) {
	    delmin = fabs(theta - theta1) + fabs(phi - phi1);
	    trackmin = pc1ghit[kpart].GetMctrack();
	    dio_ptrkorigin(&trackmin, &nfile, &error, 
	    		   &ptotpri, &pthetpri, &pphipri,
	  	           &r_vertex, &z0vertex, &theta_vertex, &phi_vertex, 
		           &itorigin, &idorigin, &idpart);
	    torigmin = itorigin;
	    dio_ptrkstack(&trackmin, &nfile, &error, 
		          &ptotpri, &pthetpri, &pphipri,
	  	          &r_vertex, &z0vertex, &theta_vertex, &phi_vertex, 
		          &itorigin, &idorigin, &idpart);
	    idmin = idpart;

	    rglobal1 = sqrt(xglobal1*xglobal1 + yglobal1*yglobal1);
	    rglobal3 = sqrt(xglobal*xglobal + yglobal*yglobal);
	    float zrslope = (zglobal - zglobal1)/(rglobal3 - rglobal1);
            z0proj = zglobal - zrslope*rglobal3;

            float theta3Rad = theta/DEGRAD;
            float theta1Rad = theta1/DEGRAD;
            float phi3Rad = phi/DEGRAD;
            float phi1Rad = phi1/DEGRAD;

	    float fact = sin(theta1Rad)*sin(theta3Rad)*cos(phi3Rad - phi1Rad) +
                         cos(theta1Rad)*cos(theta3Rad);

            if(fabs(fact)<=1.0)
               deltheta = DEGRAD*acos(fact);
            else
               deltheta = +999.0;

	    if(ipc123<101){
	      delth13 = theta - theta1;
	      delph13 = phi - phi1;
	    }
	  }
	}
      }

      evt_ntpl[35] = delth13;
      evt_ntpl[36] = delph13;
      evt_ntpl[37] = realmin;
      evt_ntpl[38] = delmin;
      evt_ntpl[39] = idmin;
      evt_ntpl[40] = torigmin;
      evt_ntpl[41] = trackmin;
      evt_ntpl[42] = z0proj;
      evt_ntpl[43] = deltheta;

      evt_ntpl[44] =  pcghit[iRow].GetTof(); 
      evt_ntpl[45] =  pcghit[iRow].GetPathLength(); 
      if(evt_ntpl[44]>0.0)
         evt_ntpl[46] = evt_ntpl[45]/(evt_ntpl[44]*29.9792458);
      else
         evt_ntpl[46] = -9999.0;

      AncPadNtuple->Fill(evt_ntpl);

      //
      // Start of correlation NTUPLE filling (only for PC3 hits)
      //
      if(iPC==3) {

	//
	// initial event information
	//
	for(int kNtpl2=0; kNtpl2<47; kNtpl2++) {
	  evt_ntpl2[kNtpl2] = evt_ntpl[kNtpl2]; // mirror image
	}

	for(int kNtpl2=47; kNtpl2<NTPL2_PARAM-3; kNtpl2++) {
	  evt_ntpl2[kNtpl2] = -999.0; // default for no sister information
	}
 
	//
	// search for same particle in TOF East
	//
	for (int kRow = 0; kRow < tofRows ; kRow++) {

	  int tof_track = tofghit[kRow].GetMctrack();
	  if(tof_track < 1)
	    continue ;  // this is an error condition

	  if(tof_track == true_track) {
	    //
	    // same particle is in TOF-East
	    //
	    xglobal = tofghit[kRow].GetXm();
	    yglobal = tofghit[kRow].GetYm();
	    phi = DEGRAD*atan2(yglobal, xglobal);
	    if( phi < -90.0 )
	      phi = 360.0 + phi;
	    //std::cout << "\n Setting TOF-East same track phi at " << phi << " for icall = " << icall << std::endl;
	    evt_ntpl2[47] = phi;  // azimuthal angle > 168.75 for TOF-East
	    break;  // break after the first match, TOF-East may store same track more than once
	  } // check for the same track;
	} // loop over TOF-East rows

	//
	// search for sister particle in TOF East
	//
	for (int kRow = 0; kRow < tofRows ; kRow++) {

	  int tof_track = tofghit[kRow].GetMctrack();
	  if(tof_track < 1 || tof_track == true_track)
	    continue;

	  //
	  // check if the parent track numbers are the same
	  //
	  int tofparent;
	  dio_ptrkstack(&tof_track, &nfile, &error, &ptot, &ptheta, &pphi,
			&r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
			&tofparent, &idparent, &idpart);
	  if(tofparent == itparent) {
	    evt_ntpl2[56] = tofghit[kRow].GetTof();
	    xglobal = tofghit[kRow].GetXm();
	    yglobal = tofghit[kRow].GetYm();
	    zglobal = tofghit[kRow].GetZm();
	    float mctofdis = sqrt(xglobal*xglobal + yglobal*yglobal + zglobal*zglobal);
	    theta = DEGRAD*acos(zglobal/mctofdis);
	    phi = DEGRAD*atan2(yglobal, xglobal);
	    if( phi < -90.0 )
	      phi = 360.0 + phi;
	    evt_ntpl2[57] = theta;
	    evt_ntpl2[58] = phi;
	    evt_ntpl2[59] = idpart;
	    evt_ntpl2[60] = ptot;
	    evt_ntpl2[61] = ptheta;
	    evt_ntpl2[62] = pphi;
	    break; // break after the first match, TOF-East may store same track more than once
	  } // check if same parent track number

	} // loop over TOF-East rows

	//
	// search for same particle in TOF-West
	//
	for (int kRow = 0; kRow < tfwRows ; kRow++) {

	  int tof_track = tfwghit[kRow].GetMctrack();

	  if(tof_track < 1)
	    continue ;  // this is an error condition

	  if(tof_track == true_track) {
	    //
	    // same particle is in TOF-West
	    //
	    xglobal = tfwghit[kRow].GetXing();
	    yglobal = tfwghit[kRow].GetYing();
	    phi = DEGRAD*atan2(yglobal, xglobal);
	    if( phi < -90.0 )
	      phi = 360.0 + phi;
	    // std::cout << "\n Attempt to set TOF-West same track phi at " << phi << " for icall = " << icall;
	    //std::cout << "\n current evt_ntpl2[47] = " << evt_ntpl2[47] << std::endl;
	    if(evt_ntpl2[47] < -90.0) {// should be still at default -999
	      evt_ntpl2[47] = phi;  // -35 < azimuthal angle < 60 for TOF-West
	    }
	    else {
	      evt_ntpl2[47] = -75.0; // indicate a problem
	    }
	    break;  // TOF-West does store same track more than once
	   
	  } // check on true track
 
	} // loop over TOF-West rows

	//
	// search for sister particle in TOF-West
	//
	for (int kRow = 0; kRow < tfwRows ; kRow++) {

	  int tof_track = tfwghit[kRow].GetMctrack();
	  if(tof_track < 1 || tof_track == true_track)
	    continue;

	  int tofparent;
	  dio_ptrkstack(&tof_track, &nfile, &error, &ptot, &ptheta, &pphi,
			&r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
			&tofparent, &idparent, &idpart);
	  if(tofparent == itparent) {
	    evt_ntpl2[56] = tfwghit[kRow].GetTof();
	    xglobal = tfwghit[kRow].GetXing();
	    yglobal = tfwghit[kRow].GetYing();
	    zglobal = tfwghit[kRow].GetZing();
	    float mctofdis = sqrt(xglobal*xglobal + yglobal*yglobal + zglobal*zglobal);
	    theta = DEGRAD*acos(zglobal/mctofdis);
	    phi = DEGRAD*atan2(yglobal, xglobal);
	    if( phi < -90.0 )
	      phi = 360.0 + phi;
	    evt_ntpl2[57] = theta;
	    evt_ntpl2[58] = phi;
	    evt_ntpl2[59] = idpart;
	    evt_ntpl2[60] = ptot;
	    evt_ntpl2[61] = ptheta;
	    evt_ntpl2[62] = pphi;
	    break;  // TOF-West stores same track more than once
	  } // check if same parent track number

	} // loop over TOF-West rows
	
	//
	// PC3 sister information
	//
	for(int kRow = 0; kRow < pcRows; kRow++ ) {

	  int kPC = pcghit[kRow].GetIpc();
	  if(kPC == 3) {
	    int pc3_track = pcghit[kRow].GetMctrack();
	    if (pc3_track < 1 || pc3_track == true_track)
	      continue;

	    int pc3parent;
	    dio_ptrkstack(&pc3_track, &nfile, &error, &ptot, &ptheta, &pphi,
			  &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
			  &pc3parent, &idparent, &idpart);
	    if(pc3parent == itparent) {
	      evt_ntpl2[48] = pcghit[kRow].GetSector();
	      xglobal = pcghit[kRow].GetXing();
	      yglobal = pcghit[kRow].GetYing();
	      zglobal = pcghit[kRow].GetZing();
	      theta = DEGRAD*acos(zglobal/sqrt(xglobal*xglobal +
					       yglobal*yglobal + zglobal*zglobal));
	      phi = DEGRAD*atan2(yglobal, xglobal);

	      if(phi < -90.0)
		phi = 360.0 + phi;

	      evt_ntpl2[49] = theta;
	      evt_ntpl2[50] = phi;
	      evt_ntpl2[51] = pcghit[kRow].GetTof();
	      evt_ntpl2[52] = idpart;
	      evt_ntpl2[53] = ptot;
	      evt_ntpl2[54] = ptheta;
	      evt_ntpl2[55] = pphi;
	      break;  // can break out of loop after sister particle has been detected
	    } // check if parent track numbers are the same

	  } // check if hit was in PC3

	} // loop over all PC3 rows
	
	//
	// PC1 sister information
	//
	for(int kRow = 0; kRow < kpart1; kRow++ ) {

	  int kPC = pc1ghit[kRow].GetIpc();
	  if(kPC == 1) {
	    int pc1_track = pc1ghit[kRow].GetMctrack();
	    if (pc1_track < 1 || pc1_track == true_track)
	      continue;

	    int pc1parent;
	    dio_ptrkstack(&pc1_track, &nfile, &error, &ptot, &ptheta, &pphi,
			  &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
			  &pc1parent, &idparent, &idpart);
	    if(pc1parent == itparent) {
	      evt_ntpl2[63] = pc1ghit[kRow].GetSector();
              break;  // break out of loop after sister track is found in PC1
            } // check for parent track match
          } // check for PC1 hit (should always be the case for pc1ghit)

        } // loop over all PC1 rows

	//
	// search for same track in aerogel
	//
	for (int kRow = 0; kRow < aerRows ; kRow++) {

	  int aer_track = aerghit[kRow].GetMctrack();

	  if(aer_track == true_track) {
	    float px = aerghit[kRow].GetPx();
	    float py = aerghit[kRow].GetPy();
	    float pz = aerghit[kRow].GetPz();
	    evt_ntpl2[64] = sqrt(px*px + py*py + pz*pz); // momentum in aerogel
	    break;
	  }
	} // loop over aerogel rows

	//
	// search for sister particle in aerogel
	//
	for (int kRow = 0; kRow < aerRows ; kRow++) {

	  int aer_track = aerghit[kRow].GetMctrack();
	  if(aer_track < 1 || aer_track == true_track)
	    continue;

	  int aerparent;
	  dio_ptrkstack(&aer_track, &nfile, &error, &ptot, &ptheta, &pphi,
		      &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		      &aerparent, &idparent, &idpart);
	  if(aerparent == itparent) {
	    xglobal = aerghit[kRow].GetX();
	    yglobal = aerghit[kRow].GetY();
	    phi = DEGRAD*atan2(yglobal, xglobal);

	    if( phi < -90.0 )
	      phi = 360.0 + phi;

	    evt_ntpl2[65] = phi;   // azimuthal angle in aerogel
	    break;
	  } // check on parent track match

	} // loop over aerogel rows

	AncPadNtuple2->Fill(evt_ntpl2);

      } // check on PC3 hit for correlations filling

    } // loop on rows for this PCx

  } // loop on all 3 PCs

  return;
}

