#include <iostream>
#include <cstdlib>
#include "PISARun.h"
#include "PISAEventHeader.h"
#include "TfwPISAHit.h"
#include "PadPISAHit.h"
#include "TofPISAHit.h"
#include "AerPISAHit.h"
#include <fstream>

#include <TROOT.h>
#include <TFile.h>
#include <TTree.h>
#include <TBranch.h>
#include <TStopwatch.h>
#include <TDirectory.h>
#include <TH1.h>
#include <TH2.h>
#include <TNtuple.h>

using namespace std;

void PISARun::AncTfw(const int & ancflag, PISAEvent::pointer pisa_event)
{
  
  // retrieve pisa event header
  PISAEventHeader *EventHeader = pisa_event->GetHeader();

  // Event header information not yet used
  //const int maxpart = 10000;
  //int pctrack[maxpart];
  //int kpart;

  int evttrack(0);
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
  float ptotpri(0);
  float pthetpri(0);
  float pphipri(0);
  float z0vertex(0);

  float theta(0);
  float phi(0);
  float xglobal(0), yglobal(0), zglobal(0);
  float mctofdis(0);
  
  const float DEGRAD = 57.295779513;

  static int icall = 0;
  const int NTPL_PARAM = 40;
  float evt_ntpl[NTPL_PARAM];

  const int NTPL2_PARAM = 66;
  float evt_ntpl2[NTPL2_PARAM];

  //
  // Initialization
  //
  static TFile *AncTfwFile = 0;
  static TNtuple *AncTfwNtuple = 0;

  static TFile *AncTfwFile2 = 0;
  static TNtuple *AncTfwNtuple2 = 0;

  if(ancflag == 1) {
    //
    // Check that the file has been opened
    //
    if(icall == 0 || AncTfwFile == 0){
      cout << "PISARun::AncTfw - bad call first NTUPLE with ancflag = 1" << endl;
      exit(1);
    }  // safety check
    AncTfwFile->Write();
    AncTfwFile->Close();

    if(icall == 0 || AncTfwFile2 == 0){
      cout << "PISARun::AncTfw - bad call second NTUPLE with ancflag = 1" << endl;
      exit(1);
    }  // safety check
    AncTfwFile2->Write();
    AncTfwFile2->Close();

    return;
  }  // check for close output file flag

  if(icall == 0){
    //
    // NTUPLE files
    //
    AncTfwFile = new TFile("anctfw.root", "recreate", "TFW PISA NTUPLE");
    AncTfwNtuple = new TNtuple("AncTfw", "TFW Ancestors",
			       "TRACK:NFILE:PTOT:PTHETA:PPHI:"//   4
			       "R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"// 8
			       "ITPARENT:IDPARENT:IDPART:IHIT:NHIT:"// 13
			       "THETA:PHI:DELE:PTOT_PRI:PTHE_PRI:"// 18
			       "PPHI_PRI:Z0ORIGIN:ETRACK:"// 21 
			       "XGLOBAL:XLOCAL1:XLOCAL2:"// 24
			       "YGLOBAL:YLOCAL1:YLOCAL2:"// 27
			       "ZGLOBAL:ZLOCAL1:ZLOCAL2:"// 30
			       "ITORIGIN:IDORIGIN:PANEL:"// 33
			       "TOFTFW:PATHTFW:BETATFW:"// 36
			       "Z0_EVENT:B_IMPACT:EVENT"); // 39
    
    AncTfwFile2 = new TFile("anctfw2.root", "recreate", "TFW CORRELATIONS PISA NTUPLE");
    AncTfwNtuple2 = new TNtuple("AncTfw2", "TFW Ancestors with Correlations",
				"TRACK:NFILE:PTOT:PTHETA:PPHI:"//   4
				"R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"// 8
				"ITPARENT:IDPARENT:IDPART:IHIT:NHIT:"// 13
				"THETA:PHI:DELE:PTOT_PRI:PTHE_PRI:"// 18
				"PPHI_PRI:Z0ORIGIN:ETRACK:"// 21 
				"XGLOBAL:XLOCAL1:XLOCAL2:"// 24
				"YGLOBAL:YLOCAL1:YLOCAL2:"// 27
				"ZGLOBAL:ZLOCAL1:ZLOCAL2:"// 30
				"ITORIGIN:IDORIGIN:PANEL:"// 33
				"TOFTFW:PATHTFW:BETATFW:"// 36
				"NTFWHIT:PC2SAME:AERSAME:PC3SAME:PC3SIST:"// 41
				"PC3STHET:PC3SPHI:PC3STOF:PC3SPART:PC3SPTOT:PC3SPTHET:PC3SPPHI:"// 48
				"TOFESTOF:TOFESTHET:TOFESPHI:TOFSPART:TOFSPTOT:TOFSPTHET:TOFSPPHI:"// 55
				"PC1SAME:PC1SIST:PC1SPART:PC2SIST:PC2SPART:"// 60
				"TOFWSPHI:AERSPHI:"// 62
				"Z0_EVENT:B_IMPACT:EVENT"); // 65
    
  }  // check on initialization

  icall++;

  int tfwRows;
  int iRow;

  evt_ntpl[NTPL_PARAM - 1] = icall;
  tfwRows = TfwPISAHit::GetTfwCount();
  TfwPISAHit *tfwghit = TfwPISAHit::GetTfwHitEvt();

  struct tfwInfo {
    int countTrack;
    int track;
    int nfile;
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
    int ihit;
    float xglobal;
    float yglobal;
    float zglobal;
    float theta;
    float phi;
    float dele;
    float ptotpri;
    float pthetpri;
    float pphipri;
    float z0vertex;
    int evttrack;
    float xlocal1;
    float xlocal2;
    float ylocal1;
    float ylocal2;
    float zlocal1;
    float zlocal2;
    int itorigin;
    int idorigin;
    int panel;
    float tof;
    float pathLength;
    float beta;

  };

  int countStore = 0;
  int *storeTrack = 0;
  tfwInfo *tfwArray = 0;
  if(tfwRows > 0) {
    tfwArray = new tfwInfo[tfwRows];
    storeTrack = new int[tfwRows];
  }

  evt_ntpl[NTPL_PARAM - 1] = icall; // used as eventnumber
  evt_ntpl[NTPL_PARAM - 2] = EventHeader->GetImpactParameter();
  evt_ntpl[NTPL_PARAM - 3] = EventHeader->GetZvertex();  // z vertex of event

  evt_ntpl2[NTPL2_PARAM - 1] = icall; // used as eventnumber
  evt_ntpl2[NTPL2_PARAM - 2] = EventHeader->GetImpactParameter();
  evt_ntpl2[NTPL2_PARAM - 3] = EventHeader->GetZvertex();  // z vertex of event

  evt_ntpl[13] = tfwRows; // total number of hits in this event for TOF-West
  evt_ntpl2[13] = tfwRows; // total number of hits in this event for TOF-West

  for (iRow = 0; iRow < tfwRows ; iRow++) {

    true_track = tfwghit[iRow].GetMctrack();
    if(true_track < 1)
      continue ;  // this is an error condition

    dio_TrueTrackToEtrack(&true_track, &evttrack, &nfile);  // not working?

    //
    // the original primary particle
    //
    dio_ptrkorigin(&true_track, &nfile, &error, 
		   &ptotpri, &pthetpri, &pphipri,
		   &r_vertex, &z0vertex, &theta_vertex, &phi_vertex, 
		   &itorigin, &idorigin, &idpart);

    /*
     *  NOTE: idpart is primary particle id (=idorigin), not the input particle id.
     */
      
    //
    // the immediate ancestor of this particle
    //
    dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		  &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		  &itparent, &idparent, &idpart);  

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
	  std::cerr << "\n rootAncTfw <E> : evttrack invalid " << evttrack << ", itorigin = " << itorigin << " at event " << icall << std::endl;
	} // check on valid evttrack number
	
      } // check on valid origin track number
      else {
	std::cerr << "\n rootAncTfw <E> : evttrack invalid " << evttrack << ", itorigin = " << itorigin << " at event " << icall << std::endl;
      }

    } // check on valid event track number

    evt_ntpl[0] = float(true_track);
    evt_ntpl[1] = float(nfile);  // used for multiple (MERGE) file inputs
    evt_ntpl[2] = ptot;  // total momentum of this particle
    evt_ntpl[3] = ptheta; // polar direction of this particle's momentum
    evt_ntpl[4] = pphi;   // azimuthal direction of this particle's momentum
    evt_ntpl[5] = r_vertex;  // radial position of this particle's original vertex
    evt_ntpl[6] = z_vertex;  // z position of this particle's original vertex
    evt_ntpl[7] = theta_vertex; // polar angle of this particle's original vertex
    evt_ntpl[8] = phi_vertex;   // azimuthal angle of this particle's original vertex
    evt_ntpl[9] = float(itparent);  // track number of the parent of this particle
    evt_ntpl[10] = float(idparent); // GEANT particle ID of the parent of this particle
    evt_ntpl[11] = float(idpart);   // GEANT particle of ID of this particle
    evt_ntpl[12] = iRow+1; // hit number in TOF-West for this event

    xglobal = tfwghit[iRow].GetXing(); // global (lab frame) x position at entrance to TOF-West
    yglobal = tfwghit[iRow].GetYing(); // global y position at entrance to TOF-West
    zglobal = tfwghit[iRow].GetZing(); // global z position at entrance to TOF-West

    float mctfwdis = sqrt(xglobal*xglobal + yglobal*yglobal + zglobal*zglobal);
    theta = DEGRAD*acos(zglobal/mctfwdis);
    phi = DEGRAD*atan2(yglobal, xglobal);
    if( phi < -90.0 )
      phi = 360.0 + phi;

    evt_ntpl[14] = theta; // lab frame polar angle at entrance to TOF-West
    evt_ntpl[15] = phi;   // lab frame azimuthal angle at entrance to TOF-West

    evt_ntpl[16] = tfwghit[iRow].GetDedx();  // energy loss in passing through TOF-West

    evt_ntpl[17] = ptotpri; // total momentum of primary particle origin of this particle
    evt_ntpl[18] = pthetpri; // polar angle direction of the momentum of primary particle origin of this particle
    evt_ntpl[19] = pphipri;  // azimuthal angle direction of the momentum of primary particle origin of this particle
    evt_ntpl[20] = z0vertex; // z0 vertex of this primary particle (should be the same as the event z0)
    evt_ntpl[21] = evttrack; // track number in input event file

    evt_ntpl[22] = xglobal;
    evt_ntpl[23] = tfwghit[iRow].GetXin();  // local (detector frame) x position at entrance to TOF-West
    evt_ntpl[24] = tfwghit[iRow].GetXout(); // local (detector frame) x position at exit from TOF-West
    evt_ntpl[25] = yglobal;
    evt_ntpl[26] = tfwghit[iRow].GetYin();  // local (detector frame) y position at entrance to TOF-West
    evt_ntpl[27] = tfwghit[iRow].GetYout(); // local (detector frame) y position at exit from TOF-West
    evt_ntpl[28] = zglobal;
    evt_ntpl[29] = tfwghit[iRow].GetZin();  // local (detector frame) z position at entrance to TOF-West
    evt_ntpl[30] = tfwghit[iRow].GetZout(); // local (detector frame) z position at exit from TOF-West

    evt_ntpl[31] = itorigin;
    evt_ntpl[32] = idorigin;

    evt_ntpl[33] = tfwghit[iRow].GetPanel();

    evt_ntpl[34] = tfwghit[iRow].GetTof();  // time of flight in ns to entracnce of TOF-West
    evt_ntpl[35] = tfwghit[iRow].GetPathLength(); // path length in cm to entrance of TOF-West
    //
    // compute Beta (= v/c) for this particle if the TOF is greater than 0
    //
    if(evt_ntpl[34]>0.0)
      evt_ntpl[36] = evt_ntpl[35]/(evt_ntpl[34]*29.9792458);
    else
      evt_ntpl[36] = -9999.0;

    AncTfwNtuple->Fill(evt_ntpl);

    int  kStore = -1;
    bool newStore = false;
    if(countStore==0) {
      storeTrack[0] = true_track;
      newStore  = true;
    }
    else {
      //
      // check if track has previously been stored
      //
      newStore = true;
      for (kStore=0; kStore<countStore; kStore++) {
	if(true_track == storeTrack[kStore]) {
	  //
	  // previously stored track
	  // add to energy loss, tof
	  // count number of hits
	  //
	  newStore = false;
	  break;
	} // check if track matches previously stored track
      } // loop over previously stored tracks
    } // check on countStore

    if(newStore) {
      //
      // new track
      //
      storeTrack[countStore] = true_track;
      tfwArray[countStore].countTrack = 1;
      tfwArray[countStore].track = true_track;
      tfwArray[countStore].nfile = nfile;
      tfwArray[countStore].ptot = ptot;
      tfwArray[countStore].ptheta = ptheta;
      tfwArray[countStore].pphi = pphi;
      tfwArray[countStore].r_vertex = r_vertex;
      tfwArray[countStore].z_vertex = z_vertex;
      tfwArray[countStore].theta_vertex = theta_vertex;
      tfwArray[countStore].phi_vertex = phi_vertex;
      tfwArray[countStore].itparent = itparent;
      tfwArray[countStore].idparent = idparent;
      tfwArray[countStore].idpart = idpart;
      tfwArray[countStore].ihit = iRow+1;
      tfwArray[countStore].xglobal =  tfwghit[iRow].GetXing(); // global (lab frame) x position at entrance to TOF-West
      tfwArray[countStore].yglobal =  tfwghit[iRow].GetYing(); // global (lab frame) y position at entrance to TOF-West
      tfwArray[countStore].zglobal =  tfwghit[iRow].GetZing(); // global (lab frame) z position at entrance to TOF-West
      tfwArray[countStore].theta = theta;
      tfwArray[countStore].phi = phi;
      tfwArray[countStore].dele = tfwghit[iRow].GetDedx();  // energy loss in passing through TOF-West
      tfwArray[countStore].ptotpri = ptotpri;
      tfwArray[countStore].pthetpri = pthetpri;
      tfwArray[countStore].pphipri = pphipri;
      tfwArray[countStore].z0vertex = z0vertex;
      tfwArray[countStore].evttrack = evttrack;
      tfwArray[countStore].xlocal1 = tfwghit[iRow].GetXin();  // local (detector frame) x position at entrance to TOF-West
      tfwArray[countStore].xlocal2 = tfwghit[iRow].GetXout(); // local (detector frame) x position at exit from TOF-West
      tfwArray[countStore].ylocal1 = tfwghit[iRow].GetYin();  // local (detector frame) y position at entrance to TOF-West
      tfwArray[countStore].ylocal2 = tfwghit[iRow].GetYout(); // local (detector frame) y position at exit from TOF-West
      tfwArray[countStore].zlocal1 = tfwghit[iRow].GetZin();  // local (detector frame) z position at entrance to TOF-West
      tfwArray[countStore].zlocal2 = tfwghit[iRow].GetZout(); // local (detector frame) z position at exit from TOF-West
      tfwArray[countStore].itorigin = itorigin;
      tfwArray[countStore].idorigin = idorigin;
      tfwArray[countStore].panel = tfwghit[iRow].GetPanel();
      tfwArray[countStore].tof = tfwghit[iRow].GetTof();  // time of flight in ns to entracnce of TOF-West
      tfwArray[countStore].pathLength = tfwghit[iRow].GetPathLength(); // path length in cm to entrance of TOF-West
      tfwArray[countStore].beta = evt_ntpl[36];
      countStore++;
    }
    else {
      //
      // previously stored track
      // add to energy loss, tof
      // count number of hits
      //
      if(kStore<0 || kStore>=countStore) {
	cout << "\n Program error kStore = " << kStore << endl;
	exit(1);
      } // safety check
      tfwArray[kStore].countTrack++;
      tfwArray[kStore].dele += tfwghit[iRow].GetDedx();  // energy loss in passing through TOF-West
      tfwArray[kStore].tof += tfwghit[iRow].GetTof();  // time of flight in ns to entracnce of TOF-West
    } // check on newStore

  } // loop over stored hits

  //
  // Fill correlation NTUPLE
  //

  PadPISAHit *pcghit = PadPISAHit::GetPadHitEvt();
  Int_t pcRows = PadPISAHit::GetPadCount();

  TofPISAHit *tofghit = TofPISAHit::GetTofHitEvt();
  Int_t tofRows = TofPISAHit::GetTofCount();

  Int_t aerRows = AerPISAHit::GetAerCount();
  AerPISAHit *aerghit = AerPISAHit::GetAerHitEvt();

  for(int kCount=0; kCount<countStore; kCount++) {
    int tfwTrack = tfwArray[kCount].track;
    int tfwParent = tfwArray[kCount].itparent;
    evt_ntpl2[0] = float(tfwArray[kCount].track);
    evt_ntpl2[1] = float(tfwArray[kCount].nfile);
    evt_ntpl2[2] = tfwArray[kCount].ptot;
    evt_ntpl2[3] = tfwArray[kCount].ptheta;
    evt_ntpl2[4] = tfwArray[kCount].pphi;
    evt_ntpl2[5] = tfwArray[kCount].r_vertex;
    evt_ntpl2[6] = tfwArray[kCount].z_vertex;
    evt_ntpl2[7] = tfwArray[kCount].theta_vertex;
    evt_ntpl2[8] = tfwArray[kCount].phi_vertex;
    evt_ntpl2[9] = tfwArray[kCount].itparent;
    evt_ntpl2[10] = tfwArray[kCount].idparent;
    evt_ntpl2[11] = tfwArray[kCount].idpart;
    evt_ntpl2[12] = tfwArray[kCount].ihit;
    evt_ntpl2[14] = tfwArray[kCount].theta;
    evt_ntpl2[15] = tfwArray[kCount].phi;
    evt_ntpl2[16] = tfwArray[kCount].dele;
    evt_ntpl2[17] = tfwArray[kCount].ptotpri;
    evt_ntpl2[18] = tfwArray[kCount].pthetpri;
    evt_ntpl2[19] = tfwArray[kCount].pphipri;
    evt_ntpl2[20] = tfwArray[kCount].z0vertex;
    evt_ntpl2[21] = tfwArray[kCount].evttrack;
    evt_ntpl2[22] = tfwArray[kCount].xglobal;
    evt_ntpl2[23] = tfwArray[kCount].xlocal1;
    evt_ntpl2[24] = tfwArray[kCount].xlocal2;
    evt_ntpl2[25] = tfwArray[kCount].yglobal;
    evt_ntpl2[26] = tfwArray[kCount].ylocal1;
    evt_ntpl2[27] = tfwArray[kCount].ylocal2;
    evt_ntpl2[28] = tfwArray[kCount].zglobal;
    evt_ntpl2[29] = tfwArray[kCount].zlocal1;
    evt_ntpl2[30] = tfwArray[kCount].zlocal2;
    evt_ntpl2[31] = tfwArray[kCount].itorigin;
    evt_ntpl2[32] = tfwArray[kCount].idorigin;
    evt_ntpl2[33] = tfwArray[kCount].panel;
    evt_ntpl2[34] = tfwArray[kCount].tof/tfwArray[kCount].countTrack;  // average TOF for all hits of this track number
    evt_ntpl2[35] = tfwArray[kCount].pathLength;
    evt_ntpl2[36] = tfwArray[kCount].beta;
    evt_ntpl2[37] = tfwArray[kCount].countTrack;

    for(int index=38; index<63; index++) {
      evt_ntpl2[index] = -999.0; // default value
    }

    //
    // search for same track in PC2 West or PC3 West
    // store sector number if matching track is found
    //
    // also search if the parent track numbers are the same in PC3
    // store pc3Row and particle momentum
    //
    float pc1SectorSame = -999.0;
    float pc2SectorSame = -999.0;
    float pc3SectorSame = -999.0;
    float pc3PtotSister = -999.0;
    float pc3PthetaSister = -999.0;
    float pc3PphiSister = -999.0;
    int pc1Row = -1;
    int pc1PartSister = -999;
    int pc2Row = -2;
    int pc2PartSister = -999;
    int pc3Row = -1;
    int pc3PartSister = -999;
    for (int iRow = 0; iRow < pcRows ; iRow++) {
      true_track = pcghit[iRow].GetMctrack();
      int iPC = pcghit[iRow].GetIpc();
      if(tfwTrack == true_track) {
	//
	// should put in a safety check that the xGlobal is positive (West Arm)
	//
	if(iPC == 1)
	  pc1SectorSame = pcghit[iRow].GetSector();
	if(iPC == 2)
	  pc2SectorSame = pcghit[iRow].GetSector();
	if(iPC == 3)
	  pc3SectorSame = pcghit[iRow].GetSector();
      } // check on matching track number
      else {
	if(iPC == 1) {
	  //
	  // check for matching parent track in PC1
	  //
	  dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
			&r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
			&itparent, &idparent, &idpart);
	  if(tfwParent == itparent) {
	    pc1Row = iRow;
	    pc1PartSister = idpart;
	  }
	} // matching parent in PC1
	if(iPC == 2) {
	  //
	  // check for matching parent track in PC2
	  //
	  dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
			&r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
			&itparent, &idparent, &idpart);
	  if(tfwParent == itparent) {
	    pc2Row = iRow;
	    pc2PartSister = idpart;
	  }
	} // matching parent in PC2
	if(iPC == 3) {
	  //
	  // check for matching parent track in PC3
	  //
	  dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
			&r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
			&itparent, &idparent, &idpart);
	  if(tfwParent == itparent) {
	    pc3Row = iRow;
	    pc3PartSister = idpart;
	    pc3PtotSister = ptot;
	    pc3PthetaSister = ptheta;
	    pc3PphiSister = pphi;
	  } // same parent track in PC3
	} // check for PC3
      } // parent track check in PC3

    } // loop over all PC hits

    //
    // sector for same track in PC2 West
    //
    evt_ntpl2[38] = pc2SectorSame;

    //
    // search for same track or sister particle in aerogel
    //
    for (int iRow = 0; iRow < aerRows ; iRow++) {

      true_track = aerghit[iRow].GetMctrack();

      if(tfwTrack == true_track) {
	float px = aerghit[iRow].GetPx();
	float py = aerghit[iRow].GetPy();
	float pz = aerghit[iRow].GetPz();
	evt_ntpl2[39] = sqrt(px*px + py*py + pz*pz); // momentum in aerogel
      }
      else {
	//
	// check parent track
	//
	dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		      &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		      &itparent, &idparent, &idpart);
	if(tfwParent == itparent) {
	  xglobal = aerghit[iRow].GetX();
	  yglobal = aerghit[iRow].GetY();
	  phi = DEGRAD*atan2(yglobal, xglobal);

	  if( phi < -90.0 )
	    phi = 360.0 + phi;

	  evt_ntpl2[62] = phi;
	}
      } // check track number in aerogel
    } // loop over aerogel rows

    //
    // sector for same track in PC3 West
    //
    evt_ntpl2[40] = pc3SectorSame;

    //
    // search for track with same parent in PC3 East or West
    //
    if(pc3Row > -1) {
      evt_ntpl2[41] = pcghit[pc3Row].GetSector();
      xglobal = pcghit[pc3Row].GetXing();
      yglobal = pcghit[pc3Row].GetYing();
      zglobal = pcghit[pc3Row].GetZing();
      theta = DEGRAD*acos(zglobal/sqrt(xglobal*xglobal +
				       yglobal*yglobal + zglobal*zglobal));
      phi = DEGRAD*atan2(yglobal, xglobal);

      if(phi < -90.0)
        phi = 360.0 + phi;

      evt_ntpl2[42] = theta;
      evt_ntpl2[43] = phi;
      evt_ntpl2[44] = pcghit[pc3Row].GetTof();
      evt_ntpl2[45] = pc3PartSister;
      evt_ntpl2[46] = pc3PtotSister;
      evt_ntpl2[47] = pc3PthetaSister;
      evt_ntpl2[48] = pc3PphiSister;
    }
    
    //
    // search for sister particle in TOF East
    //
    for (int iRow = 0; iRow < tofRows ; iRow++) {

      true_track = tofghit[iRow].GetMctrack();
      if(true_track < 1)
	continue ;  // this is an error condition

      dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		    &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		    &itparent, &idparent, &idpart);
      if(tfwParent == itparent) {
	evt_ntpl2[49] = tofghit[iRow].GetTof();
	xglobal = tofghit[iRow].GetXm();
	yglobal = tofghit[iRow].GetYm();
	zglobal = tofghit[iRow].GetZm();
	mctofdis = sqrt(xglobal*xglobal + yglobal*yglobal + zglobal*zglobal);
	theta = DEGRAD*acos(zglobal/mctofdis);
	phi = DEGRAD*atan2(yglobal, xglobal);
	if( phi < -90.0 )
	  phi = 360.0 + phi;
	evt_ntpl2[50] = theta;
	evt_ntpl2[51] = phi;
	evt_ntpl2[52] = idpart;
	evt_ntpl2[53] = ptot;
	evt_ntpl2[54] = ptheta;
	evt_ntpl2[55] = pphi;
	break;
      } // check if same parent track number

    } // loop over TOF-East rows

    //
    // variables for same or sister in PC1 or PC2
    //
    evt_ntpl2[56] = pc1SectorSame;
    if(pc1Row > -1) {
      evt_ntpl2[57] =  pcghit[pc1Row].GetSector(); // sister particle in PC1
      evt_ntpl2[58] =  pc1PartSister; // sister particle ID in PC1
    }
    if(pc2Row > -1) {
      evt_ntpl2[59] =  pcghit[pc2Row].GetSector(); // sister particle in PC2
      evt_ntpl2[60] =  pc2PartSister; // sister particle ID in PC2
    }
    //
    // search for sister track in TOF-West
    //
    for (int iRow = 0; iRow < tfwRows ; iRow++) {

      true_track = tfwghit[iRow].GetMctrack();
      if(true_track < 1)
	continue ;  // this is an error condition

      if(true_track != tfwTrack) {
	dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		      &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		      &itparent, &idparent, &idpart);
	if(tfwParent == itparent) {
	  xglobal = tfwghit[iRow].GetXing(); // global (lab frame) x position at entrance to TOF-West
	  yglobal = tfwghit[iRow].GetYing(); // global y position at entrance to TOF-West

	  phi = DEGRAD*atan2(yglobal, xglobal);
	  if( phi < -90.0 )
	    phi = 360.0 + phi;

	  evt_ntpl2[61] = phi;   // lab frame azimuthal angle at entrance to TOF-West

	  break;
	} // check for same parent track

      } //  check for different track

    } // loop over TOF-West rows

    AncTfwNtuple2->Fill(evt_ntpl2);

  } // loop over stored tracks

  if(tfwArray) {
    delete [] tfwArray;
    delete [] storeTrack;
  }

  return;
}

