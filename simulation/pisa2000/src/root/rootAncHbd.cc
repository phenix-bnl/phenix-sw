#include <iostream>
#include <cstdlib>
#include "rootAnc.h"
#include "PISAEventHeader.h"
#include "HbdPISAHit.h"

void rootAncHbd(Int_t ancflag, PISAEventHeader *EventHeader){ 

  static int icall = 0;
  static TFile *AncHbdFile = 0;
  static TNtuple *AncHbdNtuple = 0;

  if(ancflag == 1) {
    
    //
    // Check that the file has been opened
    //
    if(icall == 0 || AncHbdFile == 0){
      std::cerr << "\n rootAncHbd <E> bad call with ancflag = 1" << std::endl;
      std::exit(1);
    }
    AncHbdFile->Write();
    AncHbdFile->Close();
    return;
  }

  //
  // 35 ntuple entries
  //
  const int NTPL_PARAM = 35;
  float evt_ntpl[NTPL_PARAM];
  if(icall == 0){

    //
    // NTUPLE variables
    //
    AncHbdFile = new TFile("anchbd.root", "recreate", "HBD PISA NTUPLE");
    AncHbdNtuple = new TNtuple("AncHbd", "HBD Ancestors",
                               "TRACK:NFILE:PTOT:PTHETA:PPHI:"//
			       "R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"//
			       "ITPARENT:IDPARENT:IDPART:ITORIGIN:IDORIGIN:"//
			       "IHIT:THETAIN:PHIIN:"//
			       "XIN:YIN:ZIN:PX:PY:PZ:"//
                               "TOF:THETAOUT:PHIOUT:"//
                               "XOUT:YOUT:ZOUT:"//
			       "SECTOR:"//
			       "DETFLAG:"//
			       "NHIT:Z0_EVENT:B_IMPACT:EVENT");

  }
  icall++;

  int hbdRows;

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

  //
  // Add separate entrance and std::exit values for HBD
  //
  float thetain, thetaout;
  float phiin, phiout;
  float xin, yin, zin, xout, yout, zout;
  //
  // Add hit-by-hit particle ID, since Cherenkov photons have the same track # as parent but different ID
  //
  int pid;
  const float DEGRAD = 57.295779513;

  int iRow;

  evt_ntpl[NTPL_PARAM - 1] = icall;
  evt_ntpl[NTPL_PARAM - 2] = EventHeader->GetImpactParameter();
  evt_ntpl[NTPL_PARAM - 3] = EventHeader->GetZvertex();

  hbdRows = HbdPISAHit::GetHbdCount();
  HbdPISAHit *hbdghit = HbdPISAHit::GetHbdHitEvt();

  evt_ntpl[NTPL_PARAM - 4] = hbdRows;
  for (iRow = 0; iRow < hbdRows ; iRow++) {

    true_track = hbdghit[iRow].GetMctrack();
    if(true_track < 1)
      continue;

    dio_ptrkorigin(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		  &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		  &itorigin, &idorigin, &idpart);
    //
    // Note: idpart above is primary particle id (=idorigin), not the input particle id.
    //
      
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

    //
    // idpart is NOT unique to the track number when there are Cherenkov photons, so retrieve idpart hit by hit
    //
    pid = hbdghit[iRow].GetIdPart();
    evt_ntpl[11] = pid;
    evt_ntpl[12] = float(itorigin);
    evt_ntpl[13] = float(idorigin);
    evt_ntpl[14] = iRow;
    xin = hbdghit[iRow].GetXin();
    yin = hbdghit[iRow].GetYin();
    zin = hbdghit[iRow].GetZin();

    thetain = DEGRAD*acos(zin/sqrt(xin*xin +
                        yin*yin + zin*zin));
    phiin = DEGRAD*atan2(yin, xin);
    if( phiin < -90.0 )
      phiin = 360.0 + phiin;
    evt_ntpl[15] = thetain;
    evt_ntpl[16] = phiin;

    evt_ntpl[17] = xin;
    evt_ntpl[18] = yin;
    evt_ntpl[19] = zin;
    evt_ntpl[20] = hbdghit[iRow].GetPx();
    evt_ntpl[21] = hbdghit[iRow].GetPy();
    evt_ntpl[22] = hbdghit[iRow].GetPz();
    evt_ntpl[23] = hbdghit[iRow].GetTOF();

    //
    // Add the std::exit values of xyz and theta, phi at the end rather than changing
    //all the indices above
    //
    xout = hbdghit[iRow].GetXout();
    yout = hbdghit[iRow].GetYout();
    zout = hbdghit[iRow].GetZout();

    //
    // Exit values aren't meaningful for thin CsI, so set them to dummy -999.   
    // All other non-sensical values for HBD gas or CsI were set to -999 in hbd_digi.f
    //
    if(hbdghit[iRow].GetDetFlag()==3) {  
      thetaout = -999;
      phiout   = -999;
    }
    else {
      thetaout = DEGRAD*acos(zout/sqrt(xout*xout +
                        yout*yout + zout*zout));
      phiout = DEGRAD*atan2(yout, xout);
      if( phiout < -90.0 )
	phiout = 360.0 + phiout;
    }
    evt_ntpl[24] = thetaout;
    evt_ntpl[25] = phiout;
    evt_ntpl[26] = xout;   
    evt_ntpl[27] = yout;
    evt_ntpl[28] = zout;

    //
    // End addition of std::exit values
    // Add detector, sector, and pad row numbers
    //
    evt_ntpl[29] = hbdghit[iRow].GetSect();

    //
    // Detector flag = 2 for HBD gas, 3 for HBD CsI
    //
    evt_ntpl[30] = hbdghit[iRow].GetDetFlag();

    AncHbdNtuple->Fill(evt_ntpl);

  }  // loop over iRow

  return;
}
