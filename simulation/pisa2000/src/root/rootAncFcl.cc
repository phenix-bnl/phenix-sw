#include <iostream>
#include <cstdlib>
#include "rootAnc.h"
#include "PISAEventHeader.h"
#include "PadPISAHit.h"
#include "FclPISAHit.h"

void rootAncFcl(Int_t ancflag, PISAEventHeader *EventHeader)
{

  static int icall = 0;
  static TFile *AncFclFile = 0;
  static TNtuple *AncFclNtuple = 0;

  if(ancflag == 1) {
    //
    // Check that the file has been opened
    //
    if(icall == 0 || AncFclFile == 0){
      std::cerr << "\n rootAncFcl <E> bad call with ancflag = 1" << std::endl;
      std::exit(1);
    }  // safety check
    AncFclFile->Write();
    AncFclFile->Close();
    return;
  }  // check for close output file flag

  const int NTPL_PARAM = 42;
  float evt_ntpl[NTPL_PARAM];
  if(icall == 0){
    //
    // NTUPLE files
    //
    AncFclFile = new TFile("ancfcl.root", "recreate", "FCL PISA NTUPLE");
    AncFclNtuple = new TNtuple("AncFcl", "FCL Ancestors",
                               "TRACK:NFILE:PTOT:PTHETA:PPHI:"//
			       "R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"//
			       "ITPARENT:IDPARENT:IDPART:ITORIGIN:IDORIGIN:"//
			       "IHIT:LAYER:THETA:PHI:"//
			       "XGLOBAL:YGLOBAL:ZGLOBAL:PMOMX:PMOMY:PMOMZ:"//
			       "IPC123:PC1THETA:PC1PHI:PC1RDIST:PC1ZDIST:"//
                               "PC2THETA:PC2PHI:PC2RDIST:PC2ZDIST:"//
			       "PC3THETA:PC3PHI:PC3RDIST:PC3ZDIST:"//
			       "DELE:"//
			       "NHIT:Z0_EVENT:B_IMPACT:EVENT");

  }  // initialization
  icall++;

  int fclRows;

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

  evt_ntpl[NTPL_PARAM - 1] = icall;
  evt_ntpl[NTPL_PARAM - 2] = EventHeader->GetNptls();
  evt_ntpl[NTPL_PARAM - 3] = EventHeader->GetZvertex();

  fclRows = FclPISAHit::GetFclCount();
  FclPISAHit *fclghit = FclPISAHit::GetFclHitEvt();

  evt_ntpl[NTPL_PARAM - 4] = fclRows;
  for (iRow = 0; iRow < fclRows ; iRow++) {

    true_track = fclghit[iRow].GetMctrack();
    if(true_track<1)
      continue ;  // this is an error condition
    else
    
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
    evt_ntpl[15] = fclghit[iRow].GetLayer();

    xglobal = fclghit[iRow].GetX();
    yglobal = fclghit[iRow].GetY();
    zglobal = fclghit[iRow].GetZ();

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
    evt_ntpl[21] = fclghit[iRow].GetPx();
    evt_ntpl[22] = fclghit[iRow].GetPy();
    evt_ntpl[23] = fclghit[iRow].GetPz();

    for(Int_t iFill=25; iFill<37; iFill++) {
      evt_ntpl[iFill] = -9999.0;
    } // initialize PC1/PC2/PC3 correlation to no match condition

    evt_ntpl[37] = 1000.0*fclghit[iRow].GetDele();  // store in units of MeV
    evt_ntpl[40] = EventHeader->GetNptls();

    AncFclNtuple->Fill(evt_ntpl);
    
  }  // loop over iRow

  return;
}
