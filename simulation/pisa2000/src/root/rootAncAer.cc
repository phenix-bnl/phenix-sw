#include <iostream>
#include <cstdlib>
#include "PISARun.h"
#include "PISAEventHeader.h"
#include "PadPISAHit.h"
#include "AerPISAHit.h"

//___________________________________________________________________
void PISARun::AncAer( const int& ancflag, PISAEvent::pointer pisa_event)
{

  static int icall = 0;
  static TFile *AncAerFile = 0;
  static TNtuple *AncAerNtuple = 0;

  PISAEventHeader *EventHeader = pisa_event->GetHeader();

  if(ancflag == 1) {
    //
    // Check that the file has been opened
    //
    if(icall == 0 || AncAerFile == 0){
      std::cerr << "\n rootAncAer <E> bad call with ancflag = 1" << std::endl;
      std::exit(1);
    }  // safety check
    AncAerFile->Write();
    AncAerFile->Close();
    return;
  }  // check for close output file flag

  const int NTPL_PARAM = 31;
  float evt_ntpl[NTPL_PARAM];
  if(icall == 0){
    //
    // NTUPLE files
    //
    AncAerFile = new TFile("ancaer.root", "recreate", "AER PISA NTUPLE");
    AncAerNtuple = new TNtuple("AncAer", "AER Ancestors",
                               "TRACK:NFILE:PTOT:PTHETA:PPHI:"//
			       "R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"//
			       "ITPARENT:IDPARENT:IDPART:ITORIGIN:IDORIGIN:"//
			       "IHIT:LAYER:THETA:PHI:"//
			       "X:Y:Z:PX:PY:PZ:"//
                               "PATHLENG:TOF:DELE:"//
			       "NHIT:Z0_EVENT:B_IMPACT:EVENT");

  }  // initialization
  icall++;

  int aerRows;

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
  float x, y, z;
  const float DEGRAD = 57.295779513;

  int iRow;

  evt_ntpl[NTPL_PARAM - 1] = icall;
  evt_ntpl[NTPL_PARAM - 2] = EventHeader->GetImpactParameter();
  evt_ntpl[NTPL_PARAM - 3] = EventHeader->GetZvertex();

  aerRows = AerPISAHit::GetAerCount();
  AerPISAHit *aerghit = AerPISAHit::GetAerHitEvt();

  evt_ntpl[NTPL_PARAM - 4] = aerRows;
  for (iRow = 0; iRow < aerRows ; iRow++) {

    true_track = aerghit[iRow].GetMctrack();
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
    evt_ntpl[15] = aerghit[iRow].GetLayer();

    x = aerghit[iRow].GetX();
    y = aerghit[iRow].GetY();
    z = aerghit[iRow].GetZ();

    theta = DEGRAD*acos(z/sqrt(x*x +
                        y*y + z*z));
    phi = DEGRAD*atan2(y, x);
    if( phi < -90.0 )
      phi = 360.0 + phi;
    evt_ntpl[16] = theta;
    evt_ntpl[17] = phi;

    evt_ntpl[18] = x;
    evt_ntpl[19] = y;
    evt_ntpl[20] = z;
    evt_ntpl[21] = aerghit[iRow].GetPx();
    evt_ntpl[22] = aerghit[iRow].GetPy();
    evt_ntpl[23] = aerghit[iRow].GetPz();
    evt_ntpl[24] = aerghit[iRow].GetPathLength();
    evt_ntpl[25] = aerghit[iRow].GetTOF();
    evt_ntpl[26] = 1000.0*aerghit[iRow].GetDele();  // store in units of MeV

    AncAerNtuple->Fill(evt_ntpl);

  }  // loop over iRow

  return;
}
