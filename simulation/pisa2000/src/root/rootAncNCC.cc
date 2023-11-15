#include <iostream>
#include <cstdlib>
#include "NCCPISAHit.h"
#include <fstream>

#include "PISARun.h"
#include "PISAEventHeader.h"

#include <TROOT.h>
#include <TFile.h>
#include <TTree.h>
#include <TBranch.h>
#include <TStopwatch.h>
#include <TDirectory.h>
#include <TRandom.h>
#include <TH1.h>
#include <TH2.h>
#include <TNtuple.h>

using namespace std;

//_____________________________________________________________________
void PISARun::AncNCC(const int& ancflag, PISAEvent::pointer pisaevent )
{

  // Event header information not yet used
  const int maxpart = 10000;

  int error;
  float ptot;
  float ptheta;
  float pphi;
  float r_vertex;
  float theta_vertex;
  float phi_vertex;
  int itparent;
  int idparent;
  int idpart;
  int itorigin;
  int idorigin;
  float ptotpri;
  float pthetpri;
  float pphipri;
  float z0vertex;

  int twrid;
  int sensid;
  int evnt;
  float z_vertex;
  float dedx, tofin;

  static int icall = 0;
  const int NTPL_PARAM = 17;
  float evt_ntpl[NTPL_PARAM];

  // Initialization
  static TFile *AncNCCFile = 0;
  static TNtuple *AncNCCNtuple = 0;

  if(ancflag == 1) 
  {
    // Check that the file has been opened
    if(icall == 0 || AncNCCFile == 0){
      cout << "PISARun::AncNCC - bad call with ancflag = 1" << endl;
      exit(1);
    }
    AncNCCFile->Write();
    AncNCCFile->Close();
    return;
  }

  if(icall == 0)
  {
    // NTUPLE files
    AncNCCFile = new TFile("ancncc.root", "recreate", "NCC PISA NTUPLE");
    AncNCCNtuple = new TNtuple(
      "AncNCC", "NCC Ancestors",
      "B_IMPACT:"
      "TWRID:SENSID:"
      "TOFIN:DEDX:"
      "EVENT:NHIT");

  }

  icall++;

  Int_t nccRows = NCCPISAHit::GetNCCCount();
  Int_t nccRow[2];
  nccRow[0]  = NCCPISAHit::GetNCC1Count();
  nccRow[1]  = NCCPISAHit::GetNCC2Count();

  NCCPISAHit *ncc1ghit = NCCPISAHit::GetNCC1HitEvt();
  NCCPISAHit *ncc2ghit = NCCPISAHit::GetNCC2HitEvt();

  for( int iPC = 1; iPC < 3; iPC++ )
  {
    if( nccRow[iPC-1] > maxpart) 
    {
      cout 
        << "PISARun::AncNCC -"
        << " too many hits: " << nccRow[iPC-1] << " for station " << iPC
        << " truncated to: " << maxpart << endl;
    }
    
  }  

  NCCPISAHit *nccghit = NCCPISAHit::GetNCCHitEvt();
  
  for( int iRow = 0; iRow < min( nccRows, maxpart ); iRow++) 
  {
    
    int iPC = nccghit[iRow].GetIncc();
    switch (iPC) {
      case 1:
      { 
        break; 
      }
      
      case 2:
      { 
        break;
      }
      
      default:
      { 
        cout << "PISARun::AncNCC - invalid PCx number " << endl;
        break;
      } 
      
    }
  } 

  for( int iPC = 1; iPC < 3 ; iPC++)
  {
    switch (iPC) 
    {
      case 1: {nccghit= ncc1ghit; nccRows = min( nccRow[iPC-1], maxpart ); break;}
      case 2: {nccghit= ncc2ghit; nccRows = min( nccRow[iPC-1], maxpart ); break;}
      default: break;
    }
    
    for ( int iRow = 0; iRow < nccRows ; iRow++) 
    {

      int true_track = nccghit[iRow].GetMctrack();
      int nfile = nccghit[iRow].GetNfile();
      int evttrack(0);
      
      // not working?
      dio_TrueTrackToEtrack(&true_track, &evttrack, &nfile);  
                  
      dio_ptrkorigin(
        &true_track, &nfile, &error, 
        &ptotpri, &pthetpri, &pphipri,
        &r_vertex, &z0vertex, &theta_vertex, &phi_vertex, 
        &itorigin, &idorigin, &idpart);

      dio_ptrkstack(
        &true_track, &nfile, &error, &ptot, &ptheta, &pphi,
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
	    std::cerr << "\n rootAncNCC <E> : evttrack invalid " << evttrack << ", itorigin = " << itorigin << " at event " << icall << std::endl;
	  } // check on valid evttrack number
	  
	} // check on valid origin track number
	else {
	  std::cerr << "\n rootAncNCC <E> : evttrack invalid " << evttrack << ", itorigin = " << itorigin << " at event " << icall << std::endl;
	}
      } // check on valid event track number

      evnt   = nccghit[iRow].GetNEvent();
      twrid  = nccghit[iRow].GetTWRID();
      sensid = nccghit[iRow].GetSENID();
      tofin  = nccghit[iRow].GetTOFIN();
      dedx   = nccghit[iRow].GetDedx();
      
      evt_ntpl[0] = pisaevent->GetHeader()->GetImpactParameter();
      evt_ntpl[1] = twrid;
      evt_ntpl[2] = sensid;
      evt_ntpl[3] = tofin;
      evt_ntpl[4] = dedx;
      evt_ntpl[5] = evnt;
      evt_ntpl[6] = nccRows;

      AncNCCNtuple->Fill(evt_ntpl);

    }

  }
  return;
}
