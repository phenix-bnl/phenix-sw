// $Id: pisaRootOSCAR.cc,v 1.13 2007/03/02 10:11:33 hpereira Exp $

/*!
  \file   pisarRootOSCAR.cc
  \brief  produce pisa to oscar ntuple
  \author Charlie Maguire
  \version $Revision: 1.13 $
  \date $Date: 2007/03/02 10:11:33 $
*/

#include <iostream>
#include <cstdlib>

#include <TROOT.h>
#include <TFile.h>
#include <TTree.h>
#include <TBranch.h>
#include <TNtuple.h>

#include "PISARun.h"
#include "PISAEvent.h"

#include <geantMass.h>
#include <pisaMass.h>
#include <geantToOSCAR.h>

#include <cmath>

using namespace std;

//________________________________________
//! main
int main(int argc, char** argv)
{
  try {
  
	  cout << "usage pisaRootOSCAR [n_events] [z0 (for runs < 1000)]" << endl;
	  
	  // conversion of cm to fm
	  const float CMTOFM = 1.0e+13;
	
	  // Initialize the ROOT system
	  TROOT simple("", "");
	
	  // variable to control how many events are read
	  // set event limit from first argument, if there is one
	  Int_t ilimit = -1;          
	  if(argc > 1) ilimit = atoi(argv[1]);  
	
	  Int_t fixZ0 = 0;
	  if(argc > 2) fixZ0 = atoi(argv[2]);
	
	  //  Connect to input file
	  TFile f("PISAEvent.root");
	
	  //   Read Tree named "T" in memory. Tree pointer is assigned the same name
	  TTree *T = (TTree*)f.Get("T");  
	
	  TFile * hfile = new TFile("oscar.root","RECREATE","ROOT file");
	
	  TNtuple *particle = new TNtuple(
	    "particle","primary particle ntuple",
	    "event:pnum:pid:px:py:pz:E:mass:xvtx:yvtx:zvtx:theta:phi:rap:eta");
	
	  int pid(0);
	  float  px(0), py(0), pz(0), E(0), mass(0), xvtx(0), yvtx(0), zvtx(0);
	  float theta, phi, rap, eta, mom;
	
	  //   Start main loop on all events
	  if(T == 0) {
	    cerr << "pisaRootOSCAR - Bad TTree " << endl;
	    exit(1);
	  }
	  
    // set branch
    PISAEvent* pisaevent = new PISAEvent();
	  T->SetBranchAddress("pisaevent",&pisaevent);
	 
	  int nevent = Int_t(T->GetEntries());
	  cout << "pisaRootOSCAR - Running PISA-to-OSCAR NTUPLE producer";
	  cout << "pisaRootOSCAR - Number of events requested = ";
	  if(ilimit == -1) cout << "ALL";
	  else cout << ilimit;
	
	  cout << ",  Number events actually in the file = " << nevent << endl;
	
	  // update events from limit and file size
	  Int_t readEvents = ilimit;
	  if(readEvents > nevent || ilimit < 0) readEvents = nevent;
	
	  Int_t nBytes = 0;
	  for (Int_t kevent=0; kevent< readEvents; kevent++) 
	  {
	
	    nBytes += T->GetEntry(kevent);
	
	    PISAEventHeader *EventHeader = pisaevent->GetHeader();
	    Int_t inputRunNumber = EventHeader->GetInputRunNumber();
	    Int_t outputRunNumber = EventHeader->GetOutputRunNumber();
	    if(outputRunNumber>1000&&outputRunNumber<1800)
	      fixZ0 = 1;
	    Int_t projectNumber = EventHeader->GetProjectNumber();
	    Int_t versionNumber = EventHeader->GetVersionNumber();
	
	    if(kevent==0) {
	      cout << "pisaRootOSCAR - PISA file contains event file inputRunNumber = " <<  inputRunNumber << endl;
	      cout << "   hits file outputRunNumber = " <<  outputRunNumber << endl;
	      cout << "   simulation projectNumber = " <<  projectNumber << endl;
	      cout << "   software versionNumber = " <<  versionNumber << endl << endl;
	    } 
	 
	    xvtx = CMTOFM*EventHeader->GetXvertex();
	    yvtx = CMTOFM*EventHeader->GetYvertex();
	    zvtx = CMTOFM*EventHeader->GetZvertex();
	
	    // kinetic information
	    static Int_t iFirst = 1;
	    Int_t KinNhit = pisaevent->GetKinNhit();
	    if(fixZ0>0 && KinNhit>0) 
	    {
	      for (Int_t khit=0; khit<KinNhit; khit++)
	      {
	        KinPISAHit* kinhit = (KinPISAHit*)pisaevent->GetKinHits()->At(khit);
	        if(kinhit->GetIdparent()==0) 
	        {
	          if(fabs(kinhit->GetZvertex() - zvtx) > 0.001) {
	            xvtx = 0.0;
	            yvtx = 0.0;
	            zvtx = kinhit->GetZvertex();
	            if(iFirst == 1) {
	              cout << "\n  checkZ0Event was successful" << endl;
	              iFirst = 0;
	            }  
	          }  
	        }  
	      }
	    } 
	
	    Int_t PriNhit = pisaevent->GetPriNhit();
	
	    // Retrieve Pri hits information
	    for (Int_t khit=0; khit<PriNhit; khit++)
	    {
	
	      PriPISAHit* prihit = (PriPISAHit*)pisaevent->GetPriHits()->At(khit);
	      int idpart = prihit->GetIdpart();
	      px = prihit->GetPx();
	      py = prihit->GetPy();
	      pz = prihit->GetPz();
	
	      phi = atan2(py,px);
	      if ( py<0.0 ) phi=phi+2.0*M_PI;
	      mom = sqrt(px*px+py*py+pz*pz);
	      
	      if ( mom!= 0.0 ) theta = acos(pz/mom);
	      else theta = 0.0;
	      
	      eta = -log(tan(theta/2.0));
	
	      if(idpart-1 < 56) {
	        mass = geantMass[idpart-1];
	        pid = geantOSCAR1[idpart-1];
	      } else {
	        cerr << "\n idpart is too large " << idpart << endl;
	        exit(1);
	      }
	
	      E = sqrt(px*px + py*py + pz*pz + mass*mass);
	
	      if(E != pz) rap = 0.5*log((E + pz)/(E - pz));
	      else rap = 0.0;
	
	      particle->Fill(kevent+1,PriNhit,pid,px,py,pz,E,mass,xvtx,yvtx,zvtx,
		     theta,phi,rap,eta);
	
	    } // loop over entries in this event
	
	  } // loop over all entries in the file
	
	  f.Close();
	
	  hfile->Write();
	  hfile->Close();
	  hfile = 0;
	  
	} catch (exception &e ) { cout << "pisaRootOSCAR - " << e.what() << endl; }
  
  cout << "pisaRootOSCAR - successfully completed" << endl;

  return 0;

}
