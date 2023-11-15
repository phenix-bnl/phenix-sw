#include <iostream>
#include "PISAEvent.h"
#include "TfwPISAHit.h"
#include <cstdlib>

void encodeRootEvntTfw(int ipanel, int kentries, int iData[], float fData[], 
		       int isubevent, PISAEvent *pisaevent)
{

  //
  // kentries is the number of Tfw hits in this subevent
  // for the last subevent, it is possible that kentries = 0
  //

  //
  //  Save the PISAEvent startflag
  //     startflag = 0  means subevent output of ROOT clone objects
  //     startflag = 1  means full event output of ROOT clone objects
  //     startflag = 2  means full event output of PHOOL objects (method not yet defined)
  //
  static Int_t startflag = pisaevent->GetStartFlag();

  if(startflag==2) {
    std::cerr << "\n\n  encodeRootEventTfw <F>: startflag = " << startflag;
    std::cerr << ",  method not yet defined" << std::endl << std::endl;
    exit(1);
  }

  //
  // Save the TFW status flag
  //
  static Int_t TfwStatus = 0;  // 0 indicates start of new event

  //
  // Save the value of the local variable nsubevent, and the local variable TfwCount
  //
  static Int_t nsubevent = 0;  // nsubevent is set when TfwStatus = 0
  static Int_t TfwCount = 0;   // TfwCount is initialized when TfwStatus = 0

  //
  // kentries is the number of TFW hits in this subevent
  // for the last subevent, it is possible that kentries = 0
  //
  if(startflag == 2 && kentries == 0 && TfwStatus == 0){
    //
    // We should have no TfwCount in this state
    //
    if(TfwCount != 0){
      //
      // This is an error condition
      //
      std::cerr << "\n\n Error state in encodeRootEvntTfw";
      std::cerr << "\n TfwStatus = 0 and TfwCount = " << TfwCount << "\n";
      std::exit(1);
    } // check on whether there is any TfwCount
    return;  // nothing to do since there is no TfwCount in this event
  }
 

  Int_t mctrack = -1;  // Will change in Off-Line
  Int_t nfile = -1;    // Will change in Off-Line
  Int_t id    = -1;    // Will change in Off-Line

  //
  // Save the array of subevent pointers, and number of entries in each subevent
  // These are intialized for the first subevent and deleted after the last subevent
  //

  static TfwPISAHit **tfwHitSub = 0;
  static Int_t *tfwNhitSub = 0;
 
  if(startflag == 2 && TfwStatus == 0){
    TfwStatus = 1;  // indicate that the Tfw instances are being filled

    //
    // Initialization of subevent pointers
    // First get number of subevents in this event from Event Header
    //
    PISAEventHeader *pisaEventHeader = pisaevent->GetHeader();
    nsubevent = pisaEventHeader->GetNsubevent();
    
    //
    // Now allocate space for array of TfwPISAHit pointers and subevent counters
    //
    tfwHitSub = new TfwPISAHit *[nsubevent];
    tfwNhitSub = new int[nsubevent];

    //
    // Initialize these arrays (some subevents may have 0 entries)
    //
    for(Int_t mevent=0; mevent<nsubevent; mevent++){
      tfwHitSub[mevent] = 0;  // pointer set to 0
      tfwNhitSub[mevent] = 0; // counter set to 0
    } // safety set for pointers and counters

    //
    // Counter for total number of Tfw entries in the full event
    //
    TfwCount = 0;
  } // initialization check for first subevent of an event

  //
  // Allocate memory for kentries of TfwPISAHit instances
  // Save this memory start location in tfwHitSub array

  TfwPISAHit *tfwhit = 0;
  if(startflag == 2 && kentries>0){
    tfwhit =  new TfwPISAHit [kentries];
    tfwHitSub[isubevent - 1] = tfwhit;
    TfwCount += kentries;  // add the number of Tfwentries in this subevent
    tfwNhitSub[isubevent - 1] = kentries;  // save the number of entries
  } // check if the number of entries is greater than 0

  Int_t p = 0;

  Float_t xyzinloc[3];
  Float_t xyzoutloc[3];
  Float_t xyzinglo[3];

  //
  // The sequential order of these variables in set by the fpwlink.inc include file
  //
  for (int k=0; k<kentries; k++){
    Int_t track        = iData[p++];     // track in subevent
    Int_t panel       = iData[p++];     // panel
    xyzinloc[0]        = fData[p++];     // entrance local x value
    xyzinloc[1]        = fData[p++];     // entrance local y value
    xyzinloc[2]        = fData[p++];     // entrance local z value
    xyzoutloc[0]       = fData[p++];     // exit local x value
    xyzoutloc[1]       = fData[p++];     // exit local y value
    xyzoutloc[2]       = fData[p++];     // exit local z value
    Float_t tof        = fData[p++];     // time of flight at entrance
    Float_t pathLength = fData[p++];     // flight path length to entrance

    p++;  // Skip particle ID output (could do trtrno redundancy check)

    xyzinglo[0]        = fData[p++];     // entrance global x value
    xyzinglo[1]        = fData[p++];     // entrance global y value
    xyzinglo[2]        = fData[p++];     // entrance global z value
    Float_t dedx       = fData[p++];     // energy loss in detector

    //
    // Original subevent output, kept for future use
    //
    if(startflag == 0 || startflag == 1) {    
      pisaevent->AddTfwHit(xyzinloc, xyzoutloc, xyzinglo,
			   tof, dedx, pathLength, track, panel,
			   id, isubevent, mctrack, nfile );
   
    }  // output format is ROOT clone objects
 
  } // loop over entries

  return;
}
