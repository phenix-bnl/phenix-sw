#include <iostream>
#include "PISAEvent.h"
#include "RxnPISAHit.h"
#include <cstdlib>

void encodeRootEvntRxn(int icode, int kentries, int iData[], float fData[], 
		       int isubevent, PISAEvent *pisaevent)
{

  //
  // kentries is the number of Rxn hits in this subevent
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
    std::cerr << "\n\n  encodeRootEventRxn <F>: startflag = " << startflag;
    std::cerr << ",  method not yet defined" << std::endl << std::endl;
    exit(1);
  }

  //
  // Save the RXN status flag
  //
  static Int_t RxnStatus = 0;  // 0 indicates start of new event

  //
  // Save the value of the local variable nsubevent, and the local variable RxnCount
  //
  static Int_t nsubevent = 0;  // nsubevent is set when RxnStatus = 0
  static Int_t RxnCount = 0;   // RxnCount is initialized when RxnStatus = 0

  //
  // kentries is the number of RXN hits in this subevent
  // for the last subevent, it is possible that kentries = 0
  //
  if(startflag == 2 && kentries == 0 && RxnStatus == 0){
    //
    // We should have no RxnCount in this state
    //
    if(RxnCount != 0){
      //
      // This is an error condition
      //
      std::cerr << "\n\n Error state in encodeRootEvntRxn";
      std::cerr << "\n RxnStatus = 0 and RxnCount = " << RxnCount << "\n";
      std::exit(1);
    } // check on whether there is any RxnCount
    return;  // nothing to do since there is no RxnCount in this event
  }
 

  Int_t mctrack = -1;  // Will change in Off-Line
  Int_t nfile = -1;    // Will change in Off-Line
  Int_t id    = -1;    // Will change in Off-Line

  //
  // Save the array of subevent pointers, and number of entries in each subevent
  // These are intialized for the first subevent and deleted after the last subevent
  //

  static RxnPISAHit **rxnHitSub = 0;
  static Int_t *rxnNhitSub = 0;
 
  if(startflag == 2 && RxnStatus == 0){
    RxnStatus = 1;  // indicate that the Rxn instances are being filled

    //
    // Initialization of subevent pointers
    // First get number of subevents in this event from Event Header
    //
    PISAEventHeader *pisaEventHeader = pisaevent->GetHeader();
    nsubevent = pisaEventHeader->GetNsubevent();
    
    //
    // Now allocate space for array of RxnPISAHit pointers and subevent counters
    //
    rxnHitSub = new RxnPISAHit *[nsubevent];
    rxnNhitSub = new int[nsubevent];

    //
    // Initialize these arrays (some subevents may have 0 entries)
    //
    for(Int_t mevent=0; mevent<nsubevent; mevent++){
      rxnHitSub[mevent] = 0;  // pointer set to 0
      rxnNhitSub[mevent] = 0; // counter set to 0
    } // safety set for pointers and counters

    //
    // Counter for total number of Rxn entries in the full event
    //
    RxnCount = 0;
  } // initialization check for first subevent of an event

  //
  // Allocate memory for kentries of RxnPISAHit instances
  // Save this memory start location in rxnHitSub array

  RxnPISAHit *rxnhit = 0;
  if(startflag == 2 && kentries>0){
    rxnhit =  new RxnPISAHit [kentries];
    rxnHitSub[isubevent - 1] = rxnhit;
    RxnCount += kentries;  // add the number of Rxnentries in this subevent
    rxnNhitSub[isubevent - 1] = kentries;  // save the number of entries
  } // check if the number of entries is greater than 0

  Int_t p = 0;

  Float_t xyzinloc[3];
  Float_t xyzoutloc[3];
  Float_t xyzinglo[3];
  Float_t pmomxyz[3];

  //
  // The sequential order of these variables in set by the fpwlink.inc include file
  //
  for (int k=0; k<kentries; k++){
    Int_t track        = iData[p++];     // track in subevent
    Int_t arm          = iData[p++];     // arm
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
    pmomxyz[0]         = fData[p++];     // x component of momentum
    pmomxyz[1]         = fData[p++];     // y component of momentum
    pmomxyz[2]         = fData[p++];     // z component of momentum

    //
    // Original subevent output, kept for future use
    //
    if(startflag == 0 || startflag == 1) {    
      pisaevent->AddRxnHit(xyzinloc, xyzoutloc, xyzinglo, pmomxyz,
			   tof, dedx, pathLength, track, arm,
			   id, isubevent, mctrack, nfile );
   
    }  // output format is ROOT clone objects
 
  } // loop over entries

  return;
}

