#include <iostream>
// T. K. Ghosh, Vanderbilt 08.09.99
// Followed by decoder routine in STAF


#include "PISAEvent.h"
#include <iostream>
using namespace std;

void encodeRootEvntMut(int kentries, int i[], float f[], int nStoredTracks, int tracks[],
                       int isubevent, PISAEvent *pisaevent)

{
  Int_t mctrack = -1;  // Will change in Off-Line
  Int_t nfile = -1;    // will change offline
  Int_t p = 1;

  bool stagedSimulation = false;
  if(nStoredTracks > 0)
    stagedSimulation = true;

  static int kPrint = 0;

  if(kPrint==0 && stagedSimulation) {
    kPrint++;
    cout << "\n\n   encodeRootEvntMut with stagedSimulation is true, nStoredTracks " << nStoredTracks << endl << endl;
  }

  for (int k=0; k<kentries; k++){
    
//    mumhits[i].track =  trtrno(iData[p++], iSubEvent);   OBSOLETE
    Int_t track = i[p++];
    bool foundTrack = true;
    if(stagedSimulation) {
      foundTrack = false;
      //
      // This hit will be stored only if it is in the ancestry history array tracks[]
      //
      for(int kTrack=0; kTrack<nStoredTracks; kTrack++) {
	if(track == tracks[kTrack]) {
	  foundTrack = true;
	  break;
	}
      } // loop over stored ancestor tracks
    } // check on staged simulation
    Int_t plane = i[p++];
    Short_t pid   = i[p++];
    if(!foundTrack) {
      if(kPrint < 0) { // Debug print if needed
	cout << "\n encodeRootEvntMut skipping MuTr track " << track << " in plane " << plane << " with pid " << pid << endl;
      }
    }
    else {
      if(stagedSimulation && kPrint < 0) { // Debug print if needed
	cout << "\n encodeRootEvntMut keeping MuTr track " << track << " in plane " << plane << " with pid " << pid << endl;
      }
    }
    Float_t t     = f[p++];
    Float_t e     = f[p++];
    Float_t x  = f[p++];
    Float_t y  = f[p++];
    Float_t z  = f[p++];
    Float_t px  = f[p++];
    Float_t py  = f[p++];
    Float_t pz  = f[p++];
    if(foundTrack) {
      pisaevent->AddMutHit(track, plane, pid, t, e, x, y, z, px, py, pz,
			 mctrack, nfile, isubevent);
    }

  }  //loop over all track entries

  if(stagedSimulation)
    kPrint++;

  return;
}
