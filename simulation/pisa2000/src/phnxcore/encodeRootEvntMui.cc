#include <iostream>
// T. K. Ghosh, Vanderbilt 08.09.99
// Followed by decoder routine in STAF

#include "PISAEvent.h"
#include <iostream>
using namespace std;

void encodeRootEvntMui(int kentries, int i[], float f[], int nStoredTracks, int tracks[], 
                       int isubevent, PISAEvent *pisaevent)
{
  Int_t mctrack = -1;  // Will change in Off-Line
  Int_t nfile = -1;    // will change offline
  Int_t p = 1;         // special start for MuID
  Float_t rhit[3];
  Float_t phit[3];

  bool stagedSimulation = false;
  if(nStoredTracks > 0)
    stagedSimulation = true;

  static int kPrint = 0;

  if(kPrint==0 && stagedSimulation) {
    kPrint++;
    cout << "\n\n   encodeRootEvntMui with stagedSimulation is true, nStoredTracks " << nStoredTracks << endl << endl;
  }

  for (int k=0; k<kentries; k++){
    
    Int_t itrksub   = i[p++];
    bool foundTrack = true;
    if(stagedSimulation) {
      foundTrack = false;
      //
      // This hit will be stored only if it is in the ancestry history array tracks[]
      //
      for(int kTrack=0; kTrack<nStoredTracks; kTrack++) {
	if(itrksub == tracks[kTrack]) {
	  foundTrack = true;
	  break;
	}
      } // loop over stored ancestor tracks
    } // check on staged simulation
    Int_t plane_num = i[p++];
    if(!foundTrack) {
      if(kPrint < 0) { // Debug print if needed
 	 cout << "\n encodeRootEvntMui skipping MuID track " << itrksub << " in plane " << plane_num << endl;
      }
    }
    else {
      if(stagedSimulation && kPrint<0) { // debug print if needed
        cout << "\n encodeRootEvntMui keeping MuID track " << itrksub << " in plane " << plane_num << endl;
      }
    }
    Int_t trk_id    = i[p++];
    Float_t tof     = f[p++];
    Float_t de      = f[p++];
    rhit[0]         = f[p++];
    rhit[1]         = f[p++];
    rhit[2]         = f[p++];
    phit[0]         = f[p++];
    phit[1]         = f[p++];
    phit[2]         = f[p++];

    if(foundTrack) {
      pisaevent->AddMuiHit(itrksub, plane_num, trk_id, tof, de, rhit, 
			 phit, mctrack, nfile, isubevent);
    }

  }  // loop over all track entries

  if(stagedSimulation)
    kPrint++;

  return;
}
