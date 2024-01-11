#include "PadDominantDch.h"
#include <iostream>

using namespace std;

int PadDominantDch(int dchTrackID, dDchGhitHitsWrapper *dDchGhitHits, DchTrackv1 *DchTracks,
		   dcghitWrapper *dcghit, int &hitPlanes, int &domTotSum, int &hitUVPlanes,
		   dDchHitWrapper *dDchHit, int iEvent) {

// int PadDominantDch(int dchTrackID, dDchGhitHitsWrapper *dDchGhitHits, dDchTracksWrapper *dDchTracks,
//                    dcghitWrapper *dcghit, int &hitPlanes, int &domTotSum, int &hitUVPlanes,
//                    dDchHitWrapper* dDchHit) {

  //
  // Function to obtain the Dominant Track number for the Dch hits in a given track
  //

  const int uvStartDC1 = 12;
  const int uvEndDC1 = 19;
  const int uvStartDC2 = 32;
  const int uvEndDC2 =39;

  static int iBad = 0;

  static int iFirst = 1;
  if(iFirst == 1) {
    iFirst = 0;
    cout << "\n call to PadDominantDch " << endl;
  }

  int dummy_array_t[40];  // holds track numbers
  int dummy_array_k[40];  // counts how many times the kth plane has been hit

  int j;
  int k;

  for (k=0; k<40; k++) {
    dummy_array_t[k] = 0;   // holds track number in this plane
    dummy_array_k[k] = 0;   // holds number of times plane is used
  } // initialize

  //
  // The last "plane" in dDchTrack->get_hits has the associated PC cluster index
  //
  for (k=0; k<39; k++) {

    short idHit = DchTracks->get_hits(dchTrackID,k);  // using new DchTracks data structure
//  int idHit = dDchTracks->get_hits(k,dchTrackID); // using wrapped dDchTracks Table
    if (idHit > 0) {
      int planeOfHit = dDchHit->get_plane(idHit);
      unsigned int dchGhitHitsID = dDchGhitHits->get_hitsid(idHit);
      if(planeOfHit>-1 && planeOfHit<40 && dchGhitHitsID<dcghit->RowCount()) {
        //
        // Dominant contributor loop filling
        // Based on the kth plane in dDchTrack being hit
        //
        int mcTrack = dcghit->get_mctrack(dchGhitHitsID);
        if(mcTrack > 0) {
          dummy_array_t[planeOfHit] = dcghit->get_mctrack(dchGhitHitsID);
          dummy_array_k[planeOfHit]++;         // count how many times plane has GhitHit
        }
        else {
          cerr << " At (dchTrackID,k) = (" << dchTrackID << "," << k << ")";
	  cerr << ", plane = " << planeOfHit << endl;
          cerr << " MCTRACK is invalid = " << mcTrack << "\n";
        }  // check if valid MC track number
      }  // check if dchGhitHitsID is valid
    } // check if valid idHit
  } // loop over k planes

  //
  // Search for dominant contributor
  //
  int current_dominant = 0;
  short current_sum = 0;
  short totplanes = 0;
  short totplanesUV = 0;
  short plnehits = 0;
  for(k=0; k<40; k++){
    if(dummy_array_k[k] > 0 ){
      totplanes++;
      plnehits = plnehits + dummy_array_k[k];
      
      if((k >= uvStartDC1 && k <= uvEndDC1) ||
	 (k >= uvStartDC2 && k <= uvEndDC2)){
	totplanesUV++;
      }
      int test_dominant = dummy_array_t[k];
      short test_sum = 0;
      for(j=0; j<40; j++){
	if(test_dominant == dummy_array_t[j])
	  test_sum++;
      } // loop over j planes
      if(test_sum > current_sum){
        current_sum = test_sum;
        current_dominant = test_dominant;
      }  // new sum being bigger than old sum
    } // check on there being at least one hit in this k plane	
  }  // loop on k planes

  if(iEvent == -1) {
    cout << "\n PadDominantDch : At track #dchTrackID = " << dchTrackID;
    cout << "\nDominant track = " << current_dominant;
    cout << "\nDominant sum = " << current_sum << "\n";
    for(k=0; k<40; k++){
      cout << " k " << k << " Track " << dummy_array_t[k];
      cout << ",  Counts " << dummy_array_k[k] << "\n";
    }
    cout << endl;
  } // print out for selected event (disabled)

  if(current_dominant <= 0 || current_sum <= 0){
    //
    // Should not have a dominant track number as 0, or hit planes as 0
    //
    if(iBad < 10) {
      iBad++;
      cerr << "\n PadDominantDch <E>: Error at track #dchTrackID = " << dchTrackID;
      cerr << "\nDominant track = " << current_dominant;
      cerr << "\nDominant sum = " << current_sum << "\n";
      for(k=0; k<40; k++){
        cout << " k " << k << " Track " << dummy_array_t[k];
        cout << ",  Counts " << dummy_array_k[k] << "\n";
      }
    } // check on bad print out
    return -1;
  }

  hitPlanes = totplanes;
  domTotSum = current_sum;
  hitUVPlanes = totplanesUV;

  return current_dominant;
}

