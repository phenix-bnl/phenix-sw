#include "TTECSectorSideEvent.hh"

#include <math.h>
#include <string.h>
#include <stdlib.h>

//Note: The following functions are placed in
//extern C so that the names of the functions
//will not be mangled C++-wise so that they
//can be passed into qsort.

extern "C" {

  int CompareHits(const void* hit1, const void* hit2);

  /**
   * Used in sorting hits in terms of amplitude.
   * 
   * @return -1 if hit1 is less than hit2, 0 if
   * hit1 equals hit2, and 1 if hit1 is greater than hit2.
   */
  int CompareHits(const void* hit1, const void* hit2)
  {
    TECHit_t* h1 = (TECHit_t*) hit1;
    TECHit_t* h2 = (TECHit_t*) hit2;

    Double_t amplitude1 = h1->amplitude;
    Double_t amplitude2 = h2->amplitude;

    if (amplitude1 < amplitude2)
      return -1;
    else if (amplitude1 > amplitude2)
      return 1;
    else
      return 0;
  }

}

//ClassImp(TTECSectorSideEvent)


//Fill in the values for static TTECSectorSideBounds array:
//  Note: The sides share the same x-y bounds.
Double_t TTECSectorSideEvent::fgSectorSideBounds[4][4] = 
{{-520.0, -275.0, -360.0, -75.0},
 {-550.0, -100.0, -400.0, 100.0},
 {-520.0, 90.0, -360.0, 270.0},
 {-450.0, 240.0, -240.0, 400.0}};


//Set the default value for the static member fgVerbosity.
Int_t TTECSectorSideEvent::fgVerbosity = 12;


TTECSectorSideEvent::TTECSectorSideEvent(Int_t sector,
					 Int_t side,
					 const char* fileName,
					 Int_t eventNumber,
					 Bool_t isSimulated)
{
  fIsErrored = kFALSE;

  //Set the minimum threshhold for hit amplitude.
	//Hits with FADC>= fThreshhold are included in the display
  fThreshhold = 3.0;

  fSector = sector;
  fSide = side;

  fHitsSorted = kFALSE;

  fFileName = 0;

  if (fSector < 0 || fSector > 3 || (fSide != 0 && fSide != 1))
    fIsErrored = kTRUE;

  if (!fIsErrored) {

    fEventNumber = eventNumber;
  
    fFileName = new char[strlen(fileName) + 1];
    strcpy(fFileName, fileName);

    fIsSimulated = isSimulated;

    fRegionName = GetNameForSectorSide(sector, side);

    fXMin = fgSectorSideBounds[fSector][0];
    fYMin = fgSectorSideBounds[fSector][1];
    fXMax = fgSectorSideBounds[fSector][2];
    fYMax = fgSectorSideBounds[fSector][3];

    fNumberOfHits = 0;
    fNumberOfTracks = 0;
    
    fHits = 0;
    fTracks = 0;
  }
}

TTECSectorSideEvent::~TTECSectorSideEvent()
{
  delete [] fFileName;
  delete [] fHits;
  delete [] fTracks;
}

///Get the title corresponding to the given sector and side.
const char* TTECSectorSideEvent::GetNameForSectorSide(Int_t sector,
						      Int_t side)
{

  const char* nameString = "";
  if (sector == 0) {
    if (side == 0)
      nameString = "TEC East Arm, Sector 0, South Side  ";
    if (side == 1)
	nameString = "TEC East Arm, Sector 0, North Side  ";
  }
  else if (sector == 1) {
    if (side == 0)
      nameString = "TEC East Arm, Sector 1, South Side  ";
    if (side == 1)
      nameString = "TEC East Arm, Sector 1, North Side  ";
  }
  else if (sector == 2) {
    if (side == 0)
      nameString = "TEC East Arm, Sector 2, South Side  ";
    if (side == 1)
      nameString = "TEC East Arm, Sector 2, North Side  ";
  }
  else if (sector == 3) {
    if (side == 0)
      nameString = "TEC East Arm, Sector 3, South Side  ";
    if (side == 1)
      nameString = "TEC East Arm, Sector 3, North Side  ";
  }
  return nameString;
}

///Add a hit to the list of hits.
void TTECSectorSideEvent::AddHit(const TECHit_t& hit)
{
  if (!fHits) {
    fHitsSize = 1024;
    fHits = new TECHit_t[fHitsSize];
    fNumberOfHits = 0;
  }

  //If there isn't enough room in the current fHits array,
  //expand the fHits array.
  if (fNumberOfHits == fHitsSize) {
    int oldHitsSize = fHitsSize;
    TECHit_t* oldFHits = fHits;
    
    fHitsSize = fHitsSize * 2;
    fHits = new TECHit_t[fHitsSize];
    
    //Copy the contents of the old fHits into the new.
    memcpy(fHits, oldFHits, oldHitsSize * sizeof(TECHit_t));
    
    delete [] oldFHits;
  }

  fHits[fNumberOfHits] = hit;
  fNumberOfHits++;

  fHitsSorted = kFALSE;
}

/**
 * Add a track to the list of tracks.
 * <p>
 * Only tracks with at least one end within
 * the region comprising this sector side
 * will be accepted.
 *
 * @return kTRUE if the track was accepted.
 */
Bool_t TTECSectorSideEvent::AddTrack(const TECTrack_t& track)
{
  if (!(InRange(track.x1, track.y1) || InRange(track.x2, track.y2)))
    return kFALSE;

  if (!fTracks) {
    fTracksSize = 64;
    fTracks = new TECTrack_t[fTracksSize];
    fNumberOfTracks = 0;
  }

  //If there isn't enough room in the current fTracks array,
  //expand the fTracks array.
  if (fNumberOfTracks == fTracksSize) {
    int oldTracksSize = fTracksSize;
    TECTrack_t* oldFTracks = fTracks;
    
    fTracksSize = fTracksSize * 2;
    fTracks = new TECTrack_t[fTracksSize];
    
    //Copy the contents of the old fTracks into the new fTracks.
    memcpy(fTracks, oldFTracks, oldTracksSize * sizeof(TECTrack_t));
    
    delete [] oldFTracks;
  }
  
  fTracks[fNumberOfTracks] = track;
  fNumberOfTracks++;

  return kTRUE;

}

///Sort the hits by amplitude.
void TTECSectorSideEvent::SortHits()
{

  if (!fHits)
    return;

  //Sort the array of hits by amplitude to allow for fast access
  //of subranges of hits by amplitude later.
  qsort(fHits, fNumberOfHits, sizeof(TECHit_t), CompareHits);

  fHitsSorted = kTRUE;
  UpdateThreshholdIndex();
}

///Update fThreshholdIndex.
void TTECSectorSideEvent::UpdateThreshholdIndex()
{
  if (!fHitsSorted)
    SortHits();

  //FindClosestHit() uses fThreshholdIndex so we
  //must temporarily set fThreshholdIndex to 0
  //to get the new value.
  fThreshholdIndex = 0;
  fThreshholdIndex = FindClosestHit(fThreshhold, kFALSE);

  //If even the largest amplitude < fThreshhold,
  //set fThreshholdIndex to -1, indicating that
  //there are no hits with amplitudes above
  //the cut off threshhold.
  if ((fThreshholdIndex == (fNumberOfHits - 1)) &&
      (fHits[fThreshholdIndex].amplitude < fThreshhold)) {
    fThreshholdIndex = -1;
  }
}

/**
 * Get a pointer to an array of all of the hits
 * of this TEC event with corresponding amplitudes
 * greater than or equal to min and less than or 
 * equal to max. The caller should <em>not</em>
 * attempt to delete the array returned through
 * hits; the model will take care of the memory.
 *
 * @param min The minimum amplitude to accept.
 * @param max The maximum amplitude to accept.
 * @param hits Reference variable that will point to an array of hits in the specified range.
 * @param numberOfHits The number of hits in the specified amplitude range.
 */
void TTECSectorSideEvent::GetHitsInAmplitudeRange(Double_t min,
					     Double_t max,
					     const TECHit_t*& hits,
					     Int_t& numberOfHits)
{

  numberOfHits = 0;
  hits = 0;

  if (!fHits)
    return;

  if (!fHitsSorted)
    SortHits();

  if (fThreshholdIndex == -1)
    return;

  Int_t lowIndex = FindClosestHit(min, kFALSE);
  Int_t highIndex = FindClosestHit(max, kTRUE);

  if (lowIndex > highIndex)
    return;

  numberOfHits = highIndex - lowIndex + 1;

  hits = fHits + lowIndex;
}

/**
 * Find the index of the track with amplitude closest
 * to the given amplitude starting from the indicated direction.
 * <p>
 * This method returns -1 if there are no hits fitting the
 * criterion.
 *
 * @param amplitude The target amplitude.
 * @param lessThan kTRUE for less than; kFALSE for greater than.
 * @return Index of the element with the closest amplitude.
 */
Int_t TTECSectorSideEvent::FindClosestHit(Double_t amplitude, Bool_t lessThan)
{

  //Assume that the hits are sorted and that there are more than
  //zero hits.

  //Use the binary search algorithm to find the proper index.
  //See 
  //http://www-ee.eng.hawaii.edu/Courses/EE150/Book/chap10/section2.1.3.html
  //for a description of this algorithm.

  if (!fHits)
    return -1;

  if (fThreshholdIndex == -1)
    return -1;

  Int_t lower = fThreshholdIndex;
  Int_t upper = fNumberOfHits - 1;
  Int_t middle = -2;

  Double_t hitAmp = -98.6; //Amplitude of hit at middle/index

  while (lower <= upper) {
    middle = (lower + upper) / 2;
    
    hitAmp = fHits[middle].amplitude;

    if (hitAmp == amplitude)
      break;
    else if (amplitude < hitAmp) {
      upper = middle - 1;
    }
    else {
      lower = middle + 1;
    }
  }

  Int_t index = middle;
  if (lessThan && amplitude < hitAmp) {
    index--;
  }
  else if (!lessThan && amplitude < hitAmp) {
    index++;
  }
  
  if (index < fThreshholdIndex) {
    index = fThreshholdIndex;
  }
  else if (index > fNumberOfHits - 1) {
    index = fNumberOfHits - 1;
  }

  return index;
}













