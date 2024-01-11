#ifndef __TTECSECTORSIDEEVENT_HH__
#define __TTECSECTORSIDEEVENT_HH__

#include <Rtypes.h>
#include <iostream>
#ifndef __CINT__
/**
 * A struct representing a TEC hit.
 */
struct TECHit_t {
  Double_t x;
  Double_t y;
  Double_t amplitude;
};

#else

struct TECHit_t;

#endif

#ifndef __CINT__
/**
 * A struct representing a TEC track.
 */
struct TECTrack_t {
  Double_t x1;
  Double_t y1;
  Double_t x2;
  Double_t y2;
};

#else

struct TECTrack_t;

#endif


class PHCompositeNode;

/**
 * A model of the specified side and sector of a TEC event. 
 * This class provides access
 * to the hits and tracks of a TEC event, or subranges
 * of those hits and tracks with respect to amplitude.
 *
 * @author David Faden, dfaden@iastate.edu
 * @version July 27, 2001
 */
class TTECSectorSideEvent {

public:

  /*
  enum ETECSector {kTECSector0, kTECSector1, kTECSector2, kTECSector3};

  enum ETECSide {kTECSouth, kTECNorth};
  */

  TTECSectorSideEvent(Int_t sector,
		      Int_t side,
		      const char* fileName,
		      Int_t eventNumber,
		      Bool_t isSimulation);

  
  virtual ~TTECSectorSideEvent();

  /**
   * Get the verbosity level for TTECSectorSideEvents.
   */
  static Int_t GetVerbosity()
  {
    return fgVerbosity;
  }

  /**
   * Set this object's verbosity level.
   * The higher the verbosity level, the more
   * information TTECSectorSideEvents will print out to
   * the standard C streams about their internal
   * workings.
   *
   * @param verbosity The new verbosity level.
   */
  static void SetVerbosity(Int_t verbosity) 
  {
    fgVerbosity = verbosity;
  }

  //Should the Add methods and the constructor
  //be private?

  ///Add a hit to the list of hits.
  void AddHit(const TECHit_t& hit);

  /**
   * Add a track to the list of tracks.
   * <p>
   * Only tracks with at least one end within
   * the region comprising this sector side
   * will be accepted.
   *
   * @return kTRUE if the track was accepted.
   */
  Bool_t AddTrack(const TECTrack_t& track);

  /**
   * Has this TTECSectorSideEvent encountered a fatal
   * error? If this method returns kTRUE, no further use
   * should be made of this object.
   * 
   * @return Whether this object has encountered a serious error.
   */
  Bool_t IsErrored() const
  {
    return fIsErrored;
  }

  /**
   * Is this event simulated?
   *
   * @return Is this event simulated?
   */
  Bool_t IsSimulated() const
  {
    return fIsSimulated;
  }

  /**
   * Get this event's event number.
   *
   * @return The event number of this event.
   */
  Int_t GetEventNumber() const 
  {
    return fEventNumber;
  }

  /**
   * Get the name of the file that
   * contains this event's data.
   * 
   * @return The name of the file containing this
   * event's data.
   */
  const char* GetFileName() const
  {
    return fFileName;
  }

  /**
   * Get the name for the part of the TEC where
   * the event was recorded.
   * 
   * @return The name for the part of the TEC where
   * the event was recorded.
   */
  const char* GetRegionName() const
  {
    return fRegionName;
  }

  /**
   * The minimum x position of the region of
   * of the TEC in which this event occurred.
   */
  Double_t GetXMin() const
  {
    return fXMin;
  }

  /**
   * The maximum x position of the region of
   * the TEC in which this event occurred.
   */
  Double_t GetXMax() const
  {
    return fXMax;
  }

  /**
   * The minimum y position of the region of
   * of the TEC in which this event occurred.
   */
  Double_t GetYMin() const
  {
    return fYMin;
  }

  /**
   * The maximum y position of the region of
   * the TEC in which this event occurred.
   */
  Double_t GetYMax() const
  {
    return fYMax;
  }

  /**
   * Get the sector in which the event occurred.
   * 0, 1, 2, 3.
   */
  Int_t GetSector() const
  {
    return fSector;
  }

  /**
   * Get the side in which the event occurred.
   * 0 for south, 1 for north.
   */
  Int_t GetSide() const
  {
    return fSide;
  }

  /**
   * Get the minimum acceptable hit amplitude.
   */
  Double_t GetThreshhold() const
  {
    return fThreshhold;
  }

  /**
   * Set the minimum acceptable hit amplitude.
   */
  void SetThreshhold(Double_t threshhold)
  {
    fThreshhold = threshhold;
    UpdateThreshholdIndex();
  }

  //Note: The methods GetHits, GetNumberOfHits, GetTracks,
  //and GetNumberOfTracks have been not been made const
  //in case it becomes clear in the future that
  //it's advantageous to delay retrieving the info.

  /**
   * Get the hits corresponding to this event with
   * amplitudes greater than the threshhold value.
   * 
   * @return This event's hits.
   */
  const TECHit_t* GetHits()
  {
    if (!fHitsSorted)
      SortHits();

    if (fThreshholdIndex == -1)
      return 0;

    return fHits + fThreshholdIndex;
  }

  /**
   * Get the number of hits corresponding to this event with
   * amplitudes greater than the threshhold value.
   *
   * @return The number of hits corresponding to this event.
   */
  Int_t GetNumberOfHits()
  {
    if (!fHitsSorted)
      SortHits();

    if (fThreshholdIndex == -1) {
      return 0;
    }

    return fNumberOfHits - fThreshholdIndex;
  }

  /**
   * Get the tracks corresponding to this event.
   * 
   * @return This event's tracks.
   */
  const TECTrack_t* GetTracks()
  {
    return fTracks;
  }
  
  /**
   * Get the number of tracks corresponding to this event.
   *
   * @return The number of tracks corresponding to this event.
   */
  Int_t GetNumberOfTracks()
  {
    return fNumberOfTracks;
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
  void GetHitsInAmplitudeRange(Double_t min,
			       Double_t max,
			       const TECHit_t*& hits,
			       Int_t& numberOfHits);

private:
  ///The bounds for each sector of the TEC (xMin, yMin, xMax, yMax.
  static Double_t fgSectorSideBounds[4][4];

  ///How much info should this class print about its inner workings?
  static Int_t fgVerbosity;

  ///Has this instance encountered a fatal error?
  Bool_t fIsErrored;

  ///Is this event simulated?
  Bool_t fIsSimulated;

  ///This event's event number.
  Int_t fEventNumber;

  ///The name of the file containing the data for this event.
  char* fFileName;

  ///This event's title.
  const char* fRegionName;

  ///The sector of this part of the event.
  Int_t fSector;

  ///The side of this part of the event.
  Int_t fSide;

  ///Array of hits corresponding to this event.
  TECHit_t* fHits;

  ///The number of hits.
  Int_t fNumberOfHits;

  ///Size (capacity) of the fHits array.
  Int_t fHitsSize;

  ///Indicates whether the fHits array is sorted.
  Bool_t fHitsSorted;

  /**
   * Hits with amplitudes less than than fThreshhold
   * are not returned through the Get methods.
   */
  Double_t fThreshhold;

  /**
   * Index of the event with the smallest amplitude
   * greater than fThreshhold.
   */
  Int_t fThreshholdIndex;

  ///Array of track corresponding to this event.
  TECTrack_t* fTracks;

  ///The number of tracks.
  Int_t fNumberOfTracks;

  ///Size (capacity) of the fTracks array.
  Int_t fTracksSize;

  ///The minimum in range x.
  Double_t fXMin;

  ///The maximum in range x.
  Double_t fXMax;

  ///The minimum in range y.
  Double_t fYMin;

  ///The maximum in range y.
  Double_t fYMax;



  ///Get the title corresponding to the given sector and side.
  static const char* GetNameForSectorSide(Int_t sector, Int_t side);

  ///Sort the hits by amplitude.
  void SortHits();

  ///Update fThreshholdIndex.
  void UpdateThreshholdIndex();

  /**
   * Find the index of the track with amplitude closest
   * to the given amplitude starting from the indicated direction.
   */
  inline Int_t FindClosestHit(Double_t amplitude, Bool_t lessThan);

  /**
   * Are the given coordinates within this sector-side's range?
   * 
   * @param x
   * @param y
   * @return kTRUE if (x, y) is in range.
   */
  Bool_t InRange(Double_t x, Double_t y)
  {
    return (Bool_t) (x >= fXMin && x <= fXMax &&
		     y >= fYMin && y <= fYMax);
  }

  //April 27, 2004:
  //There's no need to create a dictionary  for
  //TTECSectorSideEvent. If we want to hook its instances up
  //using the signal/slot mechanism, however, we will need
  //this.
  //David Faden, dfaden@cs.iastate.edu
  //ClassDef(TTECSectorSideEvent,0)
  
};

#endif









