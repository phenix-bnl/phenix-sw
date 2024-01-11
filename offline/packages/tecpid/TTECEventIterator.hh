#ifndef __TTECEVENTITERATOR_HH__
#define __TTECEVENTITERATOR_HH__

#include <TQObject.h>

#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "Eventiterator.h"
#include "PHNodeReset.h"
#include "mTecUnpackModule.h"
#include "mTecCalibModule.h"
#include "mTecHoughTrackModule.h"
#include "TecOutV1.hh"
#include "TTECSectorSideEvent.hh"

/**
 * A class to iterate over the TEC events in a PRDF file.
 * This class is based on and does about the same
 * things as the tecrecini.C and tecrecrun.C macros.
 * It is not a good idea to call any of this class's
 * non-const methods from more than one thread
 * at the same time.
 * 
 * @author David Faden, dfaden@iastate.edu
 * @version July 31, 2001
 */
class TTECEventIterator : public TQObject {

  //Would it be better to name this class
  //TPrdfTecEventIterator? It is iterating over the
  //PRDF file, not over a TEC event.

public:

  /**
   * Create a new iterator for the given PRDF file.
   *
   * @param prdfFileName Name of the file to load data from.
   * @param runNumber The run number.
   * @param isSimulation Is this simulated data?
   * @param useObjy Should info be fetched from the database?
   */
  TTECEventIterator(char* prdfFileName,
		    Int_t runNumber, 
		    Bool_t isSimulation = kFALSE,
		    Bool_t useObjy = kFALSE);

  ///Clean up.
  ~TTECEventIterator();

  /**
   * Set this object's verbosity level.
   * The higher the verbosity level, the more
   * information this object will print out to
   * the standard C streams about its internal
   * workings.
   *
   * @param verbosity The new verbosity level.
   */
  void SetVerbosity(Int_t verbosity) 
  {
    fVerbosity = verbosity;
    TTECSectorSideEvent::SetVerbosity(verbosity);
  }

  /**
   * Get this object's verbosity level.
   */
  Int_t GetVerbosity() const
  {
    return fVerbosity;
  }

  /**
   * Is info being fetched from the database
   * or from local configuration files?
   */
  Bool_t GetUseObjy() const
  {
    return fUseObjy;
  }

  /**
   * Does the PRDF file associated with this
   * object hold real or simulated data?
   */
  Bool_t IsSimulation() const
  {
    return fIsSimulation;
  }

  /**
   * Get the name of the file from which data is being loaded.
   * 
   * @return The name of the file from which data is being loaded.
   */
  const char* GetFileName() const
  {
    return fPrdfFileName;
  }

  /**
   * Get the number of the last event to be gotten.
   * If no events have been gotten, this method will
   * return 0.
   *
   * @return The number of the last event to be gotten.
   */
  Int_t GetEventNumber() const
  {
    return fEventNumber;
  }

  /**
   * Is the iterator past the last event? Are there no more
   * events to load from the PRDF file in the present direction?
   * This method will return kTRUE only after an attempt has
   * been made to read past the end.
   */
  Bool_t IsPastLast() const
  {
    return fIsPastLast;
  }

  /**
   * Move onto the next TEC event from the PRDF file.
   * The Get methods return whatever event the iterator
   * is currently at. kTRUE is returned if the data
   * for the next event was retrieved without error.
   * kFALSE is returned if there are no more events
   * or if there was an error in retrieving the event.
   * 
   * @return kTRUE if the next event was fetched.
   */
  Bool_t MoveToNext();

  /**
   * Move to the specified event.
   * 
   * @param eventNumber The number of the event to load.
   * @return kTRUE if the specified event was fetched without error.
   */
  Bool_t MoveTo(Int_t eventNumber);

  /**
   * Get the specified sector and side of the 
   * TEC event.
   * <p>
   * Null is returned if there was an error in retrieving
   * the event, if there are no more events, or if this 
   * side-sector has been gotten before (in this iteration).
   * <p>
   * The caller is responsible for deleting the memory
   * of the returned object.
   * <p>
   * This method will return a particular sector-side only once.
   * After a sector-side has been gotten once, subsequent calls
   * to this method will return null.
   * <p>
   * The result of calling this method with an invalid sector
   * and side pair is undefined.
   * 
   * @param sector 0, 1, 2, 3.
   * @param side 0 for south, 1 for north.
   * @return The specified side and sector of the TEC event. 
   */
  TTECSectorSideEvent* GetEvent(Int_t sector, Int_t side);

private:

  ///Verbosity level.
  Int_t fVerbosity;

  ///Is the data simulated or real?
  Bool_t fIsSimulation;

  ///Should info be fetched from the database?
  Bool_t fUseObjy;

  ///The name of the file from which to load data.
  char* fPrdfFileName;

  ///Current event number.
  Int_t fEventNumber;

  ///Run number.
  Int_t fRunNumber;

  ///Has the PHOOL stuff been initialized?
  Bool_t fHasDonePHOOLInit;

  ///Has the first event initialization been done?
  Bool_t fHasDoneFirstEventInit;

  ///Is the iterator past the last event.
  Bool_t fIsPastLast;

  ///Array of events.
  TTECSectorSideEvent* fEvents[8];

  ///Current event.
  Event* fEvent;

  PHCompositeNode* fTopNode;

  PHNodeIterator* fMainIter;

  Eventiterator* fEventIter;  

  PHNodeReset fReset;

  mTecUnpackModule* fMTecUnpack;

  mTecCalibModule* fMTecCalib;

  mTecHoughTrackModule* fMTecHoughTrack;

  TecAddressObject* fTecAddress;

  TecGeometryObject* fTecGeometry;

  TecCalibrationObject* fTecCalibration;

  TecOutV1* fDTecOut;


  //From mTecDrawModule:

  ///
  static void CalcXYFromBin(float relativebin, float Xwire, float Ywire, 
                    float Sinus, float& Xpos, float& Ypos);

  ///
  static void GetMidphi(float* MidPhi, float *midphi, float *sinalpha, 
			TecGeometryObject* TGO);

  ///
  static void GetMinMax(int *MinBin, 
			int *MaxBin, TecCalibrationObject * TCO);


  ///Hide the copy constructor.
  TTECEventIterator(const TTECEventIterator&) {}

  ///Set up the PHOOL classes.
  void InitializePHOOL();

  ///Clean up the memory from the PHOOL objects.
  void CleanUpPHOOL();


  /**
   * Load the event data for the current event. 
   * If the data is loaded successfully,
   * return kTRUE. Otherwise, return kFALSE.
   * 
   * @return kTRUE if the data loading was error free.
   */
  Bool_t LoadData();

  /**
   * look up timestamp from Run1.
   * 
   * @param fname File containing the lookup table.
   * @param timestp Pointer to PHTimeStamp class
   * @param run run number
   * @return 0 if TimeSTamp was found, -1 on Error
   */
  int lookup_timeStamp(char *fname, PHTimeStamp *timestp, int run);

  ClassDef(TTECEventIterator,0)

};

#endif





