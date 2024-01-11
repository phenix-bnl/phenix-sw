#ifndef __EMCCALIBRATIONDATA_H__
#define __EMCCALIBRATIONDATA_H__

#ifndef __EMCMANAGEABLE_H__
#include "emcManageable.h"
#endif
#ifndef __PHTIMESTAMP_H__
#include "PHTimeStamp.h"
#endif

/** Generic class for storing EMCAL calibration data.
 
First implementation : 31-Jan-2001.
Author : L. Aphecetche (aphecetc@in2p3.fr)
 
This class can be used to store various flavours of calibration, e.g. Initial Calibrations, Mip Corrections, and so on. It has been designed to be (hopefully) easily extendable to new flavours (types).
 
It is characterized by a type, a positive number (and the range of valid numbers : [0-range]) and a validity period (2 timestamps).
    
For every "channel", it stores 2 arrays : one for value, and the other for the associated error on those values. The length of those arrays is called hereafter the dimension. 
The meaning of the index (i.e. what is a "channel") of those arrays is let to the user : depending on the type, user should know what it is about (e.g. sector towers or PHENIX-wide towers or even a single per-run number).
The range is used by the plugin that handles this class to know the maximum number of such objects it could have to store at the same time.
 
Only predefined types of calibration data can be used. Those types are identified by the enum EType below. The GetTypeName() method can be used to retrieve a human-readable name from this type.
 
By now, only 2 differents types are defined :
 
(1) kIniCal : for initial sector-wide calibrations. 
Dimension=3 (i.e. 3 values and 3 errors per channel), 
Range=8 (i.e. 8 sectors),
channel=sector tower,	
 
(2) kTofSectorOffset : run-by-run sector t0s
Dimension=5
Range=8
channel=1 (phenix sector)

How to add new types :
 
a) Add an enum value (AFTER the already present ones)
b) Edit the SetTypeAndSize() method to set the range and dimension for
the new type.
c) Edit the GetTypeName(EType type) method to return a name from 
the new type.
d) In the DataManager_plugins package, have a look at the emcOMCalibrationData plugin to see if it can handle automatically the new type or not (this depends basically on the dimension : 3 and 1 are already handled. If you want other dimensions, you'll have to edit the plugin code (basically adding a new low-level Pdbcal class). See the Read and Write methods, those are the only ones that should be changed.
*/

class emcCalibrationData : public emcManageable
  {

  public:

    // If you add a new enum there, put it AFTER those ones !
    enum EType { kUnknown = 0, 
                 kIniCal=1,         
 // 2,3,4 are not used on purpose (used to be there and were removed) 
                 kTofSectorOffset=5 
               };

    /** Default ctor. The size needs not to be given if this object
        is to be fetched from the database, but type and number should
        be given then, so the relevant plugin can be used. */
    emcCalibrationData(EType type = kUnknown, size_t number = 0, size_t size = 0);
    virtual ~emcCalibrationData();

    // The category is used to build the database filename.
    const char* GetCategory(void) const;

    // End-of-validity time.
    const PHTimeStamp& GetEndValTime(void) const
      {
        return fEnd;
      }

    // Get the dimension of the internal arrays.
    size_t GetDimension(void) const
      {
        return fDimension;
      }

    // Get dim-th error for a given "channel".
    float GetError(size_t index, size_t dim = 0) const;

    // Get the characteristic number of this object.
    size_t GetNumber(void) const
      {
        return fNumber;
      }

    // Get the maximum allowed number of this object.
    size_t GetRange(void) const
      {
        return fRange;
      }

    // Get the size (i.e. the number of "channels").
    size_t GetSize(void) const
      {
        return fSize;
      }

    // Start-of-validity time.
    const PHTimeStamp& GetStartValTime(void) const
      {
        return fStart;
      }

    // Get the type of this object.
    EType GetType(void) const
      {
        return fType;
      }

    // Convert the type into a string. (static version)
    static const char* GetTypeName(EType type);

    // Convert the type into a string.
    const char* GetTypeName(void) const
      {
        return GetTypeName(fType);
      }

    // Get the dim-th value of a given "channel", indexed by index.
    float GetValue(size_t index, size_t dim = 0) const;

    // Tells if the object is valid at time 'when'
    virtual bool IsValid(const PHTimeStamp& when) const;

    // Prints header (level=0) or full object (level>0).
    void Print(int level = 0) const;

    // Resets completely the object (erases internal arrays).
    virtual void Reset(void);

    // Set value and error for a given dimension of a given channel.
    bool Set(size_t index, float value, float error, size_t dim = 0);

    /** Set the type and the size.
        Those 2 are set together, as it can be that if you change the
        type you'll change the dimension, and then the internal
        structures will have to be re-allocated to reflect the changes.
        This method erases the object (Reset).
     */
    void SetTypeAndSize(EType type, size_t size);

    // Set the characteristic number of this object.
    void SetNumber(size_t number)
    {
      fNumber = number;
    }

    // Set the validity period.
    void SetValidityPeriod(const PHTimeStamp& t1, const PHTimeStamp& t2)
    {
      fStart = t1;
      fEnd = t2;
    }

  public:
    static const float fgInvalidFloat;

  private:
    PHTimeStamp fStart;
    PHTimeStamp fEnd;
    EType fType;
    size_t fNumber;
    float** fValues;
    float** fErrors;
    size_t fSize;
    size_t fDimension;
    size_t fRange;
  };

#endif
