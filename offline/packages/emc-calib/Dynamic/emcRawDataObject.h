#ifndef __EMCRAWDATAOBJECT_H__
#define __EMCRAWDATAOBJECT_H__

// Purpose: Storage object for EmCal Raw Data
//
// Description: This object is meant to be commonly used in on- and off-line
//              codes.
// Should be deprecated as soon as online code does not need it anylonger,
// as it's now deprecated in offline.
//
// Author: Laurent Aphecetche (aphecetc@in2p3.fr)

#ifndef __EMCDATAOBJECT_H__
#include "emcDataObject.h"
#endif
#ifndef __EMCFEM_H__
#include "EmcFEM.h"
#endif
#include <map>

/** Common RawDataObject to be used on- and off-line.
 
Purpose: encapsulate the "flat" arrays of the Dynamic classes (online) and
replace the STAF table mEmcRawData (offline).
    
It derives from TObject so it can be written in a TTree using the
PHNodeIOManager of PHOOL.
 
FIXME ? Here are some things to keep in mind :
 
- The data map is not written for the moment, which
means that this object alone is useless, i.e. you need to know
which was the configuration used to create it. Can we imagine a global
way of solving this problem ? -> see the Streamer method in the
implementation file for the chosen solution so far (which requires
an emcRawDataAccessor object to have been already built).
 
- The LG and HG values are not to be found in this object 
(see emcMixedDataObject if you need those)
  
- The Streamer function is a custom one, which means that if you ever
make changes in the data members, you MUST check that the Streamer
is still valid.
*/

  class emcRawDataObject : public emcDataObject
  {

  public:

    emcRawDataObject();

    emcRawDataObject(Int_t thesize,
                     Int_t * datamap,
                     Float_t* tac,
                     Float_t* hgpost, Float_t* lgpost,
                     Float_t* hgpre, Float_t* lgpre,
                     const cells* amucells,
                     int* dataerrors = 0);

    virtual ~emcRawDataObject();
    void SetCells(int iSM, int ctac, int cpre, int cpost);
    void GetCells(int index, int &ctac, int &cpre, int &cpost) const;
    int GetTACCell(int index) const;
    /** Set content of the channel (used with new emcDataFormatter) */
    void Set(const int index,
             const int & tac,
             const int & hgpost, const int & lgpost,
             const int & hgpre, const int & lgpre, const int & dataerror);

    /** Get information for one channel = 1 TAC + 4 ADC + Amu cells
        + data-readout-error */
    void Get(Int_t index,
             Float_t& tac,
             Float_t& hgpost, Float_t& lgpost,
             Float_t& hgpre, Float_t& lgpre,
             int& amupre, int& amupost, int& amutac,
             int& dataerror) const;
    void Get(Int_t index,
             Float_t& tac,
             Float_t& hgpost, Float_t& lgpost,
             Float_t& hgpre, Float_t& lgpre,
             int& dataerror) const;

    /** Get High Gain Post value for one channel
        or -9999 if index is not valid.*/
    Float_t GetHGPost(Int_t index) const
    {
      return ValidIndex(index) ? fHGPost[index] : -9999;
    }

    /** Get High Gain Pre value for one channel
        or -9999 if index is not valid.*/
    Float_t GetHGPre(Int_t index) const
    {
      return ValidIndex(index) ? fHGPre[index] : -9999;
    }

    /// Get index from towerId
    int GetIndexByTowerId(int towerid) const;

    /** Get Low Gain Post value for one channel
        or -9999 if index is not valid.*/
    Float_t GetLGPost(Int_t index) const
    {
      return ValidIndex(index) ? fLGPost[index] : -9999;
    }

    /** Get Low Gain Pre value for one channel
        or -9999 if index is not valid.*/
    Float_t GetLGPre(Int_t index) const
    {
      return ValidIndex(index) ? fLGPre[index] : -9999;
    }

    /** Get TAC value for one channel
        or -9999 if index is not valid.*/
    Float_t GetTAC(Int_t index) const
    {
      return ValidIndex(index) ? fTAC[index] : -9999;
    }

    /** Reset AMU cells to "zero". The reason to do it - whenever the packet is missing - it is otherwise left uninitiated */
    void resetAMUAddresses(const int fem) const;

    /** Get AMU Post value for one channel
        or 255 if index is not valid.*/
    Int_t GetAMUPost(Int_t index) const
    {
      return ValidIndex(index) ? fAMUcells[index / fNumberOfWordsPerFEM].post : 255;
    }

    /** Get AMU Pre value for one channel
        or 255 if tower_index is not valid.*/
    Int_t GetAMUPre(Int_t index) const
    {
      return ValidIndex(index) ? fAMUcells[index / fNumberOfWordsPerFEM].pre : 255;
    }

    /** Get AMU TAC value for one channel
        or 255 if index is not valid.*/

    Int_t GetAMUTAC(Int_t index) const
    {
      return ValidIndex(index) ? fAMUcells[index / fNumberOfWordsPerFEM].tac : 255;
    }

    /** Get number of words per FEM (should be 144 most of the time, but
        could be 192 for some online use cases).*/
    Int_t GetNumberOfWordsPerFEM(void) const
    {
      return fNumberOfWordsPerFEM;
    }

    /// Tells if channel is zero
    bool IsZero(Int_t index) const;

    friend std::ostream& operator << (std::ostream&, const emcRawDataObject&);

  private:
    /// Time
    Float_t * fTAC;
    /// High gain post
    Float_t * fHGPost;
    /// Low gain post
    Float_t * fLGPost;
    /// High gain pre
    Float_t * fHGPre;
    /// Low gain pre
    Float_t * fLGPre;
    /// AMU cells array
    cells * fAMUcells;
    /// Indicates if WE are allocating the tower data arrays or not (true if reading from Root file, false in any other cases).
    bool fOwnAllocation;
    /// Number of words per FEM (144 except for some online purposes=192).
    Int_t fNumberOfWordsPerFEM;
    /// Used to recover index number from TowerId number.
    std::map<int, int> fIndexMap;

  private:
    // Copy constructor and assignation (disabled by default).
    emcRawDataObject(const emcRawDataObject& obj);
    // Assignment operator (disabled).
    emcRawDataObject& operator = (const emcRawDataObject& obj);

  public:
    ClassDef(emcRawDataObject, 1)
  };

#endif // #ifndef __emcRawDataObject_h__
