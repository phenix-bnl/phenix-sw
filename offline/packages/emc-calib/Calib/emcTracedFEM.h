#ifndef  __EMCTRACEDFEM_H__
#define  __EMCTRACEDFEM_H__

#include <Rtypes.h>
#include <vector>
#include <cstdio>

#ifndef __EMCCALFEM_H__
#include "emcCalFEM.h"
#endif

class emcTracedValue;

/** Base class for storing time-dependant calibration data for one FEM.
    
    It is used for Gains, TacPeds and TofT0s calibrations.

@ingroup calibration
*/

class emcTracedFEM : public emcCalFEM
{
 public:
 
  /// ctor.
  explicit emcTracedFEM(int absPosition);

  /// ctor.
  emcTracedFEM(int absPosition, const PHTimeStamp& t1, const PHTimeStamp& t2);

  /// copy ctor.
  emcTracedFEM(const emcTracedFEM&);

  /// assignment operator.
  emcTracedFEM& operator=(const emcTracedFEM&);

  /// virtual constructor.
  virtual emcTracedFEM* clone(void) const = 0 ; 

  /// dtor.
  virtual ~emcTracedFEM();

  /** Add a new item for a given channel. The item is adopted (i.e. this
      object is responsible for the deletion of item). */
  virtual void AddNewItem(int channel, emcTracedValue* item);

  /** Compact items that are consistent within epsilon.
      WARNING: not thoroughly tested yet. Use with care. */
  virtual float Compact(float epsilon) = 0;

  /// Default value to be returned by getValue methods.
  virtual float DefaultReturnValue(void) const { return 1.0; }

  /// Delete all items.
  void Delete(void);

  /** Initialize iterator for looping over items corresponding to channel. */
  void FirstItem(int channel) const;

  /** Still virtual at this point.
      see emcGainFEM, emcTacPedFEM, emcTofT0FEM for concrete one.
  */
  virtual const char* GetCategory(void) const = 0;

  /// How many channels are handled by this object ?
  size_t GetNumberOfChannels(void) const { return fItems.size(); }

  /** How many objects are kept by this object ?
      It's equal to sum_over_channel(number_of_item_for_channel).
  */
  size_t GetNumberOfItems(void) const { return fNItems; }

  /// How many items for a given channel ?
  size_t GetNumberOfItems(int ichannel) const; 

  using emcCalFEM::getValue;
  using emcCalFEM::getValueFast;

#ifndef __CINT__
  /// Get the value for a given channel at a given x
  virtual float getValueFast(int ichannel, time_t x) const;
#endif

  virtual float getValueFast(int ichannel, int x) const { 
    return getValueFast(ichannel,static_cast<time_t>(x)); }

  /// comparison.
  virtual bool IsEqual(const emcCalFEM& obj) const;

  /** Get last line of a given channel.
      Please note that you'll get the true pointer to our guts, so
      you can easily change things in place. That might be cool, but
      that's also dangerous, so be careful...*/
  emcTracedValue* LastItem(int channel) const;

  /** Get next item of the loop initialized with FirstValue(item). 
      See also comment for LastItem method.*/
  emcTracedValue* NextItem(void) const;

  /// Print (for debug purposes)
  virtual std::ostream& Print(std::ostream& out=std::cout, int level=0) const;

  /** Remove last item for each channel.
      This is a fix for PbSc m_091001 calibration data set.*/
  void RemoveLastItems(void);

  /// Reset the object
  virtual void Reset(void);

  /// Set the number of channels (recreating the object from scratch).
  virtual void SetNumberOfChannels(int nchannels);

  /// Some text output on a file.
  virtual void writeDataToFile(FILE * fp) const;

public:

#ifndef __CINT__

  typedef std::vector<emcTracedValue*> emcItemVector;

protected:
  /// Copy this into o.
  void Copy(emcTracedFEM& o) const;


  ///
  emcTracedValue* getTV(int channel, time_t absoluteX, 
			size_t& thecase) const;

protected:
  ///
  std::vector<emcItemVector*> fItems; //!
  ///
  size_t fNItems; //!

private:
  ///
  mutable size_t fCurrentItem; //!
  ///
  mutable size_t fCurrentPosition; //!
#endif

};

#endif   //  __emcTracedFEM__ 


