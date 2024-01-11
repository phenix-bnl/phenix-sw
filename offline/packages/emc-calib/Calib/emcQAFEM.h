#ifndef __EMCQAFEM_H__
#define __EMCQAFEM_H__

#ifndef __EMCCALFEM_H__
#include "emcCalFEM.h"
#endif

#include <vector>
#include <map>
#include <iostream>
#include "Rtypes.h"


typedef UInt_t INT32;

/** Stores (online) Q&A information for one FEM. 
    @ingroup calibration
    @sa emcBadModules 
*/

class emcQAFEM : public emcCalFEM
{

public:

  /// ctor
  explicit emcQAFEM(int absPosition);

  /// ctor
  emcQAFEM(int absPosition, 
           const PHTimeStamp& t1, const PHTimeStamp& t2);

  /// copy ctor
  emcQAFEM(const emcQAFEM&);

  /// assignment operator.
  emcQAFEM& operator=(const emcQAFEM&);

  /// dtor
  virtual ~emcQAFEM();

  virtual emcQAFEM* clone(void) const 
    { return new emcQAFEM(*this); }

  virtual emcQAFEM* create(void) const 
  {
    return new emcQAFEM(AbsolutePosition(),
			GetStartValTime(),
			GetEndValTime());
  }


  /// Append a new channel to this FEM.
  void AppendOneChannel(int channel, INT32 error, INT32 warning);

  /** Create a default object with 144 perfect channels.
      perfect means no error, no warning.
   */
  static emcQAFEM* Default(int absPosition,
                           const PHTimeStamp& tStart = PHTimeStamp(0),
                           const PHTimeStamp& tEnd = 
			   PHTimeStamp(PHTimeStamp::PHFarFuture));
  
  /// Initialize the iterator to loop over channels
  void First(void);

  /// Category of this emcManageable = "QAs"
  virtual const char* GetCategory(void) const { return "QAs"; }

  /// Get the number of channels of this FEM.
  virtual size_t GetNumberOfChannels(void) const { return fQA.size(); }

  using emcCalFEM::getValue;
  using emcCalFEM::getValueFast;

  /// Get error for a given channel.
  virtual float getValue(int ichannel) const {
    return getValue(ichannel,0); }

  /// Same as above but without bound checking.
  virtual float getValueFast(int ichannel) const {
    return getValueFast(ichannel,0); }

  /// Get value (what=0->error, what=1->warning) for a given channel.
  virtual float getValue(int ichannel, int what) const;
  
  /// Same as above but without bound checking.
  virtual float getValueFast(int ichannel, int what) const;

  /// Comparison.
  virtual bool IsEqual(const emcCalFEM&) const;

  /// Get next values (to be used with First())
  bool Next(int& channel, INT32& error, INT32& warning);

  /// A simple output.
  virtual std::ostream& Print(std::ostream& out = std::cout, int level=0) const;

  /// Reset this object completely.
  virtual void Reset(void);

private:

  /// Used by copy ctor.
  void Copy(emcQAFEM&) const;

  /// Get the error value of a given channel.
  INT32 GetError(int channel) const;

  /// Get the warning value of a given channel.
  INT32 GetWarning(int channel) const;


 private:

#ifndef __CINT__
  std::map<int,std::vector<INT32> > fQA;
  std::map<int,std::vector<INT32> >::const_iterator fIterator;
#endif

};

#endif
