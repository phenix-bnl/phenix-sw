#ifndef __EMCHLRATIOFEM_H__
#define __EMCHLRATIOFEM_H__

#ifndef __EMCCALFEM_H__
#include "emcCalFEM.h"
#endif
#include <vector>

/** Stores H/L gain ratio values for one FEM.

    emcHLRatioFEM is to be understood here as a set of four values :
    - an average value (0)
    - an RMS           (1)
    - an intercept     (2)
    - a slope          (3)

    @ingroup calibration
 */

class emcHLRatioFEM : public emcCalFEM
{

 public:

  /// ctor
  explicit emcHLRatioFEM(int absPosition);

  /// ctor
  emcHLRatioFEM(int absPosition,const PHTimeStamp& t1,const PHTimeStamp& t2);

  /// copy ctor
  emcHLRatioFEM(const emcHLRatioFEM&);

  /// assignment operator.
  emcHLRatioFEM& operator=(const emcHLRatioFEM&);

  /// Destructor
  virtual ~emcHLRatioFEM();

  virtual emcHLRatioFEM* clone(void) const 
    { return new emcHLRatioFEM(*this); }

  virtual emcHLRatioFEM* create(void) const 
  {
    return new emcHLRatioFEM(AbsolutePosition(),
			     GetStartValTime(),
			     GetEndValTime());
  }

#ifndef __CINT__
  typedef std::vector<float> Ratio;
  typedef std::vector<Ratio> RatioVector;
  typedef std::vector<float>::iterator itRatio;
  typedef std::vector<float>::const_iterator citRatio;
  typedef std::vector<Ratio>::iterator itRatioVector;
  typedef std::vector<Ratio>::const_iterator citRatioVector;
#endif

  /// Append H/L gain ratio vector for one channel
  void AppendOneChannel(float average, float rms, 
			float intercept, float slope);


  /** Creates a default object with 144 channels. 
      where H/L=fGlobalAverage and rms=fGlobalRMS.*/
  static emcHLRatioFEM* Default(const int& absPosition, 
				const PHTimeStamp& t1, 
				const PHTimeStamp& t2);

  /// Category of this emcManageable = "HLRatios"
  virtual const char* GetCategory(void) const { return "HLRatios"; }

  using emcCalFEM::getValue;
  using emcCalFEM::getValueFast;

  /// Get average value.
  virtual float getValue(int ichannel) const {
    return getValue(ichannel,0); }

  /// Same as above w/o bound checking.
  virtual float getValueFast(int ichannel) const {
    return getValueFast(ichannel,0);
  }

  /** Get the H/L ratio value(s) for a given channel.
   *    what = 0 -> average
   *    what = 1 -> rms
   *    what = 2 -> intercept
   *    what = 3 -> slope
   */
  virtual float getValue(int ichannel, int what) const;

  /// Same as above but without bound checking.
  virtual float getValueFast(int ichannel, int what) const;

  /// Get global (i.e. over all channels) average value.
  static float GetGlobalAverage(void) { return fGlobalAverage; }

  /// Get global (i.e. over all channels) RMS.
  static float GetGlobalRMS(void) { return fGlobalRMS; }

  /// Get number of channels handled by this object.
  virtual size_t GetNumberOfChannels(void) const { 
    return fHLRatioVector.size();}

  /// Comparison.
  virtual bool IsEqual(const emcCalFEM& obj) const;

  /// Reset the object
  virtual void Reset(void); 

  /// Print
  virtual std::ostream& Print(std::ostream& out = std::cout, int level=0) const;

#ifndef __CINT__ 

private:

  /// used by copy ctor.
  void Copy(emcHLRatioFEM& o) const;

private:

  RatioVector  fHLRatioVector;

  static float fGlobalAverage; // for all channels
  static float fGlobalRMS; // for all channels

#endif

};

#endif
