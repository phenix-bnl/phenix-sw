#ifndef __EMCLCTOFFEM_H__
#define __EMCLCTOFFEM_H__

#ifndef __EMCCALFEM_H__
#include "emcCalFEM.h"
#endif

#include <vector>

/** Stores least-count tof values for one FEM. 
@ingroup calibration
*/

class emcLCTofFEM : public emcCalFEM
{

 public:

  /// ctor
  explicit emcLCTofFEM(int absPosition);

  /// ctor
  emcLCTofFEM(int absPosition,  
	    const PHTimeStamp& t1, const PHTimeStamp& t2);

  /// copy ctor
  emcLCTofFEM(const emcLCTofFEM&);

  /// assignment operator.
  emcLCTofFEM& operator=(const emcLCTofFEM&);

  /// dtor
  virtual ~emcLCTofFEM();

  virtual emcLCTofFEM* clone(void) const 
    { return new emcLCTofFEM(*this); }
 
  virtual emcLCTofFEM* create(void) const 
  {
    return new emcLCTofFEM(AbsolutePosition(),
			   GetStartValTime(),
			   GetEndValTime());
  }

#ifndef __CINT__
  typedef std::vector<float> vfloat;
  typedef std::vector<vfloat> LCTof;
  typedef std::vector<float>::iterator itvfloat;
  typedef std::vector<float>::const_iterator citvfloat;
  typedef std::vector<vfloat>::iterator itLCTof;
  typedef std::vector<vfloat>::const_iterator citLCTof;
#endif

  /// Append Tof Least count values for one channel
  void AppendOneChannel( const float value1, const float value2);


  /// Create default object with 144 channels
  static emcLCTofFEM* Default(const int& absPosition, 
                              const PHTimeStamp& t1, 
                              const PHTimeStamp& t2);

  ///
  virtual const char* GetCategory(void) const { return "LCTofs"; }

  /// Get number of channels handled by this object.
  virtual size_t GetNumberOfChannels(void) const { return fLCTof.size(); }

  using emcCalFEM::getValue;
  using emcCalFEM::getValueFast;

  /// Get first value.
  virtual float getValue(int ichannel) const {
    return getValue(ichannel,0); }

  /// Same as above but without bound checking.
  virtual float getValueFast(int ichannel) const {
    return getValueFast(ichannel,0);
  }

  /// Get value(s) from this object.
  virtual float getValue(int ichannel, int what) const;

  /// Set value(s) from this object.
  virtual void  setValue(int ichannel, int what, float value) ;

  /// Same as above but without bound checking.
  virtual float getValueFast(int ichannel, int what) const;

  /// comparison.
  virtual bool IsEqual(const emcCalFEM&) const;

  /// Reset the object
  virtual void Reset(void); 

  /// Print
  virtual std::ostream& Print(std::ostream& out = std::cout, int level=0) const;

private:

  /// Used by copy ctor.
  void Copy(emcLCTofFEM&) const;

#ifndef __CINT__ 
 private:
  LCTof  fLCTof;
#endif

};

#endif
