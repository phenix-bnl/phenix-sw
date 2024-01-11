#ifndef __EMCWALKTOFFEM_H__
#define __EMCWALKTOFFEM_H__

#ifndef __EMCCALFEM_H__
#include "emcCalFEM.h"
#endif

#include <vector>

/** Stores walk tof values for one FEM. 
@ingroup calibration
*/

class emcWalkTofFEM : public emcCalFEM
{

 public:

  /// ctor
  explicit emcWalkTofFEM(int absPosition);

  /// ctor
  emcWalkTofFEM(int absPosition,  
	    const PHTimeStamp& t1, const PHTimeStamp& t2);

  /// copy ctor
  emcWalkTofFEM(const emcWalkTofFEM&);

  /// assignment operator.
  emcWalkTofFEM& operator=(const emcWalkTofFEM&);

  /// dtor
  virtual ~emcWalkTofFEM();

  virtual emcWalkTofFEM* clone(void) const 
    { return new emcWalkTofFEM(*this); }

  virtual emcWalkTofFEM* create(void) const 
  {
    return new emcWalkTofFEM(AbsolutePosition(),
			     GetStartValTime(),
			     GetEndValTime());
  }

#ifndef __CINT__
  typedef std::vector<float> vfloat;
  typedef std::vector<vfloat> WalkTof;
  typedef std::vector<float>::iterator itvfloat;
  typedef std::vector<float>::const_iterator citvfloat;
  typedef std::vector<vfloat>::iterator itWalkTof;
  typedef std::vector<vfloat>::const_iterator citWalkTof;
#endif

  /// Append Tof Least count values for one channel
  void AppendOneChannel( const float value1, const float value2);


  /// Create default object with 144 channels
  static emcWalkTofFEM* Default(const int& absPosition, 
                              const PHTimeStamp& t1, 
                              const PHTimeStamp& t2);

  ///
  virtual const char* GetCategory(void) const { return "WalkTofs"; }

  /// Get number of channels handled by this object.
  virtual size_t GetNumberOfChannels(void) const { return fWalkTof.size(); }

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
  void Copy(emcWalkTofFEM&) const;

#ifndef __CINT__ 
 private:
  WalkTof  fWalkTof;
#endif

};

#endif
