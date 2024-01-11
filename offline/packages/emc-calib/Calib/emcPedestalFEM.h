#ifndef __EMCPEDESTALFEM_H__
#define __EMCPEDESTALFEM_H__

#ifndef __EMCCALFEM_H__
#include "emcCalFEM.h"
#endif

#include <vector>
#include <map>
#include <string>
#include <iostream>

/** Stores pedestal values for one FEM.
 *
 *   This class has 2 versions :
 *   - version 0 for old style pedestals (3 per amu per channel)
 *   - version 1 for new style pedestals (5 per amu per channel).
 *
 * @ingroup calibration
 */

class emcPedestalFEM : public emcCalFEM
{

public:

  /// ctor
  explicit emcPedestalFEM(int absPosition,
			  int version = 0);

  /// ctor
  emcPedestalFEM(int absPosition,
		 const PHTimeStamp& t1, const PHTimeStamp& t2,
		 int version = 0);

  /// copy ctor
  emcPedestalFEM(const emcPedestalFEM& o);

  /// assignment operator.
  emcPedestalFEM& operator=(const emcPedestalFEM&);

  /// dtor
  virtual ~emcPedestalFEM();

  virtual emcPedestalFEM* clone(void) const
  {
    return new emcPedestalFEM(*this);
  }


  virtual emcPedestalFEM* create(void) const
  {
    return new emcPedestalFEM(AbsolutePosition(),
			      GetStartValTime(),
			      GetEndValTime(),
			      Version());
  }

#ifndef __CINT__
public:

  typedef std::vector<int> AmuVector;
  typedef std::vector<AmuVector> ChannelVector;
  typedef std::map<std::string, ChannelVector*> PedMap;
  typedef std::map<std::string, std::vector<int> > PedAverageMap;
  typedef std::map<std::string, std::vector<float> > PedRMSMap;

  /// Append one-kind-pedestal values for one channel
  void AppendOneChannel(const char* ped_type, AmuVector& vec);

  /** Create an object with 144 channels and default values (0).
      The returned pointer is yours.*/
  static emcPedestalFEM* Default(const int& absPosition,
				 const PHTimeStamp& t1,
				 const PHTimeStamp& t2,
				 int version = 0);

  /// Get list of valid types
  void GetListOfValidTypes(std::vector<std::string>& list)
  {
    list = fValidTypes;
  }
#endif

  /// Get number of channels handled by this object.
  virtual size_t GetNumberOfChannels(void) const;

  /// Category of this emcManageable = "Pedestals"
  virtual const char* GetCategory(void) const;

  using emcCalFEM::getValue;
  using emcCalFEM::getValueFast;

  /// same as above with const char* -> string (avoid temporary object).
  virtual float getValue(int ichannel, int amucell, const std::string& kind) const;

  /// Same as above, but with char* -> string
  virtual float getValueFast(int ch_index, int amu_number,
			     const std::string& ped_type) const;

  /// same as above without cell-dependence
  virtual float getValue(int ich, const std::string& kind, float& out) const;

  /// same as above without cell-dependence
  virtual float getValueFast(int ich, const std::string& kind, float& out) const;

  /// Unchecked get of all ped. values (see implementation for warning).
  void GetValues(int ch_index, int amu_number,
		 int& low, int& high, int& tac);

  /// comparison.
  virtual bool IsEqual(const emcCalFEM&) const;

  /// Is ped_type a valid pedestal type ?
  bool IsValidType(const std::string& ped_type) const;

  // Reset the object
  virtual void Reset(void);

  /// Print
  virtual std::ostream& Print(std::ostream& out = std::cout, int level = 0) const;

  /// Get names of valid types for a given version.
  static std::vector<std::string> ValidTypes(int version);

#ifndef __CINT__
private:
  void Copy(emcPedestalFEM&) const;

  private:
    std::vector<std::string> fValidTypes;
    mutable PedMap fPed;
    mutable PedAverageMap fPedAverage;
    mutable PedRMSMap fPedRMS;
#endif

  };

#endif
