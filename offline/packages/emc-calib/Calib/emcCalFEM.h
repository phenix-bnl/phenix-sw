#ifndef __EMCCALFEM_H__
#define __EMCCALFEM_H__

#ifndef __EMCMANAGEABLE_H__
#include "emcManageable.h"
#endif
#ifndef __PHTIMESTAMP_H__
#include "PHTimeStamp.h"
#endif
#include <string>
#include <iostream>
#include <ctime>

/** (ABC) Calibration data storage object for a single FEM.
 
An \c emcCalFEM is characterized by :
- an (integer) absolute position
- a (string) description
- a validity period (2 timestamps)
 
It's an emcManageable, so it can be read/write through
the emcDataManager.
 
To access calibration data values, see getValue(Fast) methods.
 
As this class is virtual (and ctors are protected), you cannot
create objects of this class. Only for subclasses you can do so.
See the emcCalFEMFactory class for a generic way of 
creating such objects.

@ingroup interface
@ingroup calibration
*/

class emcCalFEM : public emcManageable
{
public:

  /// Dtor (virtual as this is a base class.)
  virtual ~emcCalFEM();

  virtual emcCalFEM* clone(void) const = 0;

  virtual emcCalFEM* create(void) const = 0;

  /// Comparison.
  bool operator==(const emcCalFEM& obj) const
  {
    return IsEqual(obj);
  }

  bool operator!=(const emcCalFEM& obj) const
  {
    return !IsEqual(obj);
  }

  /**
   *  The absolute FEM position. 
   *  Ranges from 0 to EmcIndexer::MaxNumberOfFEMs(). 
   */
  int AbsolutePosition(void) const
  {
    return fAbsPosition;
  }

  /// Default value to be returned by getValue methods.
  virtual float DefaultReturnValue(void) const
  {
    return 0.0;
  }

  /// Get the description of this calfem.
  std::string Description(void) const
  {
    return fDescription;
  }

  /** emcCalFEM "isa" Manageable, so it must have this one.
      But it's also a generic base class, so it's still pure virtual. */
  virtual const char* GetCategory(void) const = 0;

  /**@name Get value methods.
     Two categories : 
     - getValue(...) with bound checking on arguments,
     - getValueFast(...) with same arguments, but no bound checking.
  */ 
  //@{
  /**@name getValue = with bound checking.*/
  //@{

  /// with only one parameter = channel index
  virtual float getValue(int ichannel) const
  {
    return getValueFast(ichannel);
  }

  /// two dim. get value
  virtual float getValue(int ichannel, time_t x) const
  {
    return getValueFast(ichannel, x);
  }

  /// two dim. get value
  virtual float getValue(int ichannel, int x) const
  {
    return getValueFast(ichannel, x);
  }

  /// get value method with a kind (e.g. for pedestals)
  virtual float getValue(int ichannel, int amucell, const char* kind) const
  {
    return getValueFast(ichannel, amucell, std::string(kind));
  }

  /// same as above with const char* -> string (avoid temporary object).
  virtual float getValue(int /*ichannel*/, int /*amucell*/, 
			 const std::string& /*kind*/) const
  {
    return DefaultReturnValue();
  }

  /// same as above without cell-dependence
  virtual float getValue(int /*i*/, const std::string& /*kind*/, 
			 float& output) const
  {
    output = 0.0;
    return DefaultReturnValue();
  }

  /// same as above with std::string -> char*
  virtual float getValue(int i, const char* kind, float& output) const
  {
    return getValue(i, std::string(kind), output);
  }
  //@}

  //@name getValueFast = without bound checking.*/
  //@{

  /// See getValue(int).
  virtual float getValueFast(int /*ichannel*/) const
  {
    return DefaultReturnValue();
  }

  /// See getValue(int, time_t).
  virtual float getValueFast(int /*ichannel*/, time_t /*x*/) const
  {
    return DefaultReturnValue();
  }

  /// See getValue(int, int).
  virtual float getValueFast(int /*ichannel*/, int /*x*/) const
  {
    return DefaultReturnValue();
  }

  /// See getValue(int,int,const char*).
  virtual float getValueFast(int ichannel, int amucell,
			     const char* kind) const
  {
    return getValueFast(ichannel, amucell, std::string(kind));
  }

  /// See getValue(int,int,const std::string&).
  virtual float getValueFast(int /*ichannel*/, int /*amucell*/, 
			     const std::string& /*kind*/)
    const
  {
    return DefaultReturnValue();
  }

  /// See getValue(int, const std::string&, float&).
  virtual float getValueFast(int /*ichannel*/, const std::string& /*kind*/, 
			     float& output) const
  {
    output = 0.0;
    return DefaultReturnValue();
  }

  /// See getValue(int,const char*,float&).
  virtual float getValueFast(int i, const char* kind, float& output) const
  {
    return getValueFast(i, std::string(kind), output);
  }

  //@}
  //@}

  /// Get the end-of-validity time
  const PHTimeStamp& GetEndValTime(void) const
  {
    return fEnd;
  }

  /// Return the number of channels (should usually be 144)
  virtual size_t GetNumberOfChannels(void) const = 0;

  /// Get the start-of-validity time
  const PHTimeStamp& GetStartValTime(void) const
  {
    return fStart;
  }

  /// Is this calfem a default one ?
  bool IsDefault(void) const
  {
    return fDefault;
  }

  /// Is this FEM draft only ?
  bool IsDraft(void) const
  {
    return fDraft;
  }

  /// Comparison.
  virtual bool IsEqual(const emcCalFEM&) const = 0;

  /// Is this FEM data valid at a given time ?
  virtual bool IsValid(const PHTimeStamp& /*cwhen*/) const;

  /// Pin Number (might not be available - in which case 0 is returned).
  int PinNumber(void) const
  {
    return fPinNumber;
  }

  /// Print (for debug purposes).
  virtual std::ostream& Print(std::ostream& out = std::cout, int level = 0) const;

  /// Reset (delete internal storage)
  virtual void Reset(void) = 0;

  /// Set description
  void SetDescription(const char* description)
  {
    fDescription = std::string(description);
  }

  /// Set the default status.
  void SetDefault(bool def = false)
  {
    fDefault = def;
  }

  /// Set the draft status
  void SetDraft(bool draft = false)
  {
    fDraft = draft;
  }

  /// Set the number of channels
  virtual void SetNumberOfChannels(int /*nchannels*/ = 144)
  {}

  /// Set the pin number.
  void SetPinNumber(int pinNumber)
  {
    fPinNumber = pinNumber;
  }

  /// Set the validity period of this FEM
  void SetValidityPeriod(const PHTimeStamp& t1, const PHTimeStamp& t2)
  {
    fStart = t1;
    fEnd = t2;
  }

  /// Set the version number.
  void SetVersion(int version = 0)
  {
    fVersion = version;
  }

  /**@name About X limits.
     (where X=(t-t0)=incremental tics for PbSc
     and X=(r-r0)=incremental run number for PbGl.*/ 
  //@{
  ///

  time_t GetXmin(void) const
  {
    return fXmin;
  }
  ///
  time_t GetXmax(void) const
  {
    return fXmax;
  }
  ///
  void SetXmin(time_t xmin)
  {
    fXmin = xmin;
  }
  ///
  void SetXmax(time_t xmax)
  {
    fXmax = xmax;
  }

  //@}

  /// Get the number of channels of this FEM.
  size_t size(void) const
  {
    return GetNumberOfChannels();
  }

  /** Update value method with a kind (e.g. for pedestals).
      This method is used to allow for in-processing update of the stored 
      pedestal values - can be important for timing. It is equivalent to 
      running mean computation with assumption that the typical RMS of the 
      cell-based pedestal is 3 counts (new entry contribution is 10% 
      compared to accumulated average). */
  virtual void updateValue(int /*ichannel*/, int /*amucell*/,
			   float /*newValue*/, const char* /*kind*/)
  {
  }


  /// Version number of this object.
  int Version(void) const
  {
    return fVersion;
  }

  //_________________________________________________________________

  /**@name Static methods.*/
  //@{
  /** Make a single integer from four characteric values of one FEM.
      post_pre and tac_pre are no longer used, but kept for backward
      compatibility.*/
  static int FEMCode(int absPosition, int pinNumber,
		     int post_pre, int tac_pre);

  /** Decode FEM code.
      post_pre and tac_pre are no longer used, but kept for backward
      compatibility.*/
  static void FEMDecode(int id, int& absPosition, int& pinNumber,
			int& post_pre, int& tac_pre);

  /// Used to keep track of the number of emcCalFEM object (for debug).
  static int NumberOfInstances(void)
  {
    return fgNemcCalFEM;
  }
  
  //@}
  
 protected:
  
  /// Ctor with only position given.
  emcCalFEM(int absPosition);
  
  /// Ctor with position and validity period given.
  emcCalFEM(int absPosition,
	    const PHTimeStamp& tStart,
	    const PHTimeStamp& tEnd);
  
  /// Copy ctor.
  emcCalFEM(const emcCalFEM&);
  
  /// Assignment operator.
  emcCalFEM& operator=(const emcCalFEM&);
  
  /// Used by copy ctor and assignement operator.
  void Copy(emcCalFEM&) const;
  
  private:
  PHTimeStamp fStart;
  PHTimeStamp fEnd;
  int fAbsPosition;
#ifndef __CINT__
  
  time_t fXmin;
  time_t fXmax;
#endif
  
  bool fDraft;
  int fVersion;
  std::string fDescription;
  int fPinNumber;
  bool fDefault;
  
 public:
    static int fgNemcCalFEM;
  
};

inline
std::ostream& operator<<(std::ostream& out, const emcCalFEM& tfem)
{
  return tfem.Print(out, 1);
}

#endif
