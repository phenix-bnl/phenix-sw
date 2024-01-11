#ifndef __EMCFEMTUPLE_H__
#define __EMCFEMTUPLE_H__

#include <vector>
#include <iosfwd>
#ifndef __EMCMANAGEABLE_H__
#include "emcManageable.h"
#endif
#ifndef __EMCCALFEM_H__
#include "emcCalFEM.h"
#endif
#ifndef __PHTIMESTAMP_H__
#include "PHTimeStamp.h"
#endif

/** (OLD) Base class for calibration data storage for several FEMs.
 *
 *  \deprecated A FEMtuple is basically a vector of emcCalFEM, and as such
 *  is quite easy to implement directly in user code (currently
 *  it's only used in the calibrator, a class which will also
 *  soon be revisited anyway...). Thus the following documentation
 *  might not be really accurate.
 *
 * @ingroup oldemccalib
 */

class emcFEMtuple : public emcManageable
{

public:

  emcFEMtuple();
  emcFEMtuple(const char* name, const char* title, const char* classname)
    : emcManageable(name, title, classname), fOwner(true)
  {
  }

  virtual ~emcFEMtuple();

public:

  PHTimeStamp Get1()
  {
    return fLastStartTime;
  }
  PHTimeStamp Get2()
  {
    return fFirstEndTime;
  }


  /** Add an emcFEMtuple to this one.
      By now, it is checked that fem.GetSize()==1, and that
      fem.IsOwner()==true.
      After this call, fem.IsOwner() will be false.
  */
  virtual bool Add(emcFEMtuple& fem);

  /** Append one FEM, and return a pointer to the created emcCalFEM.
      @param absPosition : the fem absolute position
      @param t1 : start-of-validity period
      @param t2 : end-of-validity period.
  */
  virtual emcCalFEM* AppendFEM(int absPosition,
			       const PHTimeStamp& t1,
			       const PHTimeStamp& t2);

  /// Check if a FEM index is valid for this object
  virtual bool CheckIndex(int ifem) const
  {
    return (ifem >= 0 && ifem < static_cast<int>(fFEMs.size()));
  }

  /// Return a given Calibration FEM.
  virtual emcCalFEM* GetFEM(int ifem);

  /// Return the number of channels handled by this object
  virtual int GetNumberOfChannels(void) const;

  /// Return the number of FEM handled by this object
  virtual int GetNumberOfFEMs(void) const
  {
    return fFEMs.size();
  }

  virtual int GetXmin(int ifem = 0) const;
  virtual int GetXmax(int ifem = 0) const;

  /// Return the number of FEM hold by this object
  virtual int GetSize(void) const
  {
    return GetNumberOfFEMs();
  }

  /// get value method for time-independant objects
  virtual float getValue(int ichannel) const
  {
    return ( !fFEMs.empty() ) ?
      fFEMs[ichannel / 144]->getValue(ichannel % 144) : 0;
  }

  /// get value method for time-dependant objects
  virtual float getValue(int ichannel, int thetime) const
  {
    return (!fFEMs.empty()) ?
      fFEMs[ichannel / 144]->getValue(ichannel % 144, thetime) : 0;
  }

  /// get value method with a kind (e.g. for pedestals)
  virtual float getValue(int ichannel, int amucell, const char* kind) const
  {
    return (!fFEMs.empty()) ?
      fFEMs[ichannel / 144]->getValue(ichannel % 144, amucell, kind) : 0;
  }

  /// get value method with a kind (e.g. for pedestals)
  virtual float getValue(int ichannel, int amucell, const std::string& kind) const
  {
    return (!fFEMs.empty()) ?
      fFEMs[ichannel / 144]->getValue(ichannel % 144, amucell, kind) : 0;
  }

  /// get value method with a kind, but no cell-dependence.
  virtual float getValue(int ichannel, const char* kind, float& out) const
  {
    return (!fFEMs.empty()) ?
      fFEMs[ichannel / 144]->getValue(ichannel % 144, kind, out) : 0;
  }

  /// Return draft status
  virtual bool IsDraft(void) const;

  /// Return the ownership
  virtual bool IsOwner(void) const
  {
    return fOwner;
  }

  /// Tells if this object is still valid at that time.
  virtual bool IsValid(const PHTimeStamp& when) const;

  /// Tells if values for one fem of this object are valid at that time
  virtual bool IsValid(const PHTimeStamp& when, int ifem) const;

  /// Print (for debug purposes)
  virtual void Print(int level = 0);

  ///
  virtual std::ostream& Print(std::ostream& out, int level);

  /** Replace a given FEM by a FEMtuple containing only one FEM.
      Will fail in any of the following situations :
      a) fem.GetNumberOfFEMs()==1
      b) (ifem)th fem does not exist in this object
      c) fem is not owner of its own FEMs array 
      (because WE need to take ownership)
      d) we are not owner of our own FEMs array.
  */
  virtual bool ReplaceFEM(int ifem, emcFEMtuple& fem);

  /// Reset the object
  virtual void Reset(void);

  /// Set the draft status of this femtuple
  virtual void SetDraft(bool draft);

  virtual bool SetXmin(int xmin, int ifem = 0);
  virtual bool SetXmax(int xmax, int ifem = 0);

  /// Set ownership
  void SetOwnership(bool owner = true)
  {
    fOwner = owner;
    }

    /// update value method with a kind (e.g. for pedestals)
    virtual void updateValue(int ichannel, int amucell, float newValue, const char* kind) const
      {
        if (!fFEMs.empty())
          {
            fFEMs[ichannel / 144]->updateValue(ichannel % 144, amucell, newValue, kind);
          }
      }

  protected:

#ifndef __CINT__

    std::vector<emcCalFEM*> fFEMs;

#endif

    bool fOwner;

    PHTimeStamp fLastStartTime;
    PHTimeStamp fFirstEndTime;

  };

#endif // #ifndef __emcFEMtuple_h__
