#ifndef __PHPARTICLE_HH_
#define __PHPARTICLE_HH_


#include "PHObject.h"
#include <iostream>
#include <cmath>

//
//  This version of the PHParticle base class is written
//  so that it contains implemented virtual functions for
//  characteristics that are common to ALL particles.
//
//  Specific PHENIX measured particles (CentralTracks, Muons
//  Photons, etc...) should inherit their common features 
//  from here and add additional virtual functions for
//  fields which are only appropriate to the measurement
//  (e.g. a CentralTrack would have a pc3x while a Muon
//  track would have a maxDepth).
//
//  Note:  For consistency with historical PHENIX conventions,
//  the momentum vector is implemented in spherical coordinates.
//
//                            TKH 3-7-2002
//

class TClonesArray;

class PHParticle : public PHObject
{
 public:
  virtual ~PHParticle() {}

  // Set the values in the particle...
  // These virtual functions should ALL be overridden!
  // If the local version is called by mistake, the user sees a
  // warning on their screen.       THK 3-2-2002
  virtual void set_npart(const unsigned int npart);
  virtual void set_px(const unsigned int itrk, const float val);
  virtual void set_py(const unsigned int itrk, const float val);
  virtual void set_pz(const unsigned int itrk, const float val);
  virtual void set_E(const unsigned int itrk, const float val);
  virtual void set_charge(const unsigned int itrk, const short val);
  virtual void set_PID(const unsigned int itrk, const short val);

  // Get the values from the particle...
  // The virtual base class prints warning then returns crap...
  virtual unsigned int   get_npart() const;
  virtual float get_px(const unsigned int itrk) const;
  virtual float get_py(const unsigned int itrk) const;
  virtual float get_pz(const unsigned int itrk) const;
  virtual float get_E(const unsigned int itrk) const;
  virtual short get_charge(const unsigned int itrk) const;
  virtual short get_PID(const unsigned int itrk) const;

  // Just to be really cool, pt can be calculated here in the base class.
  // The implementation uses the typical SQRT(px*px + py*py) calculation
  // In case your derived class does this calculation more efficiently,
  // I have made the function virtual so you can override it...
  virtual float get_pt(const unsigned int itrk) const {
    return std::sqrt(get_px(itrk)*get_px(itrk) + get_py(itrk)*get_py(itrk));
  }

  // Standard functions of all virtual classes...
  virtual void Reset();
  virtual int isValid() const;
  virtual void identify(std::ostream &os=std::cout) const;
  virtual void ShutUp(const int i = 1);

  //  Container methods:
  //  Unfortunately, many of our single particles are TObjects instead of
  //  PHObjects.  Since PHObject _inherits_ from TObject, the line below
  //  will indeed work for all particles, but it is not entirely elegant.
  virtual TObject* GetSingleParticle(unsigned int /*ipart*/)
  { std::cout << "GetSingleParticle not implemented" << std::endl; return NULL;}
  virtual void AddPHParticle(unsigned int /*ipart*/, TObject*) { std::cout << "AddParticle(obj) not implemented"  << std::endl;}
  virtual PHParticle* clone() const;

 private:
  void warning(const char *fname) const;

  ClassDef(PHParticle,1)
};
#endif /* __PHPARTICLE_HH_ */
