// ===============
// FILE: SvxGhit.h
// ===============

#ifndef __SVXGHIT_HH_
#define __SVXGHIT_HH_

#include <SvxHit.h>

/**
 * @brief  Temporary(?) implementation of Silicon GEANT hits.
 *
 * This will be superceded by additions to offline/packages/gea.
 * @date  Created on 9/8/03 by Jeffery Mitchell.
 * @date  Modified by V. L. Rykov on 07-Feb-2004
 */
class SvxGhit : public SvxHit
{

 public:
  SvxGhit(SvxGhit* ghit = NULL) : SvxHit(ghit)
    {/*std::cout << "SvxGhit object created" << std::endl;*/}
  virtual ~SvxGhit(){/*std::cout << "SvxGhit object destroyed" << std::endl;*/}

  // Standard functions of all inheritors of PHObject classes...
  // """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: virtual SvxGhit object" << std::endl;
  }

  // Set the values in the SvxGhit...
  // """"""""""""""""""""""""""""""""
  virtual void set_isubevent (const int val)   {PHOOL_VIRTUAL_WARN("isubevent ");}
  virtual void set_nfile     (const int val)   {PHOOL_VIRTUAL_WARN("nfile     ");}
  virtual void set_mctrack   (const int val)   {PHOOL_VIRTUAL_WARN("mctrack   ");}
  virtual void set_idPart    (const int val)   {PHOOL_VIRTUAL_WARN("idPart    ");}
  virtual void set_track     (const int val)   {PHOOL_VIRTUAL_WARN("track     ");}
  virtual void set_dele      (const float val) {PHOOL_VIRTUAL_WARN("dele      ");}
  virtual void set_tof       (const float val) {PHOOL_VIRTUAL_WARN("tof       ");}
  virtual void set_xyzglobal (const int ind, const float val) 
    { PHOOL_VIRTUAL_WARN("xyzglobal   ") ;}
  virtual void set_xyzlocalin  (const int ind, const float val) 
    { PHOOL_VIRTUAL_WARN("xyzlocalin  ") ;}
  virtual void set_xyzlocalout (const int ind, const float val) 
    { PHOOL_VIRTUAL_WARN("xyzlocalout ") ;}
  virtual void set_pmomxyz     (const int ind, const float val) 
    { PHOOL_VIRTUAL_WARN("pmomxyz     ") ;}


  // Get the values from the SvxGhit...
  // """"""""""""""""""""""""""""""""""
  virtual int get_isubevent    () const {PHOOL_VIRTUAL_WARN("isubevent "); return -9999 ;}
  virtual int get_nfile        () const {PHOOL_VIRTUAL_WARN("nfile     "); return -9999 ;}
  virtual int get_mctrack      () const {PHOOL_VIRTUAL_WARN("mctrack   "); return -9999 ;}
  virtual int get_idPart       () const {PHOOL_VIRTUAL_WARN("idPart    "); return -9999 ;}
  virtual int get_track        () const {PHOOL_VIRTUAL_WARN("track     "); return -9999 ;}
  virtual float get_dele       () const {PHOOL_VIRTUAL_WARN("dele      "); return -9999.;}
  virtual float get_tof        () const {PHOOL_VIRTUAL_WARN("tof       "); return -9999.;}
  virtual float get_xyzglobal   (const unsigned int ind) const
    {PHOOL_VIRTUAL_WARN("xyzglobal   "); return -9999.;}
  virtual float get_xyzlocalin  (const unsigned int ind) const
    {PHOOL_VIRTUAL_WARN("xyzlocalin  "); return -9999.;}
  virtual float get_xyzlocalout (const unsigned int ind) const
    {PHOOL_VIRTUAL_WARN("xyzlocalout "); return -9999.;}
  virtual float get_pmomxyz   (const unsigned int ind)   const
    {PHOOL_VIRTUAL_WARN("pmomxyz     "); return -9999.;}

  // Methods
  // """""""
  virtual SvxHit* Clone()           { PHOOL_VIRTUAL_WARN("Clone()"); return 0; }
  virtual void    Copy(SvxHit* hit) { PHOOL_VIRTUAL_WARN("Copy()"); }

  ClassDef(SvxGhit, 1);
};
#endif
