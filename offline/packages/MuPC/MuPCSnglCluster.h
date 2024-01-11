#ifndef __MUPCSNGLCLUSTER_HH_
#define __MUPCSNGLCLUSTER_HH_

#include "PHObject.h"
#include <iostream>

class MuPCSnglCluster : public PHObject
{


 public:
  virtual ~MuPCSnglCluster() {}

  // Set the values in the SnglCluster...
  // These virtual functions should ALL be overridden!
  // If the local version is called by mistake, the user sees a
  // warning on their screen.

  virtual void set_phi    (const float val)   {warning("phi   ");}
  virtual void set_xglo    (const float val)   {warning("xglo   ");}
  virtual void set_yglo (const float val)   {warning("yglo");}
  virtual void set_zglo   (const float val)   {warning("zglo  ");}
  virtual void set_idpart   (const short val)   {warning("idpart  ");}
  virtual void set_idparent   (const short val)   {warning("idparent  ");}
  virtual void set_ptot   (const float val)   {warning("ptot  ");}
  virtual void set_ptheta   (const float val)   {warning("ptheta  ");}
  virtual void set_pphi   (const float val)   {warning("pphi  ");}
  virtual void set_r_vertex   (const float val)   {warning("r_vertex  ");}
  virtual void set_z_vertex   (const float val)   {warning("z_vertex  ");}
  virtual void set_idorigin   (const short val)   {warning("idorigin  ");}
  virtual void set_ptotpri   (const float val)   {warning("ptotpri  ");}
  virtual void set_pthetpri   (const float val)   {warning("pthetpri  ");}
  virtual void set_pphipri   (const float val)   {warning("pphipri  ");}
  virtual void set_z0vertex   (const float val)   {warning("z0vertex  ");}

  // Get the values from the SnglCluster...
  // The virtual base class prints warning then returns crap...
  virtual float get_phi() const  {warning("phi   "); return -9999;}
  virtual float get_xglo()const   {warning("xglo   "); return -9999;}
  virtual float get_yglo()const   {warning("yglo"); return -9999;}
  virtual float get_zglo() const  {warning("zglo  "); return -9999;}
  virtual short get_idpart() const  {warning("idpart  "); return -9999;}
  virtual short get_idparent() const  {warning("idparent  "); return -9999;}
  virtual float get_ptot()const   {warning("ptot  "); return -9999;}
  virtual float get_ptheta()const   {warning("ptheta  "); return -9999;}
  virtual float get_pphi()const   {warning("pphi  "); return -9999;}
  virtual float get_r_vertex()const   {warning("r_vertex  "); return -9999;}
  virtual float get_z_vertex()const   {warning("z_vertex  "); return -9999;}
  virtual short get_idorigin()const   {warning("idorigin  "); return -9999;}
  virtual float get_ptotpri()const   {warning("ptotpri  "); return -9999;}
  virtual float get_pthetpri()const   {warning("pthetpri  "); return -9999;}
  virtual float get_pphipri()const   {warning("pphipri  "); return -9999;}
  virtual float get_z0vertex()const   {warning("z0vertex  "); return -9999;}

 private:
  void warning(const char* field) const;

  ClassDef(MuPCSnglCluster,1)
};
#endif
