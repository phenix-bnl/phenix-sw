#ifndef __MUPCSNGLCLUSTERV1_H_
#define __MUPCSNGLCLUSTERV1_H_

#include "PHObject.h"
#include "MuPCSnglCluster.h"

class MuPCSnglClusterv1 : public MuPCSnglCluster
{
 public:
  MuPCSnglClusterv1();
  MuPCSnglClusterv1(MuPCSnglClusterv1*track);  
  virtual ~MuPCSnglClusterv1() {}

  // Here are the very explicit set routines...
  void set_phi    (const float val)   {phi = val;}
  void set_xglo    (const float val)   {xglo = val;}
  void set_yglo (const float val)   {yglo = val;}
  void set_zglo   (const float val)   {zglo = val;}
  void set_idorigin   (const short val)   {idorigin = val;}
  void set_idpart   (const short val)   {idpart = val;}
  void set_idparent   (const short val)   {idparent = val;}
  void set_ptot   (const float val)   {ptot = val;}
  void set_ptheta   (const float val)   {ptheta = val;}
  void set_pphi   (const float val)   {pphi = val;}
  void set_r_vertex   (const float val)   {r_vertex = val;}
  void set_z_vertex   (const float val)   {z_vertex = val;}
  void set_ptotpri   (const float val)   {ptotpri = val;}
  void set_pthetpri   (const float val)   {pthetpri = val;}
  void set_pphipri   (const float val)   {pphipri = val;}
  void set_z0vertex   (const float val)   {z0vertex = val;}

  // Here are the very explicit "get" routines...

  float get_phi()  const  {return phi;}
  float get_xglo() const   {return xglo;}
  float get_yglo() const   {return yglo;}
  float get_zglo() const   {return zglo;}
  short get_idorigin() const   {return idorigin;}
  short get_idpart() const   {return idpart;}
  short get_idparent() const   {return idparent;}
  float get_ptot() const   {return ptot;}
  float get_ptheta()const    {return ptheta;}
  float get_pphi()const    {return pphi;}
  float get_r_vertex()const    {return r_vertex;}
  float get_z_vertex()const    {return z_vertex;}
  float get_ptotpri()const    {return ptotpri;}
  float get_pthetpri()const    {return pthetpri;}
  float get_pphipri()const    {return pphipri;}
  float get_z0vertex()const    {return z0vertex;}

 protected:
  short idpart;
  short idparent;
  short idorigin;
  float phi;
  float xglo;
  float yglo;
  float zglo;
  float ptot;
  float ptheta;
  float pphi;
  float r_vertex;
  float z_vertex;
  float ptotpri;
  float pphipri;
  float pthetpri;
  float z0vertex;

  ClassDef(MuPCSnglClusterv1,1)
};

#endif

