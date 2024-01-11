#ifndef __EMCGeaCLUSTER_H__
#define __EMCGeaCLUSTER_H__

#include <phool.h>
#include "EmcClusterLocalExt.h"
#include <iostream.h>

static unsigned int EMCNGeaCLUS = 200;

class EmcGeaCluster : public EmcClusterLocalExt
{

 public:

  virtual ~EmcGeaCluster(){}
  virtual int  get_multi_embed_index(int iclus) const 
    { PHOOL_VIRTUAL_WARNING; return -888;}  
  virtual void set_multi_embed_index(int iclus,int ival)
  { PHOOL_VIRTUAL_WARNING; } 

  virtual float get_real_event_zvertex() 
    {PHOOL_VIRTUAL_WARNING; return -888;}  
  virtual void set_real_event_zvertex(float val) 
    { PHOOL_VIRTUAL_WARNING; } 
 
  virtual int get_real_event_centrality() 
    {PHOOL_VIRTUAL_WARNING; return -888;}
  virtual void set_real_event_centrality(int val)
    {PHOOL_VIRTUAL_WARNING;}

  virtual int get_real_event_emc_multiplicity() 
    { PHOOL_VIRTUAL_WARNING; return -888;}
  virtual void set_real_event_emc_multiplicity(int val) 
    { PHOOL_VIRTUAL_WARNING;}
 
  virtual float get_real_event_bbcT0() 
    {PHOOL_VIRTUAL_WARNING; return -888;}  
  virtual void set_real_event_bbcT0(float val) 
    { PHOOL_VIRTUAL_WARNING; } 

  virtual int get_geapart(int iclus, int which) { cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}
  virtual int get_pid(int iclus,int which) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}
  virtual float get_ptot(int iclus,int which) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}
  virtual float get_edep(int iclus,int which)  {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}
  virtual float get_dist_closest_real_pc3(int iclus) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}
  virtual float get_dist_closest_gea_pc3(int iclus) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}
  virtual float get_dist_closest_real_cgl(int iclus) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}
  virtual float get_dist_closest_gea_cgl(int iclus) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}
  
  virtual void set_geapart(int iclus,int which, int inpart){ cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; } 
  virtual void set_pid(int iclus,int which, int inpid){ cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; } 
  virtual void set_ptot(int iclus,int which, float inptot){ cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; } 
  virtual void set_edep(int iclus,int which, float inedep){ cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; } 
  virtual void set_dist_closest_real_pc3(int iclus,float inval){ cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; } 
  virtual void set_dist_closest_gea_pc3(int iclus,float inval){ cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; } 
  virtual void set_dist_closest_real_cgl(int iclus,float inval){ cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; } 
  virtual void set_dist_closest_gea_cgl(int iclus,float inval){ cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; } 

  virtual float get_vtx_xyz(const unsigned int iclus, const short i){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -9999.;}
  virtual void set_vtx_xyz(const unsigned int iclus, const short i, const float rval){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING;}

  virtual float get_truexyz(const unsigned int iclus, const short i){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -9999.;}
  virtual void set_truexyz(const unsigned int iclus, const short i, const float rval){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING;}

  virtual float get_edep_nom(unsigned int iclus, unsigned int which)
    {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}
  virtual void set_edep_nom(unsigned int iclus,unsigned int which, float inedep)
    {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING;}

  virtual float get_etof(unsigned int iclus)
    {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}
  virtual void set_etof(unsigned int iclus, float inedep)
    {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; }

  virtual void identify(ostream &os=cout) const
    {
      os << "identify yourself: virtual EmcGeaCluster object" << endl;
      return;
    }
  virtual void * get_SnglItem(int nlus) {return NULL;}

  virtual unsigned int get_EmcNGeaCluster() {return 0;}
  virtual void set_EmcNGeaCluster(unsigned int nclus) {return;}
  virtual void AddEmcGeaCluster(unsigned int iclus) {return;}
  
  ClassDef(EmcGeaCluster,1)
    
    };

#endif // __EMCGeaCLUSTER_H__





