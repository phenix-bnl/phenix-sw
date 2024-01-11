#ifndef __EMCGeaPARTICLE_H__
#define __EMCGeaPARTICLE_H__

#include <iostream.h>
#include "phool.h"
#include "PHObject.h"

static unsigned int EMCNGeaPART = 100;

class EmcGeaParticle : public PHObject
{
  public :
    virtual ~EmcGeaParticle(){}

  virtual void Reset()
    {
      cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << endl;
      return;
    }

  virtual int isValid() const
    {
      cout << PHWHERE << "isValid() not implemented by daughter function" << endl;
      return 0;
    }

  virtual void identify(ostream &os=cout) const
    {
      os << "identify yourself: virtual EmcGeaParticle object" << endl;
      return;
    }


  virtual short   get_pid(int ipart) const {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -9999;}
  virtual void set_pid(int ipart,short ival) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return;}

  virtual short   get_pidparent(int ipart) const {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -9999;}
  virtual void set_pidparent(int ipart,short ival) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return;}  

  virtual short   get_fConversion(int ipart) const {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -9999;}
  virtual void set_fConversion(int ipart,short ival) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return;}

  virtual float get_ZVertex(int ipart) const {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -9999.;}
  virtual void set_ZVertex(int ipart, float rval) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return;}

  virtual float get_px(int ipart) const {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -9999.;}
  virtual void set_px(int ipart, float rval) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return;}

  virtual float get_py(int ipart) const {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -9999.;}
  virtual void set_py(int ipart, float rval) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return;}

  virtual float get_pz(int ipart) const {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -9999.;}
  virtual void set_pz(int ipart, float rval) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return;}

  virtual float get_e(int ipart) const {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -9999.;}
  virtual void set_e(int ipart, float rval) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return;}


  virtual int get_multi_embed_index(int ipart){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -9999;}
  virtual void set_multi_embed_index(int ipart, int rval){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING;}
  	  
  virtual float get_impxyz(int ipart, int ixyz){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -9999.;}
  virtual void set_impxyz(int ipart, int ixyz, float rval){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING;}
  	  
  virtual int get_clusid(int ipart, int which){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -9999;}
  virtual void set_clusid(int ipart, int which, int rval){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING;}

  virtual int  get_twrhit(int ipart){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}
  virtual unsigned int get_parent_ptr(int ipart){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return 888;}
  virtual float get_ptot(int ipart) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}

  virtual void set_twrhit(int ipart, int val){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING;}
  virtual void set_parent_ptr(int ipart, unsigned int val){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING;}
  
  virtual int get_num_geatrk_tracks(int ipart){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}
  virtual int get_ancestry_lvl(int ipart, int entry){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}
  virtual int get_geatrk_itparent(int ipart, int entry) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}
  
  virtual int get_fkin_true_track(int ipart){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;} 
  virtual int get_num_fkin_tracks(int ipart){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}
  virtual int get_fkin_itparent(int ipart, int entry){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -888;}
  
  virtual void set_ancestry_lvl(int ipart, int entry, int rval){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING;}
  virtual void set_geatrk_itparent(int ipart, int entry, int rval){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING;}
  virtual void set_num_geatrk_tracks(int ipart, int rval){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING;} 
  
  virtual void set_fkin_true_track(int ipart, int rval){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING;}
  virtual void set_fkin_itparent(int ipart, int entry, int rval){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING;}
  virtual void set_num_fkin_tracks(int ipart, int rval) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING;}
  
  virtual int get_id(int ipart) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -9999;}
  virtual void set_id(int ipart, int ival) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return;}

  virtual float get_vtx_xyz(int ipart, int ixyz){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return -9999.;}
  virtual void set_vtx_xyz(int ipart, int ixyz, float rval){cout << PHWHERE ; PHOOL_VIRTUAL_WARNING;}  	  

  // ---  
  virtual unsigned int get_EmcNGeaParticle() const { cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return 0;}
  virtual void set_EmcNGeaParticle(unsigned int npart)  {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return;}

  virtual int set_TClonesArraySize(unsigned int npart) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return 0;}
  virtual void AddEmcGeaParticle(unsigned int ipart) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return;}

  virtual void * get_SnglItem(int nlus) {cout << PHWHERE ; PHOOL_VIRTUAL_WARNING; return NULL;}
  
  ClassDef(EmcGeaParticle,1)

};

#endif /* __EMCGeaPARTICLE_H__ */






