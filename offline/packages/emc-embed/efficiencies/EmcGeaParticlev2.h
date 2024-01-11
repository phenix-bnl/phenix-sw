#ifndef __EMCGeaPARTICLEMICROV2_H__
#define __EMCGeaPARTICLEMICROV2_H__

#include <iostream.h>
#include "TClonesArray.h"
#include "phool.h"
#include "EmcGeaParticle.h"

class EmcGeaParticlev2 : public EmcGeaParticle
{

 public:
  EmcGeaParticlev2();
  virtual ~EmcGeaParticlev2();

  void Reset();

  int isValid() const;
  void identify(ostream &os=cout) const;

  short get_pid(int ipart) const;
  void  set_pid(int ipart, short ival);

  short get_pidparent(int ipart) const;
  void  set_pidparent(int ipart, short ival);  
  
  short get_fConversion(int ipart) const;
  void  set_fConversion(int ipart, short ival); 

  float get_ZVertex(int ipart) const;
  void  set_ZVertex(int ipart, float rval);

  float get_px(int ipart) const;
  void  set_px(int ipart, float rval);

  float get_py(int ipart) const;
  void  set_py(int ipart, float rval);

  float get_pz(int ipart) const;
  void  set_pz(int ipart, float rval);

  // total e, not kinetic
  float get_e(int ipart) const;
  void  set_e(int ipart, float rval);

  int  get_multi_embed_index(int ipart);
  void set_multi_embed_index(int ipart, int rval);

  float get_impxyz(int ipart, int ixyz); 
  void  set_impxyz(int ipart, int ixyz, float rval);
  
  int  get_clusid(int ipart, int which); 
  void set_clusid(int ipart, int which, int rval); 

  float get_vtx_xyz(int ipart, int ixyz); 
  void  set_vtx_xyz(int ipart, int ixyz, float rval);

  int  get_twrhit(int ipart);
  unsigned int get_parent_ptr(int ipart);
  
  void set_twrhit(int ipart, int val);
  void set_parent_ptr(int ipart, unsigned int val);

  float get_ptot(int ipart );

  int  get_id(int ipart);
  void set_id(int ipart, int ival);

  int get_num_geatrk_tracks(int ipart);
  int get_ancestry_lvl(int ipart, int entry);
  int get_geatrk_itparent(int ipart, int entry);
  
  int get_fkin_true_track(int ipart);
  int get_num_fkin_tracks(int ipart);
  int get_fkin_itparent(int ipart, int entry); 
  
  void set_ancestry_lvl(int ipart, int entry, int rval);
  void set_geatrk_itparent(int ipart, int entry, int rval);
  void set_num_geatrk_tracks(int ipart, int rval); 
  
  void set_fkin_true_track(int ipart, int rval);
  void set_fkin_itparent(int ipart, int entry, int rval);
  void set_num_fkin_tracks(int ipart, int rval); ;

  unsigned int get_EmcNGeaParticle() const {
    return EmcNGeaPart;
  }
  void set_EmcNGeaParticle(unsigned int npart){
    EmcNGeaPart = npart;
    return;
  }

  void * get_SnglItem(int nclus) 
    {if (nclus < EmcNGeaPart) return EmcGeaPart->At(nclus); return NULL;}

  int set_TClonesArraySize(unsigned int npart);
  void AddEmcGeaParticle(unsigned int ipart);
  

 protected:

  TClonesArray *GetEmcGeaPart() const { return EmcGeaPart;}
  unsigned int EmcNGeaPart;
  TClonesArray *EmcGeaPart;

  ClassDef(EmcGeaParticlev2,1)

};

#endif /* __EMCGeaPARTICLEMICROV2_H__ */





