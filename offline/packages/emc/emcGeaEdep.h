#ifndef __EMC_GEAEDEP_H__
#define __EMC_GEAEDEP_H__





#include <iostream>
#include <cmath>

#include <PHObject.h>
#include <TObject.h>

#include <EmcIndexer.h>
#include <emctypes.h>
#include <emcContentT.h>


class emcGeaEdep: public TObject, public emcContentT<emcGeaEdep> {
public:
  emcGeaEdep(){ geaedep = edep = 0; geatof = tof = NAN; }
  emcGeaEdep(const emcGeaEdep & e){ this->copy(&e); }
  emcGeaEdep & operator += (const emcGeaEdep & d);
  emcGeaEdep * clone() const { return new emcGeaEdep(*this); }  
  void copy(emcGeaEdep const * from);

  emc_towerid_t get_towerid(){ return EmcIndexer::TowerID(swkey); }
  emc_trkno_t get_trkno(){ return trkno; }
  
public:
  /* source */
  long trkno;
  int input;
  
  /* destination */
  arm_t arm;
  sector_t sector;
  sector_type_t type;
  short smodind;
  short towerind;
  long swkey, hwkey;
  
  float x, y, z;
  int iy, iz;
  
  float edep, geaedep; // the modified (ie attenuated) and the
  float tof, geatof;   //   original (gea) values



public:
  const static bool __buildhash = false;
  typedef int key_type;
  key_type get_key() const { return 0; }


  ClassDef(emcGeaEdep, 0)
};



std::ostream & operator << (std::ostream & s, const emcGeaEdep & deposit);
  

typedef  emcGeaEdep xdep_t ;


#ifndef __CINT__

struct xdep_adder : public std::unary_function<xdep_t const *, void>, public xdep_t {
  xdep_adder(xdep_t const * d): xdep_t(*d) { edep = geaedep = 0; }
  void operator()(xdep_t const * x) { operator += ( *x ); }
};



struct xdep_more: public std::binary_function<double, double, bool> {
  bool operator()(xdep_t * const & a, xdep_t * const & b){
    return a->edep > b->edep;
  }
};


struct xdep_less: public std::binary_function<double, double, bool> {
  bool operator()(xdep_t * const & a, xdep_t * const & b){
    return a->edep < b->edep;
  }
};

#endif 





#endif /* ! __EMC_GEAEDEP_H__ */

