//////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////

#include <cassert>

#include <emcGeaDepositv1.h>


ClassImp(emcGeaDepositv1);



void emcGeaDepositv1::Reset(){ 
  trkno = EMC_INVALID_TRKNO;
  towerid = EMC_INVALID_TOWERID;
  clusterid = EMC_INVALID_CLUSTERID;

  geaedep = origedep = edep = NAN; 
  geatof = origtof = tof = NAN; 
  simulated = false;
}


void emcGeaDepositv1::set_edep(float _edep, datatype_t type){
  switch(type){
  case GEA: set_gea_edep(_edep); break;
  case ORIG: set_orig_edep(_edep); break;
  case CALIB: set_calib_edep(_edep); break;
  default: assert(type == GEA  ||  type == ORIG  ||  type == CALIB);
  }
}



float emcGeaDepositv1::get_edep(datatype_t type) const {
  switch(type){
  case GEA: return get_gea_edep(); break;
  case ORIG: return get_orig_edep(); break;
  case CALIB: return get_calib_edep(); break;
  default: assert(type == GEA  ||  type == ORIG  ||  type == CALIB); return NAN;
  }
}



void emcGeaDepositv1::set_tof(float _tof, datatype_t type){
  switch(type){
  case GEA: set_gea_tof(_tof); break;
  case ORIG: set_orig_tof(_tof); break;
  case CALIB: set_calib_tof(_tof); break;
  default: assert(type == GEA  ||  type == ORIG  ||  type == CALIB);
  }
}



float emcGeaDepositv1::get_tof(datatype_t type) const {
  switch(type){
  case GEA: return get_gea_tof(); break;
  case ORIG: return get_orig_tof(); break;
  case CALIB: return get_calib_tof(); break;
  default: assert(type == GEA  ||  type == ORIG  ||  type == CALIB); return NAN;
  }
}







emcGeaDepositv1 * emcGeaDepositv1::split(float frac){
  emcGeaDepositv1 * ret = new emcGeaDepositv1(*this);
  assert( 0 <= frac );
  assert( frac <= 1.0 );

  geaedep *= ( 1.0 - frac );
  ret->geaedep *= frac;
  
  origedep *= ( 1.0 - frac );
  ret->origedep *= frac;

  edep *= ( 1.0 - frac );
  ret->edep *= frac;

  return ret;
}





emcGeaDepositv1 * emcGeaDepositv1::split_by_track(float frac, emc_trkno_t trkno){
  emcGeaDepositv1 * ret = split(frac);
  ret->trkno = trkno;
  return ret;
}


emcGeaDepositv1 * emcGeaDepositv1::split_by_tower(float frac, emc_towerid_t towerid){
  emcGeaDepositv1 * ret = split(frac);
  ret->towerid = towerid;
  return ret;
}


emcGeaDepositv1 * emcGeaDepositv1::split_by_cluster(float frac, emc_clusterid_t clusterid){
  emcGeaDepositv1 * ret = split(frac);
  ret->clusterid = clusterid;
  return ret;
}


