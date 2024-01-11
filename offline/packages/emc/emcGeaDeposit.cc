//////////////////////////////////////////////////////////////////////////////////
//
// class that describes energy deposit in emcal. instances of this class are 
// produced by EmcGeaRawDataSimMaker. small deposits proveded by geant (and 
// represented by EmcGeaEdep) are added an stored in this class.
//
// later the instances are assigned to the appropiate emcGeaTrackContent, 
// and stored within them (they are also written to disc as part of the 
// corresponding track).
//
//////////////////////////////////////////////////////////////////////////////////



#include <emctypes.h>

#include <emcGeaDeposit.h>
#include <emcGeaDepositv1.h>
#include <cassert>

ClassImp(emcGeaDeposit);
ClassImp(emcGeaDeposit::key_type);
template class emcContentT< emcGeaDeposit >;
const bool emcGeaDeposit::__buildhash;


emcGeaDeposit * emcGeaDeposit::createdef(){ return new emcGeaDepositv1; }



emcGeaDeposit * emcGeaDeposit::createdef(const emcGeaDeposit::key_type & k){
  emcGeaDeposit * ret = createdef();
  ret->set_trkno(k.trkno);
  ret->set_towerid(k.towerid);
  ret->set_clusterid(k.clusterid);
  return ret;
}




void emcGeaDeposit::copy(emcGeaDeposit const * from){
  set_trkno( from->get_trkno() );
  set_towerid( from->get_towerid() );
  set_clusterid( from->get_clusterid() );

  datatype_t type = GEA;
  set_edep( from->get_edep(type), type );
  set_tof( from->get_tof(type), type );

  type = ORIG;
  set_edep( from->get_edep(type), type );
  set_tof( from->get_tof(type), type );

  type = CALIB;
  set_edep( from->get_edep(type), type );
  set_tof( from->get_tof(type), type );
}



bool emcGeaDeposit::isSpecial() const {
  return 
    isSpecialClusterid( get_clusterid() ) ||
    isSpecialTowerid( get_towerid() ) ||
    isSpecialTrkno( get_trkno() )
    ;
}




emcGeaDeposit & emcGeaDeposit::operator += (emcGeaDeposit const & dep){
  assert( get_trkno() == dep.get_trkno() );
  assert( get_towerid() == dep.get_towerid() );
  assert( get_clusterid() == dep.get_clusterid() );

  set_gea_edep( get_gea_edep() + dep.get_gea_edep() );
  set_gea_tof(  get_gea_tof() < dep.get_gea_tof() ? get_gea_tof() : dep.get_gea_tof()  );

  set_orig_edep( get_orig_edep() + dep.get_orig_edep() );
  set_orig_tof(  get_orig_tof() < dep.get_orig_tof() ? get_orig_tof() : dep.get_orig_tof()  );

  set_edep( get_edep() + dep.get_edep() );
  set_tof(  get_tof() < dep.get_tof() ? get_tof() : dep.get_tof()  );

  return *this;
}
