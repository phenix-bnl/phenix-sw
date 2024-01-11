//////////////////////////////////////////////////////////////////////////////////
//
// v3 of emcGeaTrackContent. this version _does_ cache some of the data. for a
// non caching version see emcGeaTrackContentv1.
//
//////////////////////////////////////////////////////////////////////////////////

#include <math.h>
#include <stdio.h>

#include <emcGeaTowerContainer.h>
#include <emcGeaTrackContentv3.h>
#include <emcGeaTrackContainer.h>


ClassImp(emcGeaTrackContentv3);


using std::map;




emcGeaTrackContentv3::emcGeaTrackContentv3(emc_trkno_t thetrkno): trkno(thetrkno){
  input = anclvl = pid = -1;
  ekin = xyz[0] = xyz[1] = xyz[2] =
    pxyz[0] = pxyz[1] = pxyz[2] = impxyz[0] = impxyz[1] = impxyz[2] = 0;
  twrhit = 0;
  itparent = EMC_INVALID_TRKNO;
  cacheok = false;
}



emcGeaTrackContentv3::emcGeaTrackContentv3(const emcGeaTrackContentv3 & t): emcGeaTrackContent(t) {
  trkno = t.trkno;
  input = t.input;
  anclvl = t.anclvl;
  pid = t.pid;
  ekin = t.ekin;
  xyz[0] = t.xyz[0]; xyz[1] = t.xyz[1]; xyz[2] = t.xyz[2];
  pxyz[0] = t.pxyz[0]; pxyz[1] = t.pxyz[1]; pxyz[2] = t.pxyz[2];
  impxyz[0] = t.impxyz[0]; impxyz[1] = t.impxyz[1]; impxyz[2] = t.impxyz[2];
  itparent = t.itparent;
  twrhit = t.twrhit;
  
  for(emc_tracklist_t::iterator i = t.daughterlist.begin(); i != t.daughterlist.end(); i++)
    daughterlist.insert(*i);

  for(map<emc_towerid_t, emcGeaDeposit *>::const_iterator i = t.deposits.begin(); i != t.deposits.end(); i++)
    deposits[i->first] = i->second->clone();
  for(emc_towerlist_t::const_iterator i = t.ctowerlist.begin(); i != t.ctowerlist.end(); i++)
    ctowerlist.insert(*i);

  cacheok = false;
}



void emcGeaTrackContentv3::identify(std::ostream& os) const {
  std::streambuf *outbuf = std::cout.rdbuf(os.rdbuf());
  Dump();
  std::cout.rdbuf(outbuf);
};  



void emcGeaTrackContentv3::Reset(){
  emcGeaTrackContent::Reset();

  input = anclvl = pid = -1;
  ekin = xyz[0] = xyz[1] = xyz[2] =
    pxyz[0] = pxyz[1] = pxyz[2] = impxyz[0] = impxyz[1] =impxyz[2] = 0;
  twrhit = 0;
  itparent = EMC_INVALID_TRKNO;
  daughterlist.clear();
  clear_deposits();
}



void emcGeaTrackContentv3::clear_deposits(){
  for(map<emc_towerid_t, emcGeaDeposit *>::const_iterator i = deposits.begin(); i != deposits.end(); i++)
    delete i->second;
  deposits.clear();
  ctowerlist.clear();
  cclusterlist.clear(); cacheok = false;
}

    










template<class T> inline T SQR(T x){ return x * x; }

float emcGeaTrackContentv3::get_ptot() const {
  return sqrt( SQR(pxyz[0]) + SQR(pxyz[1]) + SQR(pxyz[2]) );
}

float emcGeaTrackContentv3::get_pt() const { 
  return sqrt( SQR(pxyz[0]) + SQR(pxyz[1]) );
}

float emcGeaTrackContentv3::get_radius() const {
  return sqrt( SQR(xyz[0]) + SQR(xyz[1]) + SQR(xyz[2]) );
}







float emcGeaTrackContentv3::get_tof(emcGeaDeposit::datatype_t type) const {
  float ret = NAN;

  for(map<emc_towerid_t, emcGeaDeposit *>::const_iterator i = deposits.begin(); i != deposits.end(); i++){
    float twrtof = i->second->get_tof(type);
    if( twrtof != NAN  &&  (ret == NAN  ||  twrtof < ret) ) ret = twrtof;
  }

  return ret;
}



emc_towerlist_t const emcGeaTrackContentv3::get_tower_list() const {
  //  emc_towerlist_t ret;
  //
  //for(map<emc_towerid_t, emcGeaDeposit *>::const_iterator i = deposits.begin(); i != deposits.end(); i++)
  // ret.insert( i->first );
  //
  //  return ret;
  return ctowerlist;
}



float emcGeaTrackContentv3::get_edep_bytower(emc_towerid_t towerid, emcGeaDeposit::datatype_t type) const {
  map<emc_towerid_t, emcGeaDeposit *>::const_iterator i = deposits.find( towerid );
  if( i == deposits.end() ) return 0;
  else return i->second->get_edep(type);
}



void emcGeaTrackContentv3::set_edep_bytower(emc_towerid_t towerid, float edep, emcGeaDeposit::datatype_t type){
  // TODO: should return error instead of adding new deposit 
  // (most field of the new deposit will not be set!
  if( deposits.find(towerid) == deposits.end() ){
    deposits[towerid] = emcGeaDeposit::createdef();
    //    if( towers && towers->find(towerid) ) towers->find(towerid)->add_track( trkno );
    ctowerlist.insert(towerid);
  }
  deposits[towerid]->set_edep(edep, type);
}



float emcGeaTrackContentv3::get_tof_bytower(emc_towerid_t towerid, emcGeaDeposit::datatype_t type) const {
  map<emc_towerid_t, emcGeaDeposit *>::const_iterator i = deposits.find( towerid );
  if( i == deposits.end() ) return NAN;
  else return i->second->get_tof(type);
}



void emcGeaTrackContentv3::set_tof_bytower(emc_towerid_t towerid, float tof, emcGeaDeposit::datatype_t type){
  // TODO: should return error instead of adding new deposit 
  // (most field of the new deposit will not be set!
  if( deposits.find(towerid) == deposits.end() ){
    deposits[towerid] = emcGeaDeposit::createdef();
    //if( towers && towers->find(towerid) ) towers->find(towerid)->add_track( trkno );
    ctowerlist.insert(towerid);
  }
  deposits[towerid]->set_tof(tof, type);
}




emc_clusterlist_t const emcGeaTrackContentv3::get_cluster_list() const {
  if( !cacheok ){
    cclusterlist.clear();
    cclusterlist = emcGeaTrackContent::get_cluster_list(); 
    cacheok = true;
  }

  return cclusterlist;
}



void emcGeaTrackContentv3::invcache(cachetype_t type){ 
  cclusterlist.clear();
  cacheok = false; 
}






