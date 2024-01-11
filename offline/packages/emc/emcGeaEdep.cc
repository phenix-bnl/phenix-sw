//////////////////////////////////////////////////////////////////////////////////
//
// class that represents an atomic geant energy deposition. energy deposits
// provided by geant are stored in instances of this class by the EmcGeaPisa2GeaEdep
// SubsysReco module. if you want to use input from other simulation program 
// than geant, write a converter. 
//
// this class is meant to be a lightweight class used only by the simulation code
// internally and not written out to disk. thus it is not a PHObject, not schema 
// evolved, and does not even has the ususal accessor/mutator functions 
// ( Get*() / Set*() ).
//
//////////////////////////////////////////////////////////////////////////////////

#include <cassert>

#include <emcGeaEdep.h>


ClassImp(emcGeaEdep);
template class emcContentT< emcGeaEdep >;
const bool emcGeaEdep::__buildhash;



void emcGeaEdep::copy(const emcGeaEdep * from){
  trkno = from->trkno;
  input = from->input;
  
  arm = from->arm;
  sector = from->sector;
  type = from->type;
  smodind = from->smodind;
  towerind = from->towerind;
  swkey = from->swkey, hwkey = from->hwkey;
  
  x = from->x, y = from->y, z = from->z;
  iy = from->iy, iz = from->iz;
  
  edep = from->edep, geaedep = from->geaedep;
  tof = from->tof, geatof = from->geatof;

}

emcGeaEdep& emcGeaEdep::operator += (const emcGeaEdep & d) {
  assert( (input == d.input) && (trkno == d.trkno) );
  assert( swkey == d.swkey);
  
  edep += d.edep;
  geaedep += d.geaedep;
  
  if( !isnan(d.tof)  &&  (isnan(tof) ||  d.tof < tof) ) tof = d.tof;
  
  if( !isnan(d.geatof)  &&  (isnan(geatof) ||  d.geatof < geatof) ){
    geatof = d.geatof;
    x = d.x; y = d.y; z = d.z;
  }
  
  return *this;
}




std::ostream & operator << (std::ostream & s, const emcGeaEdep & deposit){
  s << "    " << deposit.trkno << " "
    << "(" << deposit.arm << "," << deposit.sector << "," << deposit.iy << "," << deposit.iz << ")  "
    << "edep=" << deposit.edep << " tof=" << deposit.tof;
  
  return s;
}
