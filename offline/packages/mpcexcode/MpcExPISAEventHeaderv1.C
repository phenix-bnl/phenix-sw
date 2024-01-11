#include "MpcExPISAEventHeaderv1.h"


using namespace std;

MpcExPISAEventHeaderv1::MpcExPISAEventHeaderv1()
{
  for(int i=0; i<MpcExConstants::NARMS; i++){
    sf[i] = 0.0; 
    GEANTEnergy[i] = 0.0; 
    DeadAreaEnergy[i] = 0.0; 
    SiEnergy[i] = 0.0; 
    FPLTEnergy[i] = 0.0; 
    eLowSat[i] = 0.0; 
    _n_si_hits[i] = 0; 
    _n_absorber_hits[i] = 0; 
    _n_dead_hits[i] = 0; 
    _n_fplt_hits[i] = 0; 
    _n_low_sat[i] = 0; 
  }


}

MpcExPISAEventHeaderv1::~MpcExPISAEventHeaderv1()
{
}

