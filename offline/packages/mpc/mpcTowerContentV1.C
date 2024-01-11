#include "mpcTowerContentV1.h"
#include "MpcMap.h"
#include <MpcRandom.h>
#include <mpcNoiseContainer.h>
#include <cmath>

ClassImp(mpcTowerContentV1)

using namespace std;

mpcTowerContentV1::mpcTowerContentV1()
{
  ch     = -9999;
  tof    = -9999.;
  energy = -9999.;
}

mpcTowerContentV1::mpcTowerContentV1(mpcTowerContent &m)
{
  ch     = m.get_ch();
  tof    = m.get_tof();
  energy = m.get_energy();
}

mpcTowerContent& mpcTowerContentV1::operator=(mpcTowerContent &rhs)
{
  if ( this == &rhs ) return *this;
  
  ch     = rhs.get_ch();
  tof    = rhs.get_tof();
  energy = rhs.get_energy();

  return *this;
}

mpcTowerContent& mpcTowerContentV1::operator+=(mpcTowerContent &rhs)
{
  if ( ch != rhs.get_ch() )
    {
      cerr << PHWHERE << " ERROR, adding different mpc channels! " << ch << " " << rhs.get_ch() << endl;
      return *this;
    }

  // how to handle time? right now we use the average
  tof += rhs.get_tof();
  tof *= 0.5;
  
  double energy_rhs = rhs.get_energy(); 
  
  energy_rhs = rhs.get_energy(0);
  if( rhs.get_type() == 11 ){
    MpcRandom *mpcrand = MpcRandom::instance();
    mpcNoiseContainer* mpcnoise = mpcNoiseContainer::instance();
    // There are 1.45 p.e./MeV.  Put in stochastic term of 2.6%*E

    double sigmaE = 0;
    double sigmaCalib = 0;
    if(energy_rhs > 0) 
      sigmaE = mpcrand->Gaus( 0.,0.02626128 )*sqrt(energy_rhs);  
    if(energy_rhs > 0) 
      sigmaCalib = mpcnoise->get_calib(ch)*energy_rhs;  
    
    // photon fluctation term 
    energy_rhs = 1.05*(energy_rhs + sigmaE + sigmaCalib);
    
  }

  energy += energy_rhs;

  // we should add in the possibility of a different dynamic range
  // if ( energy > 40. ) energy = 40.;

  return *this;
}

mpcTowerContent& mpcTowerContentV1::operator+(mpcTowerContent &rhs)
{
  mpcTowerContentV1 *new_towcontent = new mpcTowerContentV1;
  *new_towcontent = *this;

  *new_towcontent += rhs;

  return *new_towcontent;
}

void mpcTowerContentV1::print(std::ostream& out = std::cout) const
{
  MpcMap *mpcmap = MpcMap::instance();
  out << ch << "\t" << mpcmap->getGridX(ch) << "\t"
      << mpcmap->getGridY(ch) << "\t" << tof << "\t" << energy << endl;
}
