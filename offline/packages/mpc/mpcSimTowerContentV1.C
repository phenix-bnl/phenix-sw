#include <mpcSimTowerContentV1.h>
#include <MpcMap.h>
#include <MpcRandom.h>
#include <recoConsts.h>
#include <cmath>

ClassImp(mpcSimTowerContentV1)

using namespace std;

mpcSimTowerContentV1::mpcSimTowerContentV1()
{
  ch     = -9999;
  tof    = -9999.;
  energy = -9999.;
  energy_noise = -9999.;	// initial value
}

mpcSimTowerContentV1::mpcSimTowerContentV1(mpcTowerContent &m)
{
  ch     = m.get_ch();
  tof    = m.get_tof();
  energy = m.get_energy(0);
  energy_noise = -9999.;
}

mpcTowerContent& mpcSimTowerContentV1::operator=(mpcTowerContent &rhs)
{
  if ( this == &rhs ) return *this;
  
  ch     = rhs.get_ch();
  tof    = rhs.get_tof();
  energy = rhs.get_energy(0);
  energy_noise = -9999.;

  return *this;
}

mpcTowerContent& mpcSimTowerContentV1::operator+=(mpcTowerContent &rhs)
{
  if ( ch != rhs.get_ch() )
    {
      cerr << PHWHERE << " ERROR, adding different mpc channels! " << ch << " " << rhs.get_ch() << endl;
      return *this;
    }

  // how to handle time? right now we use the average
  tof += rhs.get_tof();
  tof *= 0.5;

  energy += rhs.get_energy(0);
  energy_noise = -9999.;

  // we should add in the different dynamic ranges
  //if ( energy > 40. ) energy = 40.;

  return *this;
}

mpcTowerContent& mpcSimTowerContentV1::operator+(mpcTowerContent &rhs)
{
  mpcSimTowerContentV1 *new_towcontent = new mpcSimTowerContentV1;
  *new_towcontent = *this;

  *new_towcontent += rhs;

  return *new_towcontent;
}

void mpcSimTowerContentV1::print(std::ostream& out = std::cout) const
{
  MpcMap *mpcmap = MpcMap::instance();
  out << ch << "\t" << mpcmap->getGridX(ch) << "\t"
      << mpcmap->getGridY(ch) << "\t" << tof << "\t" << energy << endl;
}

float mpcSimTowerContentV1::get_energy(const int noise_alg) const
{
  if ( noise_alg == 1 )
    {
      get_energy_noise();
      return (energy + energy_noise)*1.05;	// 1.05 is the leakage
    }

  return energy;
}

void mpcSimTowerContentV1::get_energy_noise() const
{
  if ( energy_noise==-9999. )
    {
      MpcRandom *mpcrand = MpcRandom::instance();
      // There are 1.45 p.e./MeV.  Put in stochastic term of 2.6%*E
      
      double sigmaE = 0;
      if(energy > 0) sigmaE = mpcrand->Gaus( 0.,0.02626128 )*sqrt(energy);  // photon fluctation term 

      // We measured 75 MeV of noise
      double sigma_electronics = mpcrand->Gaus( 0., 0.075 );  // electronics noise

      energy_noise = sigmaE + sigma_electronics;				

      //cout << "energy noise " << energy << "\t" << sigmaE << "\t" << sigma_electronics << endl;
    }
}

