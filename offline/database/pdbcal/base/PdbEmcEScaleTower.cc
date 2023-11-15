
#include <iostream>
#include <string.h>

#include <PdbEmcEScaleTower.hh>

PdbEmcEScaleTower::PdbEmcEScaleTower( const PdbEmcEScaleTower &c)
{
  int arm, sector,iy,iz;

  for ( arm=0; arm <2; arm++)
    {
      for (sector=0; sector<4; sector++)
	{
	  for (iz = 0; iz <96; iz++)
	    {
	      for (iy = 0; iy <48; iy++)
		{

		  setTwrEScaleStatus(arm,sector,iy,iz, c.getTwrEScaleStatus(arm,sector,iy,iz) );
		  setTwrEScaleFactor(arm,sector,iy,iz, c.getTwrEScaleFactor(arm,sector,iy,iz) );


		}
	    }
	}
    }
}

PdbEmcEScaleTower& PdbEmcEScaleTower::operator= ( const PdbEmcEScaleTower &c)
{
  int arm, sector,iy,iz;

  for ( arm=0; arm <2; arm++)
    {
      for (sector=0; sector<4; sector++)
	{
	  for (iz = 0; iz <96; iz++)
	    {
	      for (iy = 0; iy <48; iy++)
		{

		  setTwrEScaleStatus(arm,sector,iy,iz, c.getTwrEScaleStatus(arm,sector,iy,iz) );
		  setTwrEScaleFactor(arm,sector,iy,iz, c.getTwrEScaleFactor(arm,sector,iy,iz) );


		}
	    }
	}
    }
  return *this;
}

 
PdbEmcEScaleTower::PdbEmcEScaleTower()
{

  memset( _twr_escale_stat,0, 2*4*96*48  *sizeof(int));
  memset( _twr_escale,1, 2*4*96*48  *sizeof(int));     // default is 1

}


int PdbEmcEScaleTower::setTwrEScaleStatus(const int arm, 
				  const int sector,
				  const int iy,
				  const int iz, 
				  const int value)
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return -1;

  _twr_escale_stat[arm][sector][iz][iy] = value;

  return 0;

}



int PdbEmcEScaleTower::setTwrEScaleFactor(const int arm, 
				      const int sector,
				      const int iy,
				      const int iz, 
				      const float value)
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return -1;

  _twr_escale[arm][sector][iz][iy] = value;

  return 0;

}


int PdbEmcEScaleTower::getTwrEScaleStatus(const int arm, 
				  const int sector,
				  const int iy,
				  const int iz) const 
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return -1;

  return _twr_escale_stat[arm][sector][iz][iy];
}


	       

float PdbEmcEScaleTower::getTwrEScaleFactor(const int arm, 
					const int sector,
					const int iy,
					const int iz) const
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return 1;


  if  (_twr_escale_stat[arm][sector][iz][iy] == 0) return 1;
  return _twr_escale[arm][sector][iz][iy];

}
		       
void PdbEmcEScaleTower::print() const
{
  std::cout << "Hello, world" << std::endl;
}


