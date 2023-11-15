
#include <iostream>
#include <string.h>

#include <PdbEmcT0Sector.hh>


PdbEmcT0Sector::PdbEmcT0Sector()
{

  memset(_run_t0_stat,0,2*4*sizeof(int));
  memset(_run_t0,0,2*4*sizeof(int));
  memset(_run_t0_error,0,2*4*sizeof(int));

}


PdbEmcT0Sector& PdbEmcT0Sector::operator = (const PdbEmcT0Sector &c)
{
  int arm, sector;
  
  for ( arm=0; arm <2; arm++)
    {
      for (sector=0; sector<4; sector++)
	{

	  setSectorT0Status(arm,sector,  c.getSectorT0Status(arm,sector) );
	  setSectorT0Correction(arm,sector,  c.getSectorT0Correction(arm,sector) );
	  setSectorT0Error(arm,sector,  c.getSectorT0Error(arm,sector) );
	}
    }
  return *this;
}





int PdbEmcT0Sector::setSectorT0Status(const int arm, 
				  const int sector,
				  const int value)
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 
      ) return -1;

  _run_t0_stat[arm][sector] = value;

  return 0;

}


int PdbEmcT0Sector::setSectorT0Correction(const int arm, 
				      const int sector,
				      const float value)
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 ) return -1;

  _run_t0[arm][sector] = value;

  return 0;

}

int PdbEmcT0Sector::setSectorT0Error(const int arm, 
				 const int sector,
				 const float value)
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 ) return -1;

  _run_t0_error[arm][sector] = value;

  return 0;

}


//-----------

int PdbEmcT0Sector::getSectorT0Status(const int arm, 
				  const int sector) const 
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4  ) return -1;

  return  _run_t0_stat[arm][sector];
}

float PdbEmcT0Sector::getSectorT0Correction(const int arm, 
					const int sector) const
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 ) return 0;

  if  (_run_t0_stat[arm][sector] == 0) return 0;
  return _run_t0[arm][sector];

}

		       

float PdbEmcT0Sector::getSectorT0Error(const int arm, 
				   const int sector) const
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 ) return 0;


  if  (_run_t0_stat[arm][sector] == 0) return 0;
  return _run_t0_error[arm][sector];

}
		      

void PdbEmcT0Sector::print() const
{
  std::cout << "PdbEmcT0Sector" << std::endl;
  std::cout << "Arm\tSector\tSector T0\tError\tStatus" << std::endl;


  for(int arm=0; arm<2; arm++)
    {
      for(int sector=0; sector<4; sector++)
	{
	  std::cout 
	    << arm << "\t"
	    << sector << "\t"
	    << getSectorT0Correction(arm, sector) << "\t"
	    << getSectorT0Error(arm, sector) << "\t"
	    << getSectorT0Status(arm, sector) 
	    << std::endl;
	}
    }

}



