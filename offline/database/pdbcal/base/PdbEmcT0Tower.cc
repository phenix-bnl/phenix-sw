
#include <iostream>
#include <string.h>

#include <PdbEmcT0Tower.hh>

/*PdbEmcT0Tower::PdbEmcT0Tower( const PdbEmcT0Tower &c)
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

		  setTwrT0Status(arm,sector,iy,iz, c.getTwrT0Status(arm,sector,iy,iz) );
		  setTwrT0Correction(arm,sector,iy,iz, c.getTwrT0Correction(arm,sector,iy,iz) );
		  setTwrT0Error(arm,sector,iy,iz, c.getTwrT0Error(arm,sector,iy,iz) );

		  setLeastCountStatus(arm,sector,iy,iz, c.getLeastCountStatus(arm,sector,iy,iz) );
		  setLeastCountCorrection(arm,sector,iy,iz, c.getLeastCountCorrection(arm,sector,iy,iz) );
		  setLeastCountError(arm,sector,iy,iz, c.getLeastCountError(arm,sector,iy,iz) );

		  setSlewingStatus(arm,sector,iy,iz, c.getSlewingStatus(arm,sector,iy,iz) );
		  setSlewingCorrection(arm,sector,iy,iz, c.getSlewingCorrection(arm,sector,iy,iz) );
		  setSlewingError(arm,sector,iy,iz, c.getSlewingError(arm,sector,iy,iz) );

		}
	    }
	}
    }
}
*/
PdbEmcT0Tower& PdbEmcT0Tower::operator= ( const PdbEmcT0Tower &c)
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

		  setTwrT0Status(arm,sector,iy,iz, c.getTwrT0Status(arm,sector,iy,iz) );
		  setTwrT0Correction(arm,sector,iy,iz, c.getTwrT0Correction(arm,sector,iy,iz) );
		  setTwrT0Error(arm,sector,iy,iz, c.getTwrT0Error(arm,sector,iy,iz) );

		  setLeastCountStatus(arm,sector,iy,iz, c.getLeastCountStatus(arm,sector,iy,iz) );
		  setLeastCountCorrection(arm,sector,iy,iz, c.getLeastCountCorrection(arm,sector,iy,iz) );
		  setLeastCountError(arm,sector,iy,iz, c.getLeastCountError(arm,sector,iy,iz) );

		  setSlewingStatus(arm,sector,iy,iz, c.getSlewingStatus(arm,sector,iy,iz) );
		  setSlewingCorrection(arm,sector,iy,iz, c.getSlewingCorrection(arm,sector,iy,iz) );
		  setSlewingError(arm,sector,iy,iz, c.getSlewingError(arm,sector,iy,iz) );

		}
	    }
	}
    }
  return *this;
}

 
PdbEmcT0Tower::PdbEmcT0Tower()
{

  memset( _twr_t0_stat,0, 2*4*96*48  *sizeof(int));
  memset( _twr_t0,0, 2*4*96*48  *sizeof(int));
  memset( _twr_t0_error,0, 2*4*96*48  *sizeof(int));

  memset( _twr_lc_stat,0, 2*4*96*48  *sizeof(int));
  memset( _twr_lc,1, 2*4*96*48  *sizeof(int));       // default is 1
  memset( _twr_lc_error,0, 2*4*96*48  *sizeof(int));

  memset( _twr_sl_stat,0, 2*4*96*48  *sizeof(int));
  memset( _twr_sl,0, 2*4*96*48  *sizeof(int));
  memset( _twr_sl_error,0, 2*4*96*48  *sizeof(int));

}


int PdbEmcT0Tower::setTwrT0Status(const int arm, 
				  const int sector,
				  const int iy,
				  const int iz, 
				  const int value)
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return -1;

  _twr_t0_stat[arm][sector][iz][iy] = value;

  return 0;

}



int PdbEmcT0Tower::setTwrT0Correction(const int arm, 
				      const int sector,
				      const int iy,
				      const int iz, 
				      const float value)
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return -1;

  _twr_t0[arm][sector][iz][iy] = value;

  return 0;

}

int PdbEmcT0Tower::setTwrT0Error(const int arm, 
				 const int sector,
				 const int iy,
				 const int iz, 
				 const float value)
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return -1;

  _twr_t0_error[arm][sector][iz][iy] = value;

  return 0;

}


int PdbEmcT0Tower::setLeastCountStatus(const int arm, 
				       const int sector,
				       const int iy,
				       const int iz, 
				       const int value)
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return -1;

  _twr_lc_stat[arm][sector][iz][iy] = value;

  return 0;

}

int PdbEmcT0Tower::setLeastCountCorrection(const int arm, 
					   const int sector,
					   const int iy,
					   const int iz, 
					   const float value)
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return -1;

  _twr_lc[arm][sector][iz][iy] = value;

  return 0;

}

int PdbEmcT0Tower::setLeastCountError(const int arm, 
				      const int sector,
				      const int iy,
				      const int iz, 
				      const float value)
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return -1;

  _twr_lc_error[arm][sector][iz][iy] = value;

  return 0;

}


int PdbEmcT0Tower::setSlewingStatus(const int arm, 
				    const int sector,
				    const int iy,
				    const int iz, 
				    const int value)
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return -1;

  _twr_sl_stat[arm][sector][iz][iy] = value;

  return 0;

}

int PdbEmcT0Tower::setSlewingCorrection(const int arm, 
					const int sector,
					const int iy,
					const int iz, 
					const float value)
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return -1;

  _twr_sl[arm][sector][iz][iy] = value;

  return 0;

}

int PdbEmcT0Tower::setSlewingError(const int arm, 
				   const int sector,
				   const int iy,
				   const int iz, 
				   const float value)
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return -1;

  _twr_sl_error[arm][sector][iz][iy] = value;

  return 0;

}

//-----------

int PdbEmcT0Tower::getTwrT0Status(const int arm, 
				  const int sector,
				  const int iy,
				  const int iz) const 
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return -1;

  return _twr_t0_stat[arm][sector][iz][iy];
}


	       

float PdbEmcT0Tower::getTwrT0Correction(const int arm, 
					const int sector,
					const int iy,
					const int iz) const
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return 0;


  if  (_twr_t0_stat[arm][sector][iz][iy] == 0) return 0;
  return _twr_t0[arm][sector][iz][iy];

}

		       

float PdbEmcT0Tower::getTwrT0Error(const int arm, 
				   const int sector,
				   const int iy,
				   const int iz) const
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return 0;


  if  (_twr_t0_stat[arm][sector][iz][iy] == 0) return 0;
  return _twr_t0_error[arm][sector][iz][iy];

}
		       


int PdbEmcT0Tower::getLeastCountStatus(const int arm, 
				       const int sector,
				       const int iy,
				       const int iz) const 
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return -1;


  return _twr_lc_stat[arm][sector][iz][iy];

}
		       

float PdbEmcT0Tower::getLeastCountCorrection(const int arm, 
					     const int sector,
					     const int iy,
					     const int iz) const
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return 1;


  if  (_twr_lc_stat[arm][sector][iz][iy] == 0) return 1;
  return _twr_lc[arm][sector][iz][iy];

}
		       

float PdbEmcT0Tower::getLeastCountError(const int arm, 
					const int sector,
					const int iy,
					const int iz) const
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return 1;


  if  (_twr_lc_stat[arm][sector][iz][iy] == 0) return 1;
  return _twr_lc_error[arm][sector][iz][iy];

}
		       
		       


int PdbEmcT0Tower::getSlewingStatus(const int arm, 
				    const int sector,
				    const int iy,
				    const int iz) const
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return -1;


  return _twr_sl_stat[arm][sector][iz][iy];

}
		       

float PdbEmcT0Tower::getSlewingCorrection(const int arm, 
					  const int sector,
					  const int iy,
					  const int iz) const
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return 0;


  if  (_twr_sl_stat[arm][sector][iz][iy] == 0) return 0;
  return _twr_sl[arm][sector][iz][iy];

}
		       

float PdbEmcT0Tower::getSlewingError(const int arm, 
				     const int sector,
				     const int iy,
				     const int iz) const
{
  if ( arm < 0    || arm>=2 || 
       sector < 0 || sector>=4 || 
       iz  < 0    || iz >= 96 || 
       iy < 0     || iy>=48 ) return 0;


  if  (_twr_sl_stat[arm][sector][iz][iy] == 0) return 0;
  return _twr_sl_error[arm][sector][iz][iy];

}
		      

void PdbEmcT0Tower::print() const
{
  std::cout << "Hello, world" << std::endl;
}


