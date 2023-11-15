
#include <iostream>
#include <string.h>

#include <PdbEmcEScaleTowerRecal.hh>

PdbEmcEScaleTowerRecal::PdbEmcEScaleTowerRecal( const PdbEmcEScaleTowerRecal &c)
{
  int arm, sector, iy, iz;
  float factors[4] = {1.0, 0.0, 0.0, 0.0};

  for ( arm = 0; arm < 2; arm++)
    {
      for (sector = 0; sector < 4; sector++)
        {
          for (iz = 0; iz < 96; iz++)
            {
              for (iy = 0; iy < 48; iy++)
                {

                  setTwrEScaleStatus(arm, sector, iy, iz, c.getTwrEScaleStatus(arm, sector, iy, iz) );

                  // Escale factor including non-linearity effect
                  c.getTwrEScaleFactor(arm, sector, iy, iz, factors);
                  setTwrEScaleFactor(arm, sector, iy, iz, factors );


                }
            }
        }
    }
}

PdbEmcEScaleTowerRecal& PdbEmcEScaleTowerRecal::operator= ( const PdbEmcEScaleTowerRecal & c)
{
  int arm, sector, iy, iz;
  float factors[4] = {1.0, 0.0, 0.0, 0.0};

  for ( arm = 0; arm < 2; arm++)
    {
      for (sector = 0; sector < 4; sector++)
        {
          for (iz = 0; iz < 96; iz++)
            {
              for (iy = 0; iy < 48; iy++)
                {

                  setTwrEScaleStatus(arm, sector, iy, iz, c.getTwrEScaleStatus(arm, sector, iy, iz) );

                  // Escale factor including non-linearity effect
                  c.getTwrEScaleFactor(arm, sector, iy, iz, factors);
                  setTwrEScaleFactor(arm, sector, iy, iz, factors );


                }
            }
        }
    }
  return *this;
}


PdbEmcEScaleTowerRecal::PdbEmcEScaleTowerRecal()
{
//  memset( _twr_escale_stat,0, 2*4*96*48  *sizeof(int));

  for (int arm = 0;arm < 2;arm++)
    {
      for (int sector = 0;sector < 4;sector++)
        {
         for (int iz = 0; iz < 96; iz++)
           {
            for (int iy = 0; iy < 48; iy++)
              {
                _twr_escale_stat[arm][sector][iz][iy]= 0;
                _twr_escale[arm][sector][iz][iy][0] = 1.0;
                _twr_escale[arm][sector][iz][iy][1] = 0.0;
                _twr_escale[arm][sector][iz][iy][2] = 0.0;
                _twr_escale[arm][sector][iz][iy][3] = 0.0;
               }
            }

        }
    }
}


int PdbEmcEScaleTowerRecal::setTwrEScaleStatus(const int arm,
					       const int sector,
					       const int iy,
					       const int iz,
					       const int value)
{
  if ( arm < 0    || arm >= 2 ||
       sector < 0 || sector >= 4 ||
       iz  < 0    || iz >= 96 ||
       iy < 0     || iy >= 48 ) return -1;

  _twr_escale_stat[arm][sector][iz][iy] = value;

  return 0;

}



int PdbEmcEScaleTowerRecal::setTwrEScaleFactor(const int arm,
					       const int sector,
					       const int iy,
					       const int iz,
					       const float value[4])
{
  if ( arm < 0    || arm >= 2 ||
       sector < 0 || sector >= 4 ||
       iz  < 0    || iz >= 96 ||
       iy < 0     || iy >= 48 )
    {
      return -1;
    }
  for (int i = 0;i < 4;i++)
    {
      _twr_escale[arm][sector][iz][iy][i] = value[i];
    }

  return 0;

}


int PdbEmcEScaleTowerRecal::getTwrEScaleStatus(const int arm,
					       const int sector,
					       const int iy,
					       const int iz) const
{
  if ( arm < 0    || arm >= 2 ||
       sector < 0 || sector >= 4 ||
       iz  < 0    || iz >= 96 ||
       iy < 0     || iy >= 48 )
    {
      return -1;
    }
  return _twr_escale_stat[arm][sector][iz][iy];
}




int PdbEmcEScaleTowerRecal::getTwrEScaleFactor(const int arm,
					       const int sector,
					       const int iy,
					       const int iz,
					       float *value) const
{
  if ( arm < 0    || arm >= 2 ||
       sector < 0 || sector >= 4 ||
       iz  < 0    || iz >= 96 ||
       iy < 0     || iy >= 48 )
    {
      return 1;
    }

  if  (_twr_escale_stat[arm][sector][iz][iy] == 0)
    {
      return 1;
    }
  for (int i = 0;i < 4;i++)
    {
      value[i] = _twr_escale[arm][sector][iz][iy][i];
    }
  return 0;

}

void PdbEmcEScaleTowerRecal::print() const
{
  std::cout << "Hello, world" << std::endl;
}
