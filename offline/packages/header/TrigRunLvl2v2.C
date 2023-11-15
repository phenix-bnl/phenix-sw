#include "TrigRunLvl2v2.h"
#include <iostream>

ClassImp(TrigRunLvl2v2)

using namespace std;

const static unsigned int INIT_LVL2_VERSION = 9999999;

TrigRunLvl2v2::TrigRunLvl2v2()
{
  Reset();
  return ;
}

TrigRunLvl2v2::TrigRunLvl2v2(const TrigRunLvl2v2& o)
{
  o.copyTo(*this);
}

void
TrigRunLvl2v2::copyTo(TrigRunLvl2v2& o) const
{
  o.lvl2_description = lvl2_description;
  o.lvl2_version = lvl2_version;
  o.lvl2_run_enable = lvl2_run_enable;
  o.lvl2_reject_enable = lvl2_reject_enable;
  o.lvl1_lvl2_reject_enable = lvl1_lvl2_reject_enable;

  for ( int i = 0; i < 32; ++i )
    {
      o.lvl1_lvl2_force_accept[i] = lvl1_lvl2_force_accept[i];
    }

  for ( int i = 0; i < 64; ++i )
    {
      o.lvl2_trig_name[i] = lvl2_trig_name[i];
      o.lvl2_trig_bit[i] = lvl2_trig_bit[i];
      o.lvl2_trig_version[i] = lvl2_trig_version[i];
      for ( int j = 0; j < 32; ++j )
	{
	  o.lvl2_lvl1_assoc[i][j] = lvl2_lvl1_assoc[i][j];
	  o.lvl2_lvl1_prescale[i][j] = lvl2_lvl1_prescale[i][j];
	}
    }
}

int TrigRunLvl2v2::isValid() const
{
  if (lvl2_version == INIT_LVL2_VERSION)
    {
      return 0;
    }
  return 1;
}

void TrigRunLvl2v2::identify(ostream& os) const
{
  os << "identify yourself: TrigRunLvl2v2 object" << endl;
  os << "Level2 Trigger Version: " << lvl2_version << endl;
  return ;
}

unsigned int
TrigRunLvl2v2::get_lvl1_lvl2_reject_enable(const unsigned int i) const
{
  if (((1 << i) & lvl1_lvl2_reject_enable) > 0)
    {
      return 1;
    }
  else
    {
      return 0;
    }
}

void
TrigRunLvl2v2::set_lvl1_lvl2_reject_enable(const unsigned int ival, const unsigned int i)
{
  if (ival == 0 || ival == 1)
    {
      lvl1_lvl2_reject_enable = (lvl1_lvl2_reject_enable & (0xffffffff - (1 << i))); // zero the bit
      if (ival == 1)
        {
          lvl1_lvl2_reject_enable = lvl1_lvl2_reject_enable + (1 << i);     // set the bit high
        }
    }
  return ;
}

void
TrigRunLvl2v2::Reset()
{
  lvl2_version = INIT_LVL2_VERSION;
  lvl2_run_enable = 0;
  lvl2_reject_enable = 0;
  lvl1_lvl2_reject_enable = 0;
  memset(lvl1_lvl2_force_accept, 0, sizeof(lvl1_lvl2_force_accept));
  memset(lvl2_trig_bit, 0, sizeof(lvl2_trig_bit));
  memset(lvl2_trig_version, 0, sizeof(lvl2_trig_version));
  memset(lvl2_lvl1_assoc, 0, sizeof(lvl2_lvl1_assoc));
  memset(lvl2_lvl1_prescale, 0, sizeof(lvl2_lvl1_prescale));
}
