#include "TrigRunLvl1v3.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include <iostream>

ClassImp(TrigRunLvl1v3)

using namespace std;

const static unsigned int INIT_TRIGGER_VERSION = 9999999;

TrigRunLvl1v3::TrigRunLvl1v3()
{
  init();
  return ;
}

TrigRunLvl1v3::TrigRunLvl1v3(const TrigRunLvl1v3& o)
{
  o.copyTo(*this);
}

void
TrigRunLvl1v3::copyTo(TrigRunLvl1v3& o) const
{
  o.trigger_description = trigger_description;
  o.trigger_version = trigger_version;
  o.bbcll1_description = bbcll1_description;
  o.bbcll1_version = bbcll1_version;
  o.partition_name = partition_name;

  for ( int i = 0; i < 32; ++i )
    {
      o.lvl1_trig_name[i] = lvl1_trig_name[i];
      o.lvl1_trig_bit[i] = lvl1_trig_bit[i];
      o.lvl1_trig_scale_down[i] = lvl1_trig_scale_down[i];
      o.lvl1_trig_bitmask[i] = lvl1_trig_bitmask[i];
      o.lvl1_trig_rate_begin[i] = lvl1_trig_rate_begin[i];
    }

  for ( int i = 0; i < 130; ++i )
    {
      o.lvl1_rbit_name[i] = lvl1_rbit_name[i];
    }

  o.lvl1_lvl2_reject_enable = lvl1_lvl2_reject_enable;
  o.lvl1_trig_enable = lvl1_trig_enable;
  o.run_number = run_number;
  o.start_time = start_time;


}

void TrigRunLvl1v3::init()
{
  // initialize all TString pointers to NULL
  // and set all values to zero


  trigger_version = INIT_TRIGGER_VERSION;
  bbcll1_description = "";
  bbcll1_version = 9999999;
  partition_name = "";
  lvl1_trig_enable = 0;
  lvl1_lvl2_reject_enable = 9999999;

  run_number = 0;
  start_time = 0;


  for (int i = 0; i < 32; i++)
    {
      lvl1_trig_rate_begin[i] = 0;
      lvl1_trig_bit[i] = 0;
      lvl1_trig_scale_down[i] = 0;
      lvl1_trig_bitmask[i] = 0;
    }

  //     for (int i = 0; i< 130; i++)
  //       {
  //         lvl1_rbit_name[i] = "";
  //       }

  return ;
}

int TrigRunLvl1v3::isValid() const
{
  if (trigger_version == INIT_TRIGGER_VERSION)
    {
      return 0;
    }
  return 1;
}

void TrigRunLvl1v3::identify(ostream& os) const
{
  os << "identify yourself: TrigRunLvl1v3 object" << endl;
  os << "Trigger Version: " << trigger_version << endl;
  return ;
}

const char *TrigRunLvl1v3::get_lvl1_trig_name_bybit(const unsigned int bit) const
{
  int index = get_lvl1_trig_index(bit);
  if (index == 999)
    {
      return NULL; // there was no matching index to the bit
    }
  return get_lvl1_trig_name(index);
}

unsigned int
TrigRunLvl1v3::get_lvl1_trigger_enable(const unsigned int i) const
{
  if (((1 << i) & lvl1_trig_enable) > 0)
    {
      return 1;
    }
  else
    {
      return 0;
    }
}

unsigned int
TrigRunLvl1v3::get_lvl1_trigger_enable_bybit(const unsigned int bit) const
{
  int index = get_lvl1_trig_index(bit);
  if (index == 999)
    {
      return 0; // there was no matching index to the bit
    }
  if (((1 << index) & lvl1_trig_enable) > 0)
    {
      return 1;
    }
  else
    {
      return 0;
    }
}

void
TrigRunLvl1v3::set_lvl1_trigger_enable(const unsigned int ival, const unsigned int i)
{
  if (ival == 0 || ival == 1)
    {
      lvl1_trig_enable = (lvl1_trig_enable & (0xffffffff - (1 << i))); // zero the bit
      if (ival == 1)
        {
          lvl1_trig_enable = lvl1_trig_enable + (1 << i);     // set the bit high
        }
    }
  return ;
}

unsigned int
TrigRunLvl1v3::get_lvl1_trig_index(const unsigned int bit) const
{
  for (int index = 0; index < 32; index++)
    {
      if (get_lvl1_trig_bit(index) == bit)
	{
	  return index;
	}
    }
  return 999;  // this means index is assigned to that trigger bit
}

unsigned int
TrigRunLvl1v3::get_lvl1_trig_scale_down_bybit(const unsigned int bit) const
{
  int index = get_lvl1_trig_index(bit);
  if (index == 999)
    {
      return 0; // there was no matching index to the bit
    }
  return lvl1_trig_scale_down[index];
}

unsigned int
TrigRunLvl1v3::get_lvl1_lvl2_reject_enable(const unsigned int i) const
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

unsigned int
TrigRunLvl1v3::get_lvl1_lvl2_reject_enable_bybit(const unsigned int bit) const
{
  int index = get_lvl1_trig_index(bit);
  if (index == 999)
    {
      return 0; // there was no matching index to the bit
    }
  if (((1 << index) & lvl1_lvl2_reject_enable) > 0)
    {
      return 1;
    }
  else
    {
      return 0;
    }
}

void
TrigRunLvl1v3::set_lvl1_lvl2_reject_enable(const unsigned int ival, const unsigned int i)
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
