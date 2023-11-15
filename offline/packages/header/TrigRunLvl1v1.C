#include "TrigRunLvl1v1.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
//INCLUDECHECKER: Removed this line: #include "TString.h"
#include <iostream>

ClassImp(TrigRunLvl1v1)

using namespace std;

TrigRunLvl1v1::TrigRunLvl1v1()
{
  init();
  return ;
}

void TrigRunLvl1v1::init()
{
  // initialize all TString pointers to NULL
  // and set all values to zero
  trigger_version = 9999999;
  trigger_description = "";
  bbcll1_description = "";
  bbcll1_version = 9999999;
  partition_name = "";
  lvl1_trig_enable = 0;
  lvl1_lvl2_reject_enable = 9999999;



  for (int i = 0; i < 32; i++)
    {
      lvl1_trig_name[i] = "";
      lvl1_trig_bit[i] = 0;
      lvl1_trig_scale_down[i] = 0;
    }
  for (int i = 0; i < 130;i++)
    {
      lvl1_rbit_name[i] = "";
    }

  return ;
}

int TrigRunLvl1v1::isValid() const
{
  return (int) trigger_version; // what is the purpose of this return
}

void TrigRunLvl1v1::identify(ostream& os) const
{
  os << "identify yourself: TrigRunLvl1v1 object" << endl;
  os << "Trigger Version: " << trigger_version << endl;
  return ;
}

void TrigRunLvl1v1::Reset()
{
  //  init();
  return ;
}

const char *TrigRunLvl1v1::get_lvl1_trig_name_bybit(const unsigned int bit) const
{
  int index = get_lvl1_trig_index(bit);
  if (index == 999)
    {
      return NULL; // there was no matching index to the bit
    }
  return get_lvl1_trig_name(index);
}

unsigned int TrigRunLvl1v1::get_lvl1_trigger_enable(const unsigned int i) const
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

unsigned int TrigRunLvl1v1::get_lvl1_trigger_enable_bybit(const unsigned int bit) const
{
  int index = get_lvl1_trig_index(bit);
  if (index == 999)
    return 0; // there was no matching index to the bit
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
TrigRunLvl1v1::set_lvl1_trigger_enable(const unsigned int ival, const unsigned int i)
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
TrigRunLvl1v1::get_lvl1_trig_index(const unsigned int bit) const
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
TrigRunLvl1v1::get_lvl1_trig_scale_down_bybit(const unsigned int bit) const
{
  int index = get_lvl1_trig_index(bit);
  if (index == 999)
    {
      return 0; // there was no matching index to the bit
    }
  return lvl1_trig_scale_down[index];
}

unsigned int
TrigRunLvl1v1::get_lvl1_lvl2_reject_enable(const unsigned int i) const
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
TrigRunLvl1v1::get_lvl1_lvl2_reject_enable_bybit(const unsigned int bit) const
{
  int index = get_lvl1_trig_index(bit);
  if (index == 999)
    return 0; // there was no matching index to the bit
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
TrigRunLvl1v1::set_lvl1_lvl2_reject_enable(const unsigned int ival, const unsigned int i)
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

