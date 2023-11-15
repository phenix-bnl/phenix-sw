#include "TrigRunLvl2v1.h"
#include <iostream>

ClassImp(TrigRunLvl2v1)

using namespace std;

TrigRunLvl2v1::TrigRunLvl2v1()
{
  Reset();
  return;
}


int
TrigRunLvl2v1::isValid() const
{
  return (int) lvl2_version; // what is the purpose of this return
}

void 
TrigRunLvl2v1::identify(ostream& os) const
{
  os << "identify yourself: TrigRunLvl2v1 object" << endl;
  os << "Level2 Trigger Version: " << lvl2_version << endl;
  return;
}

void
TrigRunLvl2v1::Reset()
{
  lvl2_version = 0;
  lvl2_run_enable = 0;
  lvl2_reject_enable = 0;
  lvl1_lvl2_reject_enable = 0;
  memset(lvl1_lvl2_force_accept, 0, sizeof(lvl1_lvl2_force_accept));
  memset(lvl2_trig_bit, 0, sizeof(lvl2_trig_bit));
  memset(lvl2_trig_version, 0, sizeof(lvl2_trig_version));
  memset(lvl2_lvl1_assoc, 0, sizeof(lvl2_lvl1_assoc));
}
