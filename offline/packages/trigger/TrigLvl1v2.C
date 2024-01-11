#include <TrigLvl1v2.h>
//INCLUDECHECKER: Removed this line: #include <phool.h>
#include <iostream>

ClassImp(TrigLvl1v2)

using namespace std;

TrigLvl1v2::TrigLvl1v2()
{
  init();
  return;
}

void TrigLvl1v2::init()
{
  short int i;
  lvl1_trigraw = 0;
  lvl1_triglive = 0;
  lvl1_trigscaled = 0;
  lvl1_clock_cross = 0xFFFFFFFF;
  for(i=0;i<RBITS_WORD_SIZE;i++)
   {
     lvl1_rbits[i] = 0;
   }
 return;
}

int TrigLvl1v2::isValid() const
{
  return ((lvl1_trigraw>0) ? 1 : 0);
}

void 
TrigLvl1v2::identify(ostream& os) const
{
  os << "identify yourself: TrigLvl1v2 object" << endl;
  printval(os);
  return;
}

void 
TrigLvl1v2::printval(ostream& os) const
{
  os << "TrigRaw: 0x" << hex << lvl1_trigraw << dec << endl;
  os << "TrigLive: 0x" << hex << lvl1_triglive << dec << endl;
  os << "TrigScaled: 0x" << hex << lvl1_trigscaled << dec << endl;
  os << "Crossing counter: " << lvl1_clock_cross << endl;
  return;
}

void TrigLvl1v2::Reset()
{
  init();
  return;
}







