#include <TrigLvl1v1.h>
//INCLUDECHECKER: Removed this line: #include <phool.h>
#include <iostream>

ClassImp(TrigLvl1v1)

using namespace std;

TrigLvl1v1::TrigLvl1v1()
{
  init();
  return;
}

void TrigLvl1v1::init()
{
  short int i;
  lvl1_trigraw = 0;
  lvl1_triglive = 0;
  lvl1_trigscaled = 0;
  lvl1_clock_cross = 0;
  for(i=0;i<5;i++)
   {
     lvl1_rbits[i] = 0;
   }
 return;
}

int TrigLvl1v1::isValid() const
{
  return ((lvl1_trigraw>0) ? 1 : 0);
}

void TrigLvl1v1::identify(ostream& os) const
{
  os << "identify yourself: TrigLvl1v1 object" << endl;
  os << "TrigRaw: 0x" << hex << lvl1_trigraw << dec << endl;
  os << "TrigLive: 0x" << hex << lvl1_triglive << dec << endl;
  os << "TrigScaled: 0x" << hex << lvl1_trigscaled << dec << endl;
  os << "Crossing counter: " << lvl1_clock_cross << endl;
  return;
}

void TrigLvl1v1::Reset()
{
  init();
  return;
}




