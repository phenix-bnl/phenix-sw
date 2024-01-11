#include <TrigLvl1v3.h>
//INCLUDECHECKER: Removed this line: #include <phool.h>

#include <iomanip>
//INCLUDECHECKER: Removed this line: #include <iostream>

ClassImp(TrigLvl1v3)

using namespace std;

TrigLvl1v3::TrigLvl1v3()
{
  init();
  return;
}

void TrigLvl1v3::init()
{
  for (int i=0; i<2;i++)
    {
      beamclk[i] = 0xFFFFFFFF;
    }
  TrigLvl1v2::init();
 return;
}

void TrigLvl1v3::identify(ostream& os) const
{
  os << "identify yourself: TrigLvl1v3 object" << endl;
  printval(os);
  os.fill('0');
  os << "BeamClk: 0x" << hex << setw(8) << get_lvl1_beam_clk(1) 
     << setw(8) << get_lvl1_beam_clk(0) << dec << endl;
  return;
}

void TrigLvl1v3::Reset()
{
  init();
  return;
}







