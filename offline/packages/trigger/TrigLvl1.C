#include <TrigLvl1.h>
#include <phool.h>
#include <iostream>

ClassImp(TrigLvl1)

using namespace std;

int TrigLvl1::isValid() const
{
  cout << PHWHERE << ": isValid() not implemented" << endl;
  return 0;
}

void TrigLvl1::identify(ostream& os) const
{
  os << "identify yourself:  TrigLvl1 object" << endl;
  return ;
}

void TrigLvl1::Reset()
{
  cout << PHWHERE << ": ERROR Reset() not implemented" << endl;
  return ;
}
