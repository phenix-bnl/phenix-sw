
#include "phool.h"
#include "SavePHPanel.h"

#include <cmath>
#include <iostream>

ClassImp(SavePHPanel)

using namespace std;

double SavePHPanel::GetPoint(const short int i, const short int index) const
{
  std::cout << PHWHERE << "calling virtual function GetPoint(...)" << std::endl;
  return NAN;
}
