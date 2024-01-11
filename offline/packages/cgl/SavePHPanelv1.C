#include <SavePHPanelv1.h>
#include <PHPanel.h>
#include <iostream>

ClassImp(SavePHPanelv1)

using namespace std;

int SavePHPanelv1::AddPanel(const PHPanel *panel, const int iarm, const int index, const char *det)
{
  Index = index;
  arm = iarm;
  Detector = det;
  PHPoint pt = panel->getPoint(0);
  p0[0] = pt.getX();
  p0[1] = pt.getY();
  p0[2] = pt.getZ();
  pt = panel->getPoint(1);
  p1[0] = pt.getX();
  p1[1] = pt.getY();
  p1[2] = pt.getZ();
  pt = panel->getPoint(2);
  p2[0] = pt.getX();
  p2[1] = pt.getY();
  p2[2] = pt.getZ();
  return 0;
}

double SavePHPanelv1::GetPoint(const short int i, const short int index) const
{
  switch(i)
    {
    case 0:
      return p0[index];
    case 1:
      return p1[index];
    case 2:
      return p2[index];
    default:
      std::cerr << PHWHERE << "Invalid Point: " << i << std::endl;
    }
  return NAN;
}
