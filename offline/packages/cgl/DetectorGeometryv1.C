
#include "SavePHPanelv1.h"
#include "DetectorGeometryv1.h"

//INCLUDECHECKER: Removed this line: #include "PHPanel.h"

//INCLUDECHECKER: Removed this line: #include "phool.h"

#include "TClonesArray.h"


ClassImp(DetectorGeometryv1)

using namespace std;

static const unsigned int NPANEL = 200;
static unsigned int currentTCsize = NPANEL;

DetectorGeometryv1::DetectorGeometryv1()
{
  SvPHPanel = new TClonesArray("SavePHPanelv1", NPANEL);
  nPanel = 0;
}

DetectorGeometryv1::~DetectorGeometryv1()
{
  if (SvPHPanel)
    {
      SvPHPanel->Clear();
      delete SvPHPanel;
    }
}

int 
DetectorGeometryv1::isValid() const
{
  return ((nPanel > 0) ? 1 : 0);
}

void 
DetectorGeometryv1::identify(ostream& os) const
{
  os << "identify yourself: DetectorGeometryv1 Object containing "
     << nPanel << " Panels" << endl;
  return ;
}

int 
DetectorGeometryv1::AddPanel(const PHPanel *panel, const int iarm, const int ipanel, const char *det)
{
  TClonesArray &newpanel = *SvPHPanel;
  new(newpanel[nPanel]) SavePHPanelv1();
  static_cast<SavePHPanel *> (newpanel[nPanel])->AddPanel(panel, iarm, ipanel, det);
  nPanel++;
  if (nPanel == (int) currentTCsize)
    {
      currentTCsize += NPANEL;
      SvPHPanel->Expand(currentTCsize);
    }
  return 0;
}
