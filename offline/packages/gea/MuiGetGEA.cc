#include <MuiPISAHit.h>
#include <munhitsWrapper.h>
#include <getClass.h>

using namespace std;

long
MuiGetGEA(PHCompositeNode* topNode)
{

  MuiPISAHit *event = MuiPISAHit::GetMuiHitEvt();
  Int_t muiRows = MuiPISAHit::GetMuiCount();    // variable number of rows

  // Instantiate the GEA table for this subsystem, and attach it to
  // the GEA "directory" node.
  munhitsWrapper* w = findNode::getClass<munhitsWrapper>(topNode,"munhits");
  if ( ! w ) 
    {
      std::cout << "MuiGetGEA - could not find node munhits" << std::endl;
      return -1;
    }

  w->SetMaxRowCount(muiRows);

  MUNHITS_ST* munhits = w->TableData();

  for(int i=0; i<muiRows; i++) {
    munhits[i].track_num  = event[i].GetMctrack();
    munhits[i].plane_num  = event[i].GetPlane_num();
    munhits[i].trk_id  = event[i].GetTrk_id();
    munhits[i].tof     = event[i].GetTof();
    munhits[i].de      = event[i].GetDe();
    munhits[i].rhit[0] = event[i].GetRhit1();
    munhits[i].rhit[1] = event[i].GetRhit2();
    munhits[i].rhit[2] = event[i].GetRhit3();
    munhits[i].phit[0] = event[i].GetPhit1();
    munhits[i].phit[1] = event[i].GetPhit2();
    munhits[i].phit[2] = event[i].GetPhit3();
    munhits[i].itrsub  = event[i].GetIsubevent();
    munhits[i].itrksub = event[i].GetNtrack();
  }
  w->SetRowCount(muiRows);

 return 0;
}
