//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "SpinDataEventOut.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpSpinDataEventOut.h"

using namespace std;

typedef PHIODataNode<SpinDataEventOut> MyNode_t;

DumpSpinDataEventOut::DumpSpinDataEventOut(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpSpinDataEventOut::process_Node(PHNode *myNode)
{
  SpinDataEventOut *spindataeventout = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      spindataeventout = thisNode->getData();
    }
  if (spindataeventout)
    {
      *fout << "SpinDataEventOut->isValid: " << spindataeventout->isValid() << endl;
      if (spindataeventout->isValid())
        {
          *fout << "GetEventSequence(): " << spindataeventout->GetEventSequence() << endl;
          *fout << "GetGL1CrossingID(): " << spindataeventout->GetGL1CrossingID() << endl;
          *fout << "GetSpinDirectionBlueFromV124(): " << spindataeventout->GetSpinDirectionBlueFromV124() << endl;
          *fout << "GetSpinDirectionYellowFromV124(): " << spindataeventout->GetSpinDirectionYellowFromV124() << endl;
          *fout << "GetSpinDirectionBlueFromCDEVFillPattern(): " << spindataeventout->GetSpinDirectionBlueFromCDEVFillPattern() << endl;
          *fout << "GetSpinDirectionYellowFromCDEVFillPattern(): " << spindataeventout->GetSpinDirectionYellowFromCDEVFillPattern() << endl;
          *fout << "GetGL1PSumEventNumber(): " << spindataeventout->GetGL1PSumEventNumber() << endl;
          *fout << "GetGL1PSumCrossingID(): " << spindataeventout->GetGL1PSumCrossingID() << endl;
          *fout << "GetGL1PSumCrossingCount(): " << spindataeventout->GetGL1PSumCrossingCount() << endl;
          *fout << "GetSpinGL1CrossingID(): " << spindataeventout->GetSpinGL1CrossingID() << endl;
          *fout << "GetSpinGL1PSumCrossingID(): " << spindataeventout->GetSpinGL1PSumCrossingID() << endl;
          for (int i = 0; i < nGL1PBoard; i++)
            {
              *fout << "GetGL1PEventNumber(" << i << "): " << spindataeventout->GetGL1PEventNumber(i) << endl;
              *fout << "GetGL1PCrossingID(" << i << "): " << spindataeventout->GetGL1PCrossingID(i) << endl;
              *fout << "GetSpinGL1PCrossingID(" << i << "): " << spindataeventout->GetSpinGL1PCrossingID(i) << endl;
              for (int j = 0 ; j < nGL1PScaler ; j++)
                {
                  *fout << "GetGL1PScalerCount(" << i << "," << j << "): " << spindataeventout->GetGL1PScalerCount(i,j) << endl;
                  *fout << "GetGL1PSumScalerCount(" << i << "," << j << "): " << spindataeventout->GetGL1PSumScalerCount(i,j) << endl;
                }
            }
        }
    }
  return 0;
}

