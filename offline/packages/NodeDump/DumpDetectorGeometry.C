#include "DumpDetectorGeometry.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"

#include "DetectorGeometry.h"
#include "SavePHPanel.h"

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "TClonesArray.h"

//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

using namespace std;

typedef PHIODataNode<DetectorGeometry> MyNode_t;

DumpDetectorGeometry::DumpDetectorGeometry(const string &NodeName): DumpObject(NodeName)
{
  write_run_event = 0; // do not write info for each event
  return ;
}

int DumpDetectorGeometry::process_Node(PHNode *myNode)
{
  DetectorGeometry *detectorgeometry = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      detectorgeometry = thisNode->getData();
    }
  if (detectorgeometry)
    {
      TClonesArray *TC = detectorgeometry->GetPHPanelTCArray();
      {
        for (int i = 0; i <= TC->GetLast(); i++)
          {
            SavePHPanel *svpanel = static_cast<SavePHPanel *> (TC->UncheckedAt(i));
            *fout << "Panel No: " << i << endl;
            *fout << "GetName(): " << svpanel->GetName() << endl;
            *fout << "GetArm(): " << svpanel->GetArm() << endl;
            *fout << "P0: ("
		  << svpanel->GetPoint(0, 0) << ", "
		  << svpanel->GetPoint(0, 1) << ", "
		  << svpanel->GetPoint(0, 2) << ")" << endl;
            *fout << "P1: ("
		  << svpanel->GetPoint(1, 0) << ", "
		  << svpanel->GetPoint(1, 1) << ", "
		  << svpanel->GetPoint(1, 2) << ")" << endl;
            *fout << "P2: ("
		  << svpanel->GetPoint(2, 0) << ", "
		  << svpanel->GetPoint(2, 1) << ", "
		  << svpanel->GetPoint(2, 2) << ")" << endl;
          }
      }
    }
  return 0;
}

