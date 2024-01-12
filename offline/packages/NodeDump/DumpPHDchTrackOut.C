//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>
#include <math.h>
#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "PHDchTrackOut.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpPHDchTrackOut.h"

using namespace std;

typedef PHIODataNode<PHDchTrackOut> MyNode_t;

DumpPHDchTrackOut::DumpPHDchTrackOut(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpPHDchTrackOut::process_Node(PHNode *myNode)
{
  PHDchTrackOut *phdchtrack = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      phdchtrack = thisNode->getData();
    }
  if (phdchtrack && phdchtrack->isValid())
    {
      *fout << "get_DchNTrack(): " << phdchtrack->get_PHDchNTrack() << endl;
      for (unsigned int i = 0; i < phdchtrack->get_PHDchNTrack(); i++)
        {
          *fout << "get_numberOfX1X2hitsFitted(" << i << "):" << phdchtrack->get_numberOfX1X2hitsFitted(i) << endl;
          *fout << "get_numberOfSuccessfulIterations(" << i << "):" << phdchtrack->get_numberOfSuccessfulIterations(i) << endl;
          *fout << "get_chi2(" << i << "):" << phdchtrack->get_chi2(i) << endl;
          *fout << "get_ErrorCode(" << i << "):" << phdchtrack->get_ErrorCode(i) << endl;
	  if (finite(phdchtrack->get_momentum(i)))
	    {
          *fout << "get_momentum(" << i << "):" << phdchtrack->get_momentum(i) << endl;
	    }
	  else
	    {
	      cout << "PHDch: Bad phi0: " << phdchtrack->get_fittedPhi0(i) 
		   << " for track " << i 
		   << " momentum: " << phdchtrack->get_momentum(i)
                   << endl;
	    }

          *fout << "get_fittedAlpha(" << i << "):" << phdchtrack->get_fittedAlpha(i) << endl;
          *fout << "get_fittedPhi(" << i << "):" << phdchtrack->get_fittedPhi(i) << endl;
          *fout << "get_fittedPhi0(" << i << "):" << phdchtrack->get_fittedPhi0(i) << endl;
          *fout << "get_fittedBeta(" << i << "):" << phdchtrack->get_fittedBeta(i) << endl;
          *fout << "get_fittedTheta0(" << i << "):" << phdchtrack->get_fittedTheta0(i) << endl;
        }

    }
  return 0;
}

