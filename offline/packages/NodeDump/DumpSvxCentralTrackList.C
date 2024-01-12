#include "DumpSvxCentralTrackList.h"

#include <SvxCentralTrackList.h>
#include <SvxCentralTrack.h>
#include <SvxClusterInfo.h>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<SvxCentralTrackList> MyNode_t;

DumpSvxCentralTrackList::DumpSvxCentralTrackList(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpSvxCentralTrackList::process_Node(PHNode *myNode)
{
  SvxCentralTrackList *svxcentraltracklist = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      svxcentraltracklist = thisNode->getData();
    }
  if (svxcentraltracklist && svxcentraltracklist->isValid())
    {
      *fout << "get_nCentralTracks(): " << svxcentraltracklist->get_nCentralTracks() << endl;
      for (int i = 0; i < svxcentraltracklist->get_nCentralTracks(); i++)
        {
          SvxCentralTrack *sngl = svxcentraltracklist->getCentralTrack(i);
          *fout << "getNhits(): " << sngl->getNhits() << endl;
          *fout << "getHitPattern(): " << (short) sngl->getHitPattern() << endl;
          *fout << "getQuality(): " << sngl->getQuality() << endl;
          *fout << "getDCA2D(): " << sngl->getDCA2D() << endl;
          *fout << "getD2DCA0(): " << sngl->getD2DCA0() << endl;
          *fout << "isPrimary(): " << sngl->isPrimary() << endl;
          *fout << "get_dEdX1(): " << sngl->get_dEdX1() << endl;
          *fout << "get_dEdX2(): " << sngl->get_dEdX2() << endl;
          *fout << "getVtxID(): " << sngl->getVtxID() << endl;
          *fout << "getChiSquare(): " << sngl->getChiSquare() << endl;
          *fout << "getNDF(): " << sngl->getNDF() << endl;
          *fout << "getNDOF(): " << sngl->getNDOF() << endl;
          *fout << "getChiSquareDPHI(): " << sngl->getChiSquareDPHI() << endl;
          *fout << "getNDFDPHI(): " << sngl->getNDFDPHI() << endl;
          *fout << "getChiSquareDZ(): " << sngl->getChiSquareDZ() << endl;
          *fout << "getNDFDZ(): " << sngl->getNDFDZ() << endl;
          *fout << "getChiSquare2(): " << sngl->getChiSquare2() << endl;
          *fout << "getUnique(): " << sngl->getUnique() << endl;
	  for (int i=0; i<2;i++)
	    { 
	      *fout << "getRotatedAngle(" << i << "): " << sngl->getRotatedAngle(i) << endl;
	    }
	  for (int i=0; i<3;i++)
	    { 
         *fout << "get3MomentumAtPrimaryVertex(" << i << "): " << sngl->get3MomentumAtPrimaryVertex(i) << endl;
          *fout << "getClosestApproach(" << i << "): " << sngl->getClosestApproach(i) << endl;
          *fout << "get3MomErrorAtPrimaryVertex(" << i << "): " << sngl->get3MomErrorAtPrimaryVertex(i) << endl;
	    }
	  for (int i=0; i<sngl->getNhits();i++)
	    {
	      SvxClusterInfo *info = sngl->getClusterInfo(i);
	      *fout << "info->getClusterId(" << i << "): " << info->getClusterId() << endl;
	      *fout << "info->getLayer(" << i << "): " << (short) info->getLayer() << endl;
	      *fout << "info->getLadder(" << i << "): " << (short) info->getLadder() << endl;
	      *fout << "info->getSensor(" << i << "): " << (short) info->getSensor() << endl;
	      *fout << "info->getSize(" << i << "): " << info->getSize() << endl;
	      *fout << "info->getEdgeFlag(" << i << "): " << info->getEdgeFlag() << endl;
	      *fout << "info->getCircumference(" << i << "): " << info->getCircumference() << endl;
	      *fout << "info->getAmbiguous(" << i << "): " << info->getAmbiguous() << endl;
	      *fout << "info->getdphi(" << i << "): " << info->getdphi() << endl;
	      *fout << "info->getdz(" << i << "): " << info->getdz() << endl;
	      *fout << "info->getdproj(" << i << "): " << info->getdproj() << endl;
	      *fout << "info->getbend(" << i << "): " << info->getbend() << endl;
	      *fout << "info->getzproj(" << i << "): " << info->getzproj() << endl;
	      *fout << "info->getfitdphi(" << i << "): " << info->getfitdphi() << endl;
	      *fout << "info->getfitdz(" << i << "): " << info->getfitdz() << endl;
	      *fout << "info->get_sublayer(" << i << "): " << info->get_sublayer() << endl;
	      for (int j=0; j<2; j++)
		{
		  *fout << "info->getXZSize(" << i << "," << j << "): " << info->getXZSize(j) << endl;
		  *fout << "info->getAdc(" << i << "," << j << "): " << info->getAdc(j) << endl;
		}
	      for (int j=0; j<3; j++)
		{
		  *fout << "info->getPosition(" << i << "," << j << "): " << info->getPosition(j) << endl;
		}
	    }
        }
    }
  return 0;
}

