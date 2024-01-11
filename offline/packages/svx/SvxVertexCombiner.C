#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHIODataNode.h>
#include <getClass.h>

#include <svxDetectorGeo.hh>
#include <SvxCluster.h>
#include <SvxClusterList.h>
#include <SvxVertexCombiner.h>
#include <SvxSegmentList.h>
#include <SvxSegment.h>
#include <SvxTracker.h>
#include <RunHeader.h>
#include <VtxOut.h>
#include <VtxOrdercodes.h>
#include <PHPoint.h>
#include <getClass.h>
#include <TMath.h>

#include <cmath>
using namespace std;
using namespace findNode;

SvxVertexCombiner::SvxVertexCombiner(const string &name):
  m_beamspotsizex(0.0130),
  m_beamspotsizey(0.0110)
{
}


//! Basic destructor
SvxVertexCombiner::~SvxVertexCombiner()
{
}


int SvxVertexCombiner::InitRun(PHCompositeNode *topNode)
{
    PHNodeIterator iter(topNode);

    // Search SvxVtxOut node.
    // If you can find it, use it. Otherwise, use VtxOut node.
    //  the node is actually gotten in the function GetNodes()
    //  which is called at the top of process event

    return EVENT_OK;
}



int SvxVertexCombiner::process_event(PHCompositeNode *topNode)
{

    VtxOut *vtxout = getClass<VtxOut>(topNode, "VtxOut");
    if (vtxout == NULL)
      {
	cout << "No VtxOut in the NODE tree THIS IS BAD!" << endl;
	return EVENT_OK;
      }

    SvxSegmentList* svxsegmentlist = findNode::getClass<SvxSegmentList>(topNode,"SvxSegmentList");
    if(!svxsegmentlist) {cerr << PHWHERE << "ERROR: No SvxsegmentList object" << endl; return DISCARDEVENT;}

    float nsegments = svxsegmentlist->get_nSegments();
    // identify the number of good segments per events
    float ngoodseg = 0;
    for (int i = 0;i<nsegments;i++)
      {
	SvxSegment* svxsegment = svxsegmentlist->get_segment(i);
	float px = svxsegment->get3Momentum(0);
	float py = svxsegment->get3Momentum(1);
	float pt = sqrt(px*px+py*py);
	float chisq = svxsegment->getChiSq();
	float ndf = svxsegment->getNDF();
	if (abs(pt)> 0.2 && chisq/ndf<6)
	  {
	    ngoodseg = ngoodseg +1;
	  }
      }
    
  


    PHPoint verpoint_precise = vtxout->get_Vertex("SVX_PRECISE");
    float px = verpoint_precise.getX();
    float py = verpoint_precise.getY();
    float pz = verpoint_precise.getZ();

    PHPoint verpoint_preciseerror = vtxout->get_VertexError("SVX_PRECISE");
    float verr[3];
    verr[0] = verpoint_preciseerror.getX();
    verr[1] = verpoint_preciseerror.getY();
    verr[2] = verpoint_preciseerror.getZ();


    PHPoint verpoint_seed = vtxout->get_Vertex("SVX");
    float sx = verpoint_seed.getX();
    float sy = verpoint_seed.getY();
    float sz = verpoint_seed.getZ();
    PHPoint verpoint_seederror = vtxout->get_VertexError("SVX");
    float svxerr[3];
    svxerr[0] = verpoint_seederror.getX();
    svxerr[1] = verpoint_seederror.getY();
    svxerr[2] = verpoint_seederror.getZ();

    if (abs(px) < 700)
      {
	// extracted from a simulation study of the precise vertex resolution as a function of
	// the number of "good" segments in the event. These numbers should 
	// not change for different types of events, pp or AuAu 
	float sigpx =  2.01452e-02/(TMath::Power(ngoodseg,5.38641e-01));
	float sigpy =  1.60394e-02/(TMath::Power(ngoodseg,5.62712e-01));

	float sigsx = m_beamspotsizex;
	float sigsy = m_beamspotsizey;
	float calcvtx[3];

	calcvtx[0] = (sigpx*sigpx)*(sigsx*sigsx)*((px/(sigpx*sigpx))+(sx/(sigsx*sigsx)))/(sigpx*sigpx + sigsx*sigsx);
	calcvtx[1] = (sigpy*sigpy)*(sigsy*sigsy)*((py/(sigpy*sigpy))+(sy/(sigsy*sigsy)))/(sigpy*sigpy + sigsy*sigsy);
	calcvtx[2] = pz;


	vtxout->AddVtx("SVX_COMBINED",calcvtx,verr,22);
      }
    else 
      {
	float seedvtx[3];
	seedvtx[0] = sx;
	seedvtx[1] = sy;
	seedvtx[2] = sz;


	vtxout->AddVtx("SVX_COMBINED",seedvtx,svxerr,22);
      }
    return EVENT_OK;
}
