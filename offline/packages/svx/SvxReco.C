// ===============
// FILE: SvxReco.C
// ===============

// ******************************************************
//
// Class: SvxReco implementation
//
// Author:  Sasha Lebedev (lebedev@iastate.edu)
//
// Revisions: December 2009 - initial version
//
// ***************************************************************************

#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <cmath>
#include <cstdio>

#include <SvxCommon.h>
#include <SvxReco.h>
#include <SvxRawhitv1.h>
#include <SvxRawhitListv1.h>
#include <SvxClusterv4.h>
#include <SvxClusterListv5.h>
#include <SvxRawhitClusterv1.h>
#include <SvxRawhitClusterListv1.h>
#include <SvxSegmentv3.h>
#include <SvxSegmentListv6.h>
#include <SvxEventInfov3.h>
#include <SvxPixel1v1.h>
#include <SvxStrip11v2.h>
#include <SvxPisaHitv1.h>
#include <RunHeader.h>

#include <SvxGhitv1.h>
#include <SvxGhitListv1.h>
#include <SvxGhitRawhitv1.h>
#include <SvxGhitRawhitListv1.h>
#include <SvxGhitClusterv1.h>
#include <SvxGhitClusterListv1.h>
#include <svxDetectorGeo.hh>
#include <SvxDeadMap.h>
#include <SvxBeamCenterPar.h>
#include <SvxClusterContainer.h>
#include <SvxStripThreshold.h>

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>

#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCoordinate.hh>
#include <PdbSvxPixelHotDeadPixelMap.hh>

#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <getClass.h>
#include <recoConsts.h>

#include <RunToTime.hh>

#include <TFile.h>

using namespace std;

// Helpers for scanning Node Tree...
typedef PHIODataNode <PHObject>             PHObjectNode_t;
typedef PHIODataNode <SvxRawhitList>        SvxRawhitListNode_t;
typedef PHIODataNode <SvxClusterList>       SvxClusterListNode_t;
typedef PHIODataNode <SvxRawhitClusterList> SvxRawhitClusterListNode_t;
typedef PHIODataNode <SvxSegmentList>       SvxSegmentListNode_t;
typedef PHIODataNode <SvxEventInfo>         SvxEventInfoNode_t;
typedef PHIODataNode <SvxGhitList>          SvxGhitListNode_t;
typedef PHIODataNode <SvxGhitRawhitList>    SvxGhitRawhitListNode_t;
typedef PHIODataNode <SvxGhitClusterList>   SvxGhitClusterListNode_t;

typedef PHIODataNode<RunHeader> RunHeaderNode_t;

// SVX geometry definitions: sensor types in the barrel layers
const unsigned int SvxReco::barSenType[] = {1, 1, 11, 11}; // sensor type/layer

//------------------------------------------------------------------------------

/**
 * Class constructor
 *
 * \param[in] name Name given to module upon initialization
 *
 */
SvxReco::SvxReco(const string &name):
    SubsysReco(name),
    d_ghit(NULL),
    d_rawhit(NULL),
    d_ghit2rawhit(NULL),
    d_cluster(NULL),
    d_fake_cluster(NULL),
    d_rawhit2cluster(NULL),
    d_ghit2cluster(NULL),
    d_segment(NULL),
    d_eventinfo(NULL),
    nSvxRawhits(0),
    nSvxClusters(0),
    nSvxSegments(0),
    m_enableGoodAdcRange(true),
    _timer(PHTimeServer::get()->insert_new(name))
{
    barSensor = NULL; // added by T.Hachiya 2011.12.24


    for (int i = 0; i < NSVXSECTIONS; i++)
        svxSecNlayers[i] = 0;

    for (int i = 0; i < SVXLADDERNUMBER; i++)
        nBarLadder[i] = 0;

    for (int i = 0; i < SVXSENSORNUMBER; i++)
        nBarSensor[i] = 0;

    d_container = new SvxClusterContainer();

    // Default parameters for stripixel
    m_stripixel_adcthre_zs         = 31; // 3 sigma of sNOISE
    m_stripixel_adcthre_rawhit     = 0; // set to zero (CHC)
    m_stripixel_adcthre_rawhit_sum = 0; // set to zero (CHC)

    ThisIsSimulation = false;

    _StripThresholds = NULL;

    EventNumber = 0;

}

//------------------------------------------------------------------------------

/**
 * Class Destructor
 */
SvxReco::~SvxReco()
{
    if ( barSensor != NULL )
        delete [] barSensor;

    /// d_container should not be deleted here.
    return;
}

//------------------------------------------------------------------------------

/**
 * Set the verbosity level for this module, and also the SvxClusterContainer
 * object (d_container) created here.
 *
 * \param[in] ival Verbosity level 0:quite, >0:verbose
 *
 */
void SvxReco::Verbosity(const int ival)
{
    SubsysReco::Verbosity(ival);
    d_container->verbosity(ival);
}

//------------------------------------------------------------------------------

/**
 * Set the zero suppression value for the stipixel detector.
 *
 * \param[in] val Zero suppression value for strippixel in ADC
 *
 */
void SvxReco::set_StripixelZeroSup( int val )
{
    cout << PHWHERE << "Zero suppression of stripixel is changed from "
         << m_stripixel_adcthre_zs << " to " << val
         << endl;
    m_stripixel_adcthre_zs = val;
}

//------------------------------------------------------------------------------

//since we have SvxStripThreshold.h
//set_StripixelAdcThreshold and set_StripixelAdcSumThreshold could be commented out, by CHC
/**
 * Set the threshold value for summed ADC used in stripixel clustering.
 *
 * \param[in] val Threshold for rawhit summed ADC used in strippixel clustering
 *
 */
void SvxReco::set_StripixelAdcSumThreshold(int val)
{
    cout << PHWHERE
         << "Threshold of rawhit summed ADC for clustering is changed from "
         << m_stripixel_adcthre_rawhit_sum << " to " << val
         << endl;
    m_stripixel_adcthre_rawhit_sum = val;
}

//------------------------------------------------------------------------------

/*
 * Run-independent initalization of class variables
 */
int SvxReco::Init(PHCompositeNode *topNode)
{
    if ( verbosity > 0 )
        cout << "SvxReco::Init-I: Execution started." << endl;

    ///////////////////////
    // detectorGeo
    // added by T.Hachiya 2011.08.14
    svxDetectorGeo* svxGeometry =
        findNode::getClass<svxDetectorGeo>(topNode, "svxDetectorGeo");
    if ( svxGeometry == NULL)
    {
        if ( verbosity > 0 )
        {
            cout << PHWHERE << "Can't find svxDetectorGeo. " << endl;
        }
        return ABORTRUN;
    }


    barSensor = new SvxSensor *[SVXLAYERNUMBER][SVXLADDERNUMBER][SVXSENSORNUMBER];

    for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
        nBarLadder[i] = svxGeometry->get_nBarLadder(i);
        nBarSensor[i] = svxGeometry->get_nBarSensor(i);
    }
    svxSecNlayers[0] = SVXLAYERNUMBER;

    for (unsigned int ilr = 0; ilr < SVXLAYERNUMBER; ilr++)
    {
        for (unsigned int ild = 0; ild < nBarLadder[ilr]; ild++)
        {
            for (unsigned int isn = 0; isn < nBarSensor[ilr]; isn++)
            {
                barSensor[ilr][ild][isn] = svxGeometry->GetSensorPtr(ilr, ild, isn);
                // set parameters for stripixel
                if ( ilr > 1 )
                {
                    barSensor[ilr][ild][isn]->set_adcthre_zs(m_stripixel_adcthre_zs);
                    barSensor[ilr][ild][isn]->set_adcthre_rawhit(m_stripixel_adcthre_rawhit);
                    barSensor[ilr][ild][isn]->set_adcthre_rawhit_sum(m_stripixel_adcthre_rawhit_sum);
                }
            }
        }
    }

    if ( verbosity > 0 )
    {
        cout << "Sensor Parameter " << endl;
        cout << endl << "pixel : " << endl;
        barSensor[0][0][0]->printPar();
        cout << endl << "strip : " << endl;
        barSensor[2][0][0]->printPar();
    }

    if ( verbosity > 0 )
        cout << "SvxReco::Init-I: Execution completed." << endl;

    return EVENT_OK;

}

//------------------------------------------------------------------------------

/**
 * Run-dependent initialization of class variables
 */
int SvxReco::InitRun(PHCompositeNode *topNode)
{

    if ( verbosity > 0 )
        cout << "SvxReco::InitRun-I: Execution started.." << endl;

    // Set up the node tree and return if failed
    int i = CreateNodeTree(topNode);
    if ( verbosity > 0 )
        cout << "SvxReco::InitRun-I: CreateNodeTree returned " << i << endl;
    if ( !(i == EVENT_OK) )
        return EVENT_OK;


    // Get SvxAddress off the node tree
    // used in .....
    // added by T.Hachiya 2011.06.17
    svxAddress* address =
        findNode::getClass<svxAddress>(topNode, "svxAddress");
    if ( address == NULL)
    {
        if ( verbosity > 0 )
        {
            cout << PHWHERE << "Can't find svxAddress. " << endl;
        }
        return ABORTRUN;
    }

    // Get the beam center parameters for use in SvxClusterContainer
    // added by T.Hachiya 2011.12.21
    SvxBeamCenterPar* beamcenter =
        findNode::getClass<SvxBeamCenterPar>(topNode, "SvxBeamCenterPar");
    if ( beamcenter == NULL)
    {
        if ( verbosity > 0 )
            cout << PHWHERE << "Can't find BeamCenter object. " << endl;

        return ABORTRUN;
    }

    double beam_x = beamcenter->getBeamCenter(0);
    double beam_y = beamcenter->getBeamCenter(1);
    d_container->set_beam_center(beam_x, beam_y);


    // Strip Threshold
    _StripThresholds =
        findNode::getClass<SvxStripThreshold>(topNode, "SvxStripThreshold");
    if ( _StripThresholds == NULL )
    {
        if ( verbosity > 0 )
            cout << PHWHERE << "Can't find SvxStripThreshold. " << endl;

        return ABORTRUN;
    }


    if ( verbosity > 0 )
    {
        cout << "Sensor Parameter " << endl;
        cout << endl << "pixel : " << endl;
        barSensor[0][0][0]->printPar();
        cout << endl << "strip : " << endl;
        barSensor[2][0][0]->printPar();
    }


    if ( verbosity > 0 )
        cout << "SvxReco::InitRun-I: Node tree created." << endl;

    return EVENT_OK;

}

//------------------------------------------------------------------------------

/**
 * Get required data and fill clusters for each event.
 *
 * \param[in] topNode Pointer to the top node on the node list
 *
 */
int SvxReco::process_event(PHCompositeNode *topNode)
{
    _timer.get()->restart();

    if ( verbosity > 0 )
        cout << "SvxReco::process_event-I: Execution started..." << endl;

    if ( verbosity > 0 && EventNumber % 1000 == 0 )
        cout << "SvxReco::process_event() event# " << EventNumber << endl;

    int iError;

    // Initialization
    nSvxRawhits        = 0;
    nSvxClusters       = 0;
    nSvxSegments       = 0;

    if ( verbosity > 0 )
        cout << "SvxReco::process_event-I: Getting nodes." << endl;
    GetNodes(topNode);

    ///////////////////////
    // added by T.Hachiya 2011.06.17
    svxAddress* address = findNode::getClass<svxAddress>(topNode, "svxAddress");
    if ( address == NULL )
    {
        if ( verbosity > 0 )
            cout << PHWHERE << "Can't find svxAddress. " << endl;

        return ABORTRUN;
    }
    svxAddress& SvxAddressObject = *address;

    // Reset SVX sensors
    // """""""""""""""""
    for ( unsigned int ilr = 0; ilr < svxSecNlayers[0]; ilr++ )
    {   /// svxNlayers[0] = SVXLAYERNUMBER
        for ( unsigned int ild = 0; ild < nBarLadder[ilr]; ild++ )
        {   /// nBarLadder[ilr] = number of ladder in the layer
            for ( unsigned int isn = 0; isn < nBarSensor[ilr]; isn++ )
            {   /// nBarSensor[ilr] = number of sensor per ladder in the layer
                barSensor[ilr][ild][isn]->Reset();
            } // isn
        } // ild
    } // ilr


    if ( verbosity > 0 )
        cout << "SvxReco::process_event-I: filling SVX clusters" << endl;

    iError = fillClusterList(SvxAddressObject);
    if ( iError == -1 )
        cerr << PHWHERE << " ERROR filling SVX clusters." << endl;


    if ( verbosity > 0 )
        cout << "SvxReco::process_event-I: Number of clusters: " << iError << endl;

    if ( verbosity > 0 )
    {
        cout << "Final number of clusters: "
             << d_cluster->get_nClusters()
             << endl;
        cout << "Number of rawhit2cluster entries: "
             << d_rawhit2cluster->get_nRawhitClusters()
             << endl;
    }

    _timer.get()->stop();


    EventNumber++;

    if ( verbosity > 0 )
        cout << "SvxReco::process_event-I: Event processed." << endl;

    return EVENT_OK;
}

//------------------------------------------------------------------------------

/**
 * Create SVX data nodes as needed:
 *    SVXSUB               (on SVX node)
 *    SvxClusterContainer  (on SVXSUB node)
 *    SvxClusterList       (on SVX node)
 *    SvxFakeClusterList   (on SVX node)
 *    SvxRawhitClusterList (on SVX node)
 *    SvxSegmentList       (on SVX node)
 *    SvxEventInfo         (on SVX node)
 *    SvxGhitClusterList   (on SVX node) (ThisIsSimulation==true only)
 * Requires that the following nodes exist:
 *    DST
 *    SVX
 *    SvxRawhitList
 *
 * \param[in] topNode Pointer to the top node on the node list
 *
 * \return True if execution successful, else False
 */
int SvxReco::CreateNodeTree(PHCompositeNode *topNode)
{

    if ( verbosity > 0 )
        cout << "SvxReco::CreateNodeTree-I: Execution started" << endl;

    PHNodeIterator iter(topNode);



    //
    // Nodes that must already exist on the node tree
    //
    PHCompositeNode *dstNode;
    dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
    if ( !dstNode )
    {
        cerr << PHWHERE << "DST node missing, doing nothing." << endl;
        return EVENT_OK;
    }

    // Find SVX node.
    PHCompositeNode* svxNode = dynamic_cast<PHCompositeNode*> (iter.findFirst("PHCompositeNode", "SVX"));
    if ( !svxNode )
    {
        cerr << PHWHERE << "SVX node missing, doing nothing." << endl;
        return EVENT_OK;
    }

    PHIODataNode<PHObject>* SvxRawhitListNode = NULL;
    SvxRawhitListNode =
        (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxRawhitList");
    if ( !SvxRawhitListNode )
    {
        cerr << PHWHERE << "SvxRawhitList node missing, doing nothing." << endl;
        return EVENT_OK;
    }



    //
    // Create nodes if missing
    //
    PHCompositeNode *svxsubNode =
        dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "SVXSUB"));
    if ( !svxsubNode )
    {
        svxsubNode = new PHCompositeNode("SVXSUB");
        /// SVXSUB node is created on topNode
        topNode->addNode(svxsubNode);
    }

    PHDataNode<SvxClusterContainer>* SvxClusterContainerNode = NULL;
    SvxClusterContainerNode = (PHIODataNode<SvxClusterContainer>*)iter.findFirst("PHDataNode", "SvxClusterContainer");
    if ( !SvxClusterContainerNode )
    {
        SvxClusterContainerNode =
            new PHDataNode<SvxClusterContainer>(d_container, "SvxClusterContainer");
        svxsubNode->addNode(SvxClusterContainerNode);
    }

    PHIODataNode<PHObject>* SvxClusterListNode = NULL;
    SvxClusterListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxClusterList");
    if ( !SvxClusterListNode )
    {
        SvxClusterList* svxclusters = new SvxClusterListv5();
        SvxClusterListNode =
            new PHIODataNode<PHObject>(svxclusters, "SvxClusterList", "PHObject");
        svxNode->addNode(SvxClusterListNode);
    }

    PHIODataNode<PHObject>* SvxFakeClusterListNode = NULL;
    SvxFakeClusterListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxFakeClusterList");
    if ( !SvxFakeClusterListNode )
    {
        SvxClusterList* svxfakeclusters = new SvxClusterListv5();
        SvxFakeClusterListNode =
            new PHIODataNode<PHObject>(svxfakeclusters, "SvxFakeClusterList", "PHObject");
        svxNode->addNode(SvxFakeClusterListNode);
    }

    PHIODataNode<PHObject>* SvxRawhitClusterListNode = NULL;
    SvxRawhitClusterListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxRawhitClusterList");
    if ( !SvxRawhitClusterListNode )
    {
        SvxRawhitClusterList* svxrawhit2clusters = new SvxRawhitClusterListv1();
        SvxRawhitClusterListNode =
            new PHIODataNode<PHObject>(svxrawhit2clusters, "SvxRawhitClusterList",
                                       "PHObject");
        svxNode->addNode(SvxRawhitClusterListNode);
    }

    PHIODataNode<PHObject>* SvxSegmentListNode = NULL;
    SvxSegmentListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxSegmentList");
    if ( !SvxSegmentListNode )
    {
        SvxSegmentList* svxsegments = new SvxSegmentListv6();
        SvxSegmentListNode =
            new PHIODataNode<PHObject>(svxsegments, "SvxSegmentList", "PHObject");
        svxNode->addNode(SvxSegmentListNode);
    }

    PHIODataNode<PHObject>* SvxEventInfoNode = NULL;
    SvxEventInfoNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxEventInfo");
    if ( !SvxEventInfoNode )
    {
        SvxEventInfo* svxeventinfo = new SvxEventInfov3();
        SvxEventInfoNode =
            new PHIODataNode<PHObject>(svxeventinfo, "SvxEventInfo", "PHObject");
        svxNode->addNode(SvxEventInfoNode);
    }

    if ( ThisIsSimulation )
    {
        PHIODataNode<PHObject>* SvxGhitClusterListNode = NULL;
        SvxGhitClusterListNode = (PHIODataNode<PHObject>*)iter.findFirst
                                 ("PHIODataNode", "SvxGhitClusterList");
        if ( !SvxGhitClusterListNode )
        {
            SvxGhitClusterList* svxghit2clusters = new SvxGhitClusterListv1();
            SvxGhitClusterListNode =
                new PHIODataNode<PHObject>(svxghit2clusters, "SvxGhitClusterList", "PHObject");
            svxNode->addNode(SvxGhitClusterListNode);
        }
    }

    if ( verbosity > 0 )
        cout << "SvxReco::CreateNodeTree-I: Execution completed" << endl;

    return EVENT_OK;

}

//------------------------------------------------------------------------------

/**
 * Get pointers to all the required nodes on the node tree:
 *    SvxRawhitList
 *    SvxClusterList
 *    SvxFakeClusterList
 *    SvxRawhitClusterList
 *    SvxSegmentList
 *    SvxEventInfo
 *    SvxGhitList        (ThisIsSimulation=True only)
 *    SvxGhitRawhitList  (ThisIsSimulation=True only)
 *    SvxGhitClusterList (ThisIsSimulation=True only)
 *
 * \param[in] topNode Pointer to the top node on the node list
 *
 */
void SvxReco::GetNodes(PHCompositeNode *topNode)
{

    if ( verbosity > 0 )
        cout << "SvxReco::GetNodes-I: Execution started" << endl;

    // Set all pointers to null...
    d_rawhit         = NULL;
    d_cluster        = NULL;
    d_rawhit2cluster = NULL;
    d_segment        = NULL;
    d_eventinfo      = NULL;
    d_ghit           = NULL;
    d_ghit2rawhit    = NULL;
    d_ghit2cluster   = NULL;

    // Search out the nodes from the node tree...

    PHTypedNodeIterator<SvxRawhitList> iRAWHIT(topNode);
    SvxRawhitListNode_t *RAWHIT = iRAWHIT.find("SvxRawhitList");
    if ( RAWHIT )
        d_rawhit = RAWHIT->getData();
    if ( !d_rawhit )
        cerr << PHWHERE << "SvxReco::GetNodes() ERROR: rawhit data not in Node Tree" << endl;
    if ( verbosity > 0 )
    {
        cout << "SvxReco::GetNodes(): Number of raw hits = "
             << d_rawhit->get_nRawhits()
             << endl;
    }

    PHTypedNodeIterator<SvxClusterList> iCLUSTER(topNode);
    SvxClusterListNode_t *CLUSTER = iCLUSTER.find("SvxClusterList");
    if ( CLUSTER )
        d_cluster = CLUSTER->getData();
    if ( !d_cluster )
        cerr << PHWHERE << "SvxReco::GetNodes() ERROR:  cluster data not in Node Tree" << endl;

    PHTypedNodeIterator<SvxClusterList> iFAKECLUSTER(topNode);
    SvxClusterListNode_t *FAKECLUSTER = iFAKECLUSTER.find("SvxFakeClusterList");
    if ( FAKECLUSTER )
        d_fake_cluster = FAKECLUSTER->getData();
    if ( !d_fake_cluster )
        cerr << PHWHERE << "SvxReco::GetNodes() ERROR:  fake cluster data not in Node Tree" << endl;

    PHTypedNodeIterator<SvxRawhitClusterList> iRAWHIT2CLUSTER(topNode);
    SvxRawhitClusterListNode_t *RAWHIT2CLUSTER =
        iRAWHIT2CLUSTER.find("SvxRawhitClusterList");
    if ( RAWHIT2CLUSTER )
        d_rawhit2cluster = RAWHIT2CLUSTER->getData();
    if ( !d_rawhit2cluster )
        cerr << PHWHERE << "SvxReco::GetNodes() ERROR:  rawhit2cluster not in Node Tree" << endl;

    PHTypedNodeIterator<SvxSegmentList> iSEGMENT(topNode);
    SvxSegmentListNode_t *SEGMENT = iSEGMENT.find("SvxSegmentList");
    if ( SEGMENT )
        d_segment = SEGMENT->getData();
    if ( !d_segment )
        cerr << PHWHERE << "SvxReco::GetNodes() ERROR:  segment data not in Node Tree" << endl;

    PHTypedNodeIterator<SvxEventInfo> iEVENTINFO(topNode);
    SvxEventInfoNode_t *EVENTINFO = iEVENTINFO.find("SvxEventInfo");
    if ( EVENTINFO )
        d_eventinfo = EVENTINFO->getData();
    if ( !d_eventinfo )
        cerr << PHWHERE << "SvxReco::GetNodes() ERROR:  eventinfo data not in Node Tree" << endl;

    if (ThisIsSimulation)
    {
        PHTypedNodeIterator<SvxGhitList> iGHIT(topNode);
        SvxGhitListNode_t *GHIT = iGHIT.find("SvxGhitList");
        if ( GHIT )
            d_ghit = GHIT->getData();
        if ( !d_ghit )
            cerr << PHWHERE << "SvxReco:: ghit data not in Node Tree" << endl;

        PHTypedNodeIterator<SvxGhitRawhitList> iGHIT2RAWHIT(topNode);
        SvxGhitRawhitListNode_t *GHIT2RAWHIT = iGHIT2RAWHIT.find("SvxGhitRawhitList");
        if ( GHIT2RAWHIT )
            d_ghit2rawhit = GHIT2RAWHIT->getData();
        if ( !d_ghit2rawhit )
            cerr << PHWHERE << "SvxReco:: ghit2rawhit data not in Node Tree" << endl;

        PHTypedNodeIterator<SvxGhitClusterList> iGHIT2CLUSTER(topNode);
        SvxGhitClusterListNode_t *GHIT2CLUSTER = iGHIT2CLUSTER.find("SvxGhitClusterList");
        if ( GHIT2CLUSTER )
            d_ghit2cluster = GHIT2CLUSTER->getData();
        if ( !d_ghit2cluster )
            cerr << PHWHERE << "SvxReco:: ghit2cluster data not in Node Tree" << endl;
    }

    if ( verbosity > 0 )
        cout << "SvxReco::GetNodes-I: Execution completed" << endl;

    return;

}

//------------------------------------------------------------------------------

/**
 * Last function called prior to Destructor.
 */
int SvxReco::End(PHCompositeNode *topNode)
{
    return EVENT_OK;
}

//------------------------------------------------------------------------------

/**
 * Find all clusters and fill cluster list containers.
 *
 * Use only raw hits which pass quality and threshold cuts
 * Find clusters using SvxRawhitList (d_rawhit)
 * Fill SvxClusterList (d_cluster), SvxClusterContainer (d_container).
 *
 * \param[in] SvxAddressObject Reference to svxAddress object
 *
 * \return 0 if completed successfully, else -1
 */
int SvxReco::fillClusterList(svxAddress& SvxAddressObject)
{
    if ( verbosity > 0 )
    {
        cout << "SvxReco::fillClusterList-I: Execution started." << endl;
        cout << "SvxReco::fillClusterList-I: Processing "
             << nSvxRawhits << " SVX raw hits."
             << endl;
    }

    if ( ThisIsSimulation )
    {
        d_ghit->sort_hitID();
        d_ghit2rawhit->sortRawhits();
    }

    // Declare and initialize variables for tracking raw hits
    // in the pixel and strip subsystems
    // D. McGlinchey -- Not sure why the factors of 2 ...
    int nraw_pixel[SVXLADDERSLAYER0 * 2 + SVXLADDERSLAYER1 * 2][SVXSENSORSLAYER0]; /// 30 ladders & 4 sensors
    int nraw_strip[SVXNMODULESTRIP];  /// 30 ladders & 4 sensors
    for ( int i = 0; i < SVXLADDERSLAYER0 * 2 + SVXLADDERSLAYER1 * 2; i++ )
    {
        for ( int j = 0; j < SVXSENSORSLAYER0; j++ )
        {
            nraw_pixel[i][j] = 0;
        }
    }
    for ( int i = 0; i < SVXNMODULESTRIP; i++ )
    {
        nraw_strip[i] = 0;
    }

    // Loop over raw hits and fill SvxSensor objects
    unsigned int nraw = d_rawhit->get_nRawhits();
    for ( unsigned int iraw = 0; iraw < nraw; iraw++ )
    {
        SvxRawhit *tmp_raw = d_rawhit->get_Rawhit(iraw);

        /// skip this rawhitif if it should not be used for clustering
        if ( !tmp_raw->get_isOkForClustering() )
            continue;

        int layer = tmp_raw->get_layer();
        int ladder = tmp_raw->get_ladder();
        int sensor = tmp_raw->get_sensor();

        if ( layer == 0 || layer == 1 ) // pixel layers
        {
            if ( sensor >= 0 && (unsigned int)sensor < nBarSensor[layer] )
            {
                int roc = tmp_raw->get_pixelROC();
                int channel = tmp_raw->get_channel();
                int rawhitID = tmp_raw->get_hitID();
                int ix = SvxAddressObject.getPixelSensorIX0(roc, channel);
                int iz = SvxAddressObject.getPixelSensorIZ0(roc, channel);
                barSensor[layer][ladder][sensor]->set_hitmap_hit(ix, iz);
                barSensor[layer][ladder][sensor]->set_hitmap_rawhitID(ix, iz, rawhitID);
                nraw_pixel[ladder + layer * 10][sensor] ++;
            }
        }
        else // stripixel layer
        {

            //apply adc cut, by CHC
            // REMINDER: adc has been offset by 24. See SvxDecode.
            // adc >= 10 : 2 sigma cut on pedestal
            //             avg pedestal mean  = 0 (assuming 24 offset)
            //             avg pedestal width = 5
            // adc < 180 : prevent ADC pileup
            float adc = tmp_raw->get_adc();
            if ( m_enableGoodAdcRange && (adc < 10 || adc > 180) )
                continue;

            //DCMII threshold cut, by CHC
            int sensorSection = tmp_raw->get_sensorSection();
            int readout = tmp_raw->get_sensorReadout();
            int channel = tmp_raw->get_channel();
            int roc = SvxAddressObject.getStripRoc(sensorSection, readout, channel);
            int rocchnl = SvxAddressObject.getStripRocChannel(sensorSection, readout, channel);
            int th = -1 - 24;

            // Get the threshold set in the DCMII if valid channel
            if ( roc == -1 || rocchnl == -1 )
            {
                cerr << "SvxStripThreshold::" << __FUNCTION__ << " : "
                     << "Roc or RocChannel is out of range : "
                     << "ROC=" << roc
                     << "ROCChannel=" << rocchnl << endl;
                cerr << "  return -1 as threshold" << endl;
            }
            else
            {
                th = _StripThresholds->getThreshold(layer - 2,
                                                    ladder,
                                                    sensor,
                                                    roc,
                                                    rocchnl);
                // Offset the threshold ADC value to account for the offset
                // in SvxDecode.
                // NOTE: This assumes the offset value in SvxDecode is set to 24
                th = th - 24;
            }

            if ( adc < th )
            {
                //cout<<"strip raw : "<<adc<<" "<<th<<" : "<<layer<<" "<<ladder<<endl;
                continue;
            }
            //end of DCMII threshold cut, by CHC

            // Add the raw hit to the sensor and count it
            if ( sensor >= 0 && (unsigned int)sensor < nBarSensor[layer] )
            {
                barSensor[layer][ladder][sensor]->add_rawhit(tmp_raw);
                int stripmod = SvxAddressObject.getStripModuleID(layer, ladder);
                if ( stripmod == -1 )
                {
                    cerr << "Error " << "SvxReco::" << __FUNCTION__ << " "
                         << " moduleID is -1. this hit is skipped" << endl;
                    continue;
                }

                nraw_strip[stripmod] ++;
            }
        }
    } // iraw


    // Fill SvxEventInfo object
    for ( int i = 0; i < SVXLADDERSLAYER0 * 2 + SVXLADDERSLAYER1 * 2; i++ )
    {
        for ( int j = 0; j < SVXSENSORSLAYER0; j++ )
        {
            d_eventinfo->set_pixelNRawhits(i, j, nraw_pixel[i][j]);
        }
    }
    for ( int i = 0; i < SVXNMODULESTRIP; i++ )
    {
        d_eventinfo->set_stripNRawhits(i, nraw_strip[i]);
    }


    // Perform the clustering using the functions in SvxSensor
    // and add information to SvxEventInfo
    for ( unsigned int ilr = 0; ilr < svxSecNlayers[0]; ilr++ )
    {
        for ( unsigned int ild = 0; ild < nBarLadder[ilr]; ild++ )
        {
            int nclus_sum = 0; // counter for the number of stripixel clusters
            for ( unsigned int isn = 0; isn < nBarSensor[ilr]; isn++ )
            {
                int ncluster = barSensor[ilr][ild][isn]
                               ->findClusters(d_rawhit,
                                              d_cluster,
                                              d_rawhit2cluster,
                                              d_ghit2rawhit,
                                              d_ghit2cluster,
                                              &SvxAddressObject);
                if ( ilr < 2 )
                {
                    int ladid = (ilr == 0) ? ild : ild + 10;
                    d_eventinfo->set_pixelNClusters(ladid, isn, ncluster);
                }
                else
                {
                    nclus_sum += ncluster;
                    if ( isn == nBarSensor[ilr] - 1 )
                    {
                        int id = SvxAddressObject.getStripModuleID(ilr, ild);
                        d_eventinfo->set_stripNClusters(id, nclus_sum);
                    }
                }
                nSvxClusters += ncluster;
            } // isn
        } // ild
    } // ilr


    /// fill SvxClusters in SvxClusterContainer
    /// clear the container before loading clusters.
    d_container->clear();
    /// load clusters
    d_container->load_clusters(d_cluster);

    /// load fake clusters
    vector<SvxCluster*> vcluster;
    for ( int isublayer = 2; isublayer <= SVXMAXSUBLAYER; isublayer++ )
    {
        int ncls = d_container->find_clusters(vcluster, isublayer);
        for ( int icls = 0; icls < ncls; icls++ )
        {
            SvxCluster *cluster = d_fake_cluster->addCluster();
            cluster->Copy(vcluster[icls]);

            /// then, make fake cluster.
            int layer = cluster->get_layer();
            int ladder = cluster->get_ladder();
            int sensor = cluster->get_sensor();
            double xl[3];
            double xg[3];
            xl[0] = cluster->get_xyz_local(0);
            xl[1] = cluster->get_xyz_local(1);
            float zwidth = barSensor[layer][ladder][sensor]->get_zhalfWidth() * 2.;
            xl[2] = cluster->get_xyz_local(2);

            if ( xl[2] > 0 )
                xl[2] -= zwidth;
            else
                xl[2] += zwidth;

            barSensor[layer][ladder][sensor]->position_local2global(xl, xg);

            /// set fake information
            cluster->set_xyz_local(2, xl[2]);
            for ( int i = 0; i < 3; i++ ) cluster->set_xyz_global(i, xg[i]);
        }
    }
    /// load fake clusters
    d_container->load_fake_clusters(d_fake_cluster);

    if ( verbosity > 5 )
    {
        float xbeam, ybeam;
        d_container->get_beam_center(xbeam, ybeam);
        cout << "SvxReco::fillClusterList BeamCenter x-y : "
             << xbeam << ", " << ybeam
             << endl;
    }


    // Ready to exit
    int EXIT_STATUS = ( nSvxClusters == d_cluster->get_nClusters() ) ? 0 : -1;

    if ( verbosity > 0 )
    {
        cout << "SvxReco::fillClusterList-I: Filled " << nSvxClusters
             << " Clusters in the event." << endl
             << "SvxReco::fillClusterList-I: Execution completed." << endl;
    }


    return EXIT_STATUS; // 0 if ok
}

