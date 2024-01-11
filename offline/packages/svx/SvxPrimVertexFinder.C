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
#include <SvxPrimVertexFinder.h>
#include <SvxSegmentList.h>
#include <SvxSegment.h>
#include <SvxTracker.h>
#include <RunHeader.h>
#include <VtxOut.h>
#include <VtxOrdercodes.h>
#include <PHPoint.h>

#include <cmath>


using namespace std;

//! Constructor of the class using hard-coded cuts for vertex finding
    /*!
      \param name the name of the object.
      \param side east or west arm.
    */
SvxPrimVertexFinder::SvxPrimVertexFinder(const string &name, int side) : SubsysReco(name), m_side(side),
    _timer(PHTimeServer::get()->insert_new(name))
{
    d_segment = NULL;
    d_vtxout = NULL;
    d_tracker = new SvxTracker();
    dv_vertex.assign(3, 0.);
    d_xycut  = 0.05;  // 0.5mm
    d_xycut0 = 1.;    // 10mm
    d_zcut   = 0.25;  // 0.25mm
    d_zcut0  = 5.;    // 50mm
    d_use_SvxVtxOut = false;
    d_ppflag = false;
    d_recalflag = false;
}


//! Basic destructor
SvxPrimVertexFinder::~SvxPrimVertexFinder()
{
    delete d_tracker;
    int ntrk = dv_vtxtracklist.size();
    for ( int itrk = 0; itrk < ntrk; itrk++ )
    {
        delete dv_vtxtracklist[itrk];
    }
    dv_vtxtracklist.clear();
    dv_neartracklist.clear();
}

//! Set x-, y-, and z-position of vertex (stored as member variable dv_vertex)
    /*!
      \param vx x-coordinate of vertex.
      \param vy y-coordinate of vertex.
      \param vz z-coordinate of vertex.
    */
void SvxPrimVertexFinder::set_vertex(float vx, float vy, float vz)
{
    dv_vertex[0] = vx;
    dv_vertex[1] = vy;
    dv_vertex[2] = vz;
}

//! Basic Fun4All InitRun function
    /*!
      \param topNode PHCompositeNode
    */
int SvxPrimVertexFinder::InitRun(PHCompositeNode *topNode)
{
    PHNodeIterator iter(topNode);

    // Search SvxVtxOut node.
    // If you can find it, use it. Otherwise, use VtxOut node.
    //  the node is actually gotten in the function GetNodes()
    //  which is called at the top of process event
    PHIODataNode<PHObject> *VtxOutNode = NULL;
    VtxOutNode = (PHIODataNode<PHObject> *)iter.findFirst("PHIODataNode", "SvxVtxOut");
    if ( VtxOutNode )
    {
        d_use_SvxVtxOut = true;
    }
    else
    {
        d_use_SvxVtxOut = false;
    }

    // check magnet current 
    RunHeader* runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
    if(runheader==NULL) {
      cout << PHWHERE<< "Can't find runheader. " << endl;
      return EVENT_OK;
    }
    d_zerofieldflag = (runheader->get_currentCentral()==0);

  d_svxgeometry = findNode::getClass<svxDetectorGeo>(topNode, "svxDetectorGeo");
  if ( d_svxgeometry == NULL) {
    if(verbosity>0) { cout << PHWHERE<< "Can't find svxDetector. " << endl; }
    return EVENT_OK;
  }

  d_tracker->set_DetectorGeo(d_svxgeometry);
    return EVENT_OK;
}

//! Basic Fun4All process_event function
    /*!
      \param topNode PHCompositeNode
    */
int SvxPrimVertexFinder::process_event(PHCompositeNode *topNode)
{
    _timer.get()->restart();

    if ( verbosity > 0 )
    {
        cout << "SvxPrimVertexFinder::process_event-I: Execution started..." << endl;
    }

    // get SvxSegmentList node
    bool status = GetNodes(topNode);
    if ( !status )
    {
        cout << "SvxPrimVertexFinder::process_event-I: Failed to get node..." << endl;
        return EVENT_OK;
    }

    // for the collision vertex reconstruction with reconstructed tracks,
    // more than 2 tracks is necessary.
    int nseg = d_segment->get_nSegments();
    if ( nseg < 2 )
    {
        return EVENT_OK;
    }

    // set current primary vertex point
    // (this is just whatever was in SvxVtxOut or VtxOut node (if there was no SvxVtxOut)

    PHPoint verpoint = d_vtxout->get_Vertex();
    set_vertex(verpoint.getX(), verpoint.getY(), verpoint.getZ());

    // load SvxVerteTrack from d_segment
    int nvtrk = load_vertextrack();
    if ( nvtrk < 2 )
    {
        return EVENT_OK;
    }

    // update the primary vertex.
    // the updated vertex point is stored in dv_vertex.
    find_PrimaryVertex();

    // update VtxOut
    float vtx[3] = {dv_vertex[0], dv_vertex[1], dv_vertex[2]};
    float vtxerr[3] = {0.002, 0.0025, 0.005};

    // check if the primary x-y vertex is good and within 2cm from beam center
    //          the primary z vertex is good and within 10cm from seed vertex (or bbcz)
    float bc_xy  = sqrt(verpoint.getX()*verpoint.getX() + verpoint.getY()*verpoint.getY());
    float vtx_xy = sqrt(dv_vertex[0]*dv_vertex[0]   + dv_vertex[1]*dv_vertex[1]);

    if(fabs(vtx_xy - bc_xy)>2.0 || fabs(dv_vertex[2] - verpoint.getZ())>10. ){
      return EVENT_OK;
    }


    // Values of vtxerr are temporary. unit is cm.
    // These values are almost correct if the number of segments>100.
    // If not, vtxerr are larger than them.
    if (m_side == 1)
    {
        d_vtxout->AddVtx("SVX_PRECISEW", vtx, vtxerr, 13);    // west
    }
    else if (m_side == 2)
    {
        d_vtxout->AddVtx("SVX_PRECISEE", vtx, vtxerr, 14);    // east
    }
    else
    {
        d_vtxout->AddVtx("SVX_PRECISE", vtx, vtxerr, VTX::SVX_PRECISEORDER);// both
    }

    if ( verbosity > 0 )
    {
        cout << "SvxPrimVertexFinder::process_event-I: Execution completed..." << endl;
    }

    _timer.get()->stop();

    return EVENT_OK;
}


int SvxPrimVertexFinder::ResetEvent(PHCompositeNode *topNode)
{
    int ntrk = dv_vtxtracklist.size();
    for ( int itrk = 0; itrk < ntrk; itrk++ )
    {
        delete dv_vtxtracklist[itrk];
    }
    dv_vtxtracklist.clear();
    dv_neartracklist.clear();

    return EVENT_OK;
}

//! Gets nodes containing cluster, segment and vertex information from the node tree.
    /*!
      \param topNode PHCompositeNode
    */
bool SvxPrimVertexFinder::GetNodes(PHCompositeNode *topNode)
{
    d_segment = 0;
    PHTypedNodeIterator<SvxSegmentList> segmentiter(topNode);
    PHIODataNode<SvxSegmentList> *SvxSegmentListNode = segmentiter.find("SvxSegmentList");
    if ( !SvxSegmentListNode )
    {
        cerr << PHWHERE << " ERROR: Can't find SvxSegmentList." << endl;
        return false;
    }
    else
    {
        d_segment = (SvxSegmentList *)SvxSegmentListNode->getData();
    }

    d_vtxout = 0;
    PHTypedNodeIterator<VtxOut> vtxoutiter(topNode);
    PHIODataNode<VtxOut> *VtxOutNode;

    // Search SvxVtxOut node.
    // If you can find it, use it. Otherwise, use VtxOut node.
    //  if it was there or not was determined in InitRun()
    if ( !d_use_SvxVtxOut )
    {
        // get from and fill output in VtxOut.
        VtxOutNode = vtxoutiter.find("VtxOut");
    }
    else
    {
        // get from and fill output in SvxVtxOut.
        VtxOutNode = vtxoutiter.find("SvxVtxOut");
    }
    if ( !VtxOutNode )
    {
        cerr << PHWHERE << " ERROR: Can't find VtxOut." << endl;
        return false;
    }
    else
    {
        d_vtxout = (VtxOut *)VtxOutNode->getData();
    }

    d_cluster = findNode::getClass<SvxClusterList>(topNode, "SvxClusterList");
    if ( d_cluster == NULL) {
        cout << PHWHERE<< "Can't find SvxClusterList. " << endl; 
        return false;
    }


    return true;
}

//! Apply iterative vertex finding algorithm.
//! Iterate 10 times, or until the difference between the new and old vertex is less than 0.001.
//! Use the wide hardcoded xy cuts in the first iteration, and then use the smallest cuts.
void SvxPrimVertexFinder::find_PrimaryVertex()
{
    vector<float> new_vertex(3, 0.);
    int count = 0;

    while ( count < 10 )
    {
        if ( count == 0 )
        {
            // at the first time, use loose cut
            update_PrimaryVertex(new_vertex, d_xycut * 2., d_zcut * 2.);
        }
        else
        {
            // after the second time, use normal cut
            update_PrimaryVertex(new_vertex, d_xycut, d_zcut);
        }

        // if the changing of the vertex is enough small, end iteration.
        // old vertex position is dv_vertex and new vertex position is new_vertex
        float vtx_diff = sqrt( (new_vertex[0] - dv_vertex[0]) * (new_vertex[0] - dv_vertex[0])
                               + (new_vertex[1] - dv_vertex[1]) * (new_vertex[1] - dv_vertex[1]) );
        if ( vtx_diff < 0.001 )
        {
            break;
        }

        // update dv_vtxtracklist
        update_vertextracklist(new_vertex);
        // update dv_vertex
        set_vertex(new_vertex[0], new_vertex[1], new_vertex[2]);

        count ++;
    }

    // update segments
    update_segment(new_vertex);
    // update vertex
    set_vertex(new_vertex[0], new_vertex[1], new_vertex[2]);

    if ( verbosity > 0 )
    {
        cout << "updated vertex = ("
             << dv_vertex[0] << ", " << dv_vertex[1] << ", " << dv_vertex[2] << ")" << endl;
    }
}

//! Takes the list of segments in a given event and approximates those passing certain chisq, dca, and nhits requirements by a straight line of SvxVertexTrack type.
//! Tracks are prioritized by their number of hits and transverse momentum, and the best 25 tracks available are used for vertex determination.
int SvxPrimVertexFinder::load_vertextrack()
{
    //Vectors to store 3- and 4-hit SvxSegments sorted by pT
    vector<SvxSegment *> v_3hitseglist_highpT;
    vector<SvxSegment *> v_4hitseglist_highpT;
    vector<SvxSegment *> v_3hitseglist_lowpT;
    vector<SvxSegment *> v_4hitseglist_lowpT;

    //Iterate over all SvxSegments in the event
    int nseg = d_segment->get_nSegments();
    for ( int iseg = 0; iseg < nseg; iseg++ )
    {
        SvxSegment *t_seg = d_segment->get_segment(iseg);

        //Reject the segment if its DCA is greater than some value AND not running in p+p 
        float dx = t_seg->getClosestApproach(0) - dv_vertex[0];
        float dy = t_seg->getClosestApproach(1) - dv_vertex[1];
        float dz = t_seg->getClosestApproach(2) - dv_vertex[2];
        if ( !d_ppflag && (sqrt(dx * dx + dy * dy) > d_xycut0 || fabs(dz) > d_zcut0) )
        {
            continue;
        }

        //Get track pT and number of hits across layers
        float px = t_seg->get3Momentum(0);
        float py = t_seg->get3Momentum(1);
        float pT = sqrt(px * px + py * py);
	int segscore = t_seg->getSegmentScore();
	if ( segscore == -13 && d_recalflag ){continue;}
        int nhits = 0;
        for ( int ilayer = 0; ilayer < 4; ilayer++)
        {
            nhits += t_seg->getNhits(ilayer);
        }

        //Are we looking at a particular arm of the detector?
        if ( (m_side == 1) || (m_side == 2))
        {
            if ((m_side == 1) && (px < 0)) continue; // choose west side (skip east side)
            if ((m_side == 2) && (px > 0)) continue; // choose east side (skip west side)
        }

        //Reject segment if chisq/ndf > 6
        float chi2 = t_seg->getChiSq();
        int   ndf  = t_seg->getNDF();
        if(!d_zerofieldflag && (ndf<=0 || chi2/ndf > 6)){ // chi2/ndf <6 is used for primary vertex reconstruction in field-ON
          continue;
        }

        //Reject segment if its DCA2D > 2 cm 
        float dca2d_beamcenter = t_seg->getDCA2D();
        if( fabs(dca2d_beamcenter) > 2.0){
          continue;
        }

        //Prioritize tracks by number of hits and pT
        if ( pT > 0.2 )   // if pt>0.2GeV/c
        {
            if ( nhits > 3 )
            {
                v_4hitseglist_highpT.push_back(t_seg);
            }
            else
            {
                v_3hitseglist_highpT.push_back(t_seg);
            }
        }
        else 
        {
            if ( nhits > 3 )
            {
                v_4hitseglist_lowpT.push_back(t_seg);
            }
            else
            {
                v_3hitseglist_lowpT.push_back(t_seg);
            }
        }
    }

    if ( verbosity > 0 )
    {
        cout << "# of 4-cluster and high pT segments : "
             << v_4hitseglist_highpT.size() << endl;
        cout << "# of 3-cluster and high pT segments : "
             << v_3hitseglist_highpT.size() << endl;
        cout << "# of 4-cluster and low pT segments : "
             << v_4hitseglist_lowpT.size() << endl;
        cout << "# of 3-cluster and low pT segments : "
             << v_3hitseglist_lowpT.size() << endl;
    }

    //Get the best available tracks (up to 25) based on the above prioritization
    vector<SvxSegment *> v_goodsegment = v_4hitseglist_highpT;
    if ( v_goodsegment.size() < 25 )
    {
        v_goodsegment.insert(v_goodsegment.end(),
                             v_3hitseglist_highpT.begin(),
                             v_3hitseglist_highpT.end());

        if ( v_goodsegment.size() < 25 )
        {
            v_goodsegment.insert(v_goodsegment.end(),
                                 v_4hitseglist_lowpT.begin(),
                                 v_4hitseglist_lowpT.end());

            if ( v_goodsegment.size() < 25 )
            {
                v_goodsegment.insert(v_goodsegment.end(),
                                     v_3hitseglist_lowpT.begin(),
                                     v_3hitseglist_lowpT.end());
            }
        }
    }

    //Approximate the selected tracks by straight lines at the vertex
    for ( unsigned int i = 0; i < v_goodsegment.size(); i++ )
    {
        SvxVertexTrack *vtrack = make_vertextrack(v_goodsegment[i]);
        dv_vtxtracklist.push_back(vtrack);
    }

    return dv_vtxtracklist.size();
}

//! Finds a new primary vertex, given a new set of cuts for track rejection and the previous vertex stored in the member variable dv_vertex
    /*!
      \param new_vertex vector of floats to store the new vertex that's found
      \param xycut x-y cut to be applied to reject tracks in vertexing algorithm
      /param zcut z cut to be applied to reject tracks in vertexing algorithm
    */
void SvxPrimVertexFinder::update_PrimaryVertex(vector<float> &new_vertex,
        float xycut, float zcut)
{
    //Find seed vertex
    vector<float> seed(3);
    if ( dv_neartracklist.size() == 0 )
    {
        find_center(dv_vtxtracklist, seed);
    }
    else
    {
        seed = dv_vertex;
    }

    //Need at least two tracks to run algorithm
    int ntrk = dv_vtxtracklist.size();
    if ( ntrk < 2 )
    {
        if ( verbosity > 0 )
        {
            cout << "ERROR | # of segment should be larger than 2." << endl;
        }
        new_vertex = seed;
        return;
    }

    dv_neartracklist.clear();

    //Iterate over all SvxVertexTracks and keep only those satisfying the cuts in xy and z
    for ( int itrk = 0; itrk < ntrk; itrk++ )
    {
        float ca_x;
        float ca_y;
        SvxVertexTrack *vtxtrk = dv_vtxtracklist[itrk];
        float sx = seed[0];
        float sy = seed[1];
        calc_ClosestApproach(vtxtrk, sx, sy, ca_x, ca_y);
        // calculate z position of vtrack at x=ca_x and y=ca_y
        float z = calc_z(vtxtrk, ca_x, ca_y);
        float xydiff = sqrt( (ca_x - seed[0]) * (ca_x - seed[0]) + (ca_y - seed[1]) * (ca_y - seed[1]) );
        float zdiff = fabs(z - seed[2]);
        if ( xydiff < xycut && zdiff < zcut )
        {
            dv_neartracklist.push_back(dv_vtxtracklist[itrk]);
        }
    }

    if ( dv_neartracklist.size() < 2 )
    {
        // To find vertex, at least 2 tracks are required.
        new_vertex = seed;
    }
    else
    {
        find_center(dv_neartracklist, new_vertex);
    }
}

//! Use direct minimization of the chi square to compute the vertex point (vx, vy) that minimizes the distance of closest approach of selected SvxVertexTracks
//! The DCA resolution is used as the error in the calculation of the chisq.
//! A full description of the mathematics can be found here <insert link>
    /*!
      \param vtracklist vector of SvxVertexTracks to use in vertex determination
      \param center vector of floats to store the vertex coordinates
    */
void SvxPrimVertexFinder::find_center(vector<SvxVertexTrack *> vtracklist,
                                      vector<float> &center)
{
    float sum_px2 = 0.;
    float sum_py2 = 0.;
    float sum_pxpy = 0.;
    float sum_xpxpy = 0.;
    float sum_ypxpy = 0.;
    float sum_xpy2 = 0.;
    float sum_ypx2 = 0.;

    //loop over the SvxVertexTracks
    //and find the sum of the squares of their postions and momenta
    for ( unsigned int i = 0; i < vtracklist.size(); i++ )
    {
        float x = vtracklist[i]->dv_pos[0];
        float y = vtracklist[i]->dv_pos[1];
        float px = vtracklist[i]->dv_mom[0];
        float py = vtracklist[i]->dv_mom[1];
        float pt = sqrt(px * px + py * py);
        px /= pt;
        py /= pt;

        //Corresponds to inverse of resolution. Inverse of normalized DCA resolution (=1/sigma)
        float weight = vtracklist[i]->d_weight;

        sum_px2 += px * px * weight;
        sum_py2 += py * py * weight;
        sum_pxpy += px * py * weight;
        sum_xpxpy += x * px * py * weight;
        sum_ypxpy += y * px * py * weight;
        sum_xpy2 += x * py * py * weight;
        sum_ypx2 += y * px * px * weight;
    }

    float X = sum_px2;
    float Y = sum_py2;
    float C = sum_pxpy;
    float A = sum_ypxpy - sum_xpy2;
    float B = sum_xpxpy - sum_ypx2;

    center[0] = (A * X + B * C) / (C * C - X * Y); //Vx
    center[1] = (A * C + B * Y) / (C * C - X * Y); //Vy

    float sum_w = 0.;
    float sum_wz = 0.;

    //loop over the SvxVertexTracks
    for ( unsigned int i = 0; i < vtracklist.size(); i++ )
    {
        float ca_x;
        float ca_y;
        SvxVertexTrack *vtrk = vtracklist[i];
        float cx = center[0];
        float cy = center[1];
        calc_ClosestApproach(vtrk, cx, cy, ca_x, ca_y);
        float z = calc_z(vtracklist[i], ca_x, ca_y);
        sum_w += vtracklist[i]->d_weight;
        sum_wz += vtracklist[i]->d_weight * z;
    }

    center[2] = sum_wz / sum_w;
}


void SvxPrimVertexFinder::update_vertextracklist(vector<float> new_vertex)
{
    int ntrk = dv_vtxtracklist.size();
    for ( int itrk = 0; itrk < ntrk; itrk++ )
    {
        SvxSegment *t_seg = dv_vtxtracklist[itrk]->segment;
        bool helicity = t_seg->IsPositive();
        float x0 = t_seg->getClosestApproach(0);
        float y0 = t_seg->getClosestApproach(1);
        float z0 = t_seg->getClosestApproach(2);
        float px0 = t_seg->get3MomentumAtPrimaryVertex(0);
        float py0 = t_seg->get3MomentumAtPrimaryVertex(1);
        float pz0 = t_seg->get3MomentumAtPrimaryVertex(2);
	if (px0 > 0 )
	  { 
	  }
	else 
	  {
	    continue;
	  }
        vector<float> pos_at_ca(3, 0.);   // position of the closest approach
        vector<float> mom_at_ca(3, 0.);   // momentum at the closest approach
        float dca2d;
        d_tracker->calc_InfoAtClosestApproach(helicity,
                                              x0, y0, z0, px0, py0, pz0,
                                              new_vertex,
                                              pos_at_ca, mom_at_ca, dca2d);
        for ( int i = 0; i < 3; i++ ) // i : x, y, z
        {
            dv_vtxtracklist[itrk]->dv_pos[i] = pos_at_ca[i];
            dv_vtxtracklist[itrk]->dv_mom[i] = mom_at_ca[i];
        }
    }
}

//! Recalculate the DCA, DCA3D, chisq and quality of the event segments once a primary vertex has been found
    /*!
      \param new_vertex latest primary vertex that was computed
    */
void SvxPrimVertexFinder::update_segment(vector<float> new_vertex)
{
    for ( int iseg = 0; iseg < d_segment->get_nSegments(); iseg++ )
    {
        SvxSegment *t_seg = d_segment->get_segment(iseg);
        bool helicity = t_seg->IsPositive();
        float x0 = t_seg->getClosestApproach(0);
        float y0 = t_seg->getClosestApproach(1);
        float z0 = t_seg->getClosestApproach(2);
        float px0 = t_seg->get3MomentumAtPrimaryVertex(0);
        float py0 = t_seg->get3MomentumAtPrimaryVertex(1);
        float pz0 = t_seg->get3MomentumAtPrimaryVertex(2);
	int score = t_seg->getSegmentScore();
	// float pt0 = sqrt(px0*px0+py0*py0);
	if ( score == -13 && d_recalflag){continue;}
        vector<float> pos_at_ca(3, 0.);   // position of the closest approach
        vector<float> mom_at_ca(3, 0.);   // momentum at the closest approach
        float dca2d;
        d_tracker->calc_InfoAtClosestApproach(helicity,
          x0, y0, z0, px0, py0, pz0,
          new_vertex,
          pos_at_ca, mom_at_ca, dca2d);
        // update SvxSegment
        t_seg->setClosestApproach(pos_at_ca[0],
          pos_at_ca[1],
          pos_at_ca[2]);
        t_seg->set3MomentumAtPrimaryVertex(mom_at_ca[0],
         mom_at_ca[1],
         mom_at_ca[2]);

        //Update the 2D DCA - D. McGlinchey 7/2/2014
        t_seg->setDCA2D(dca2d);

        //Update the 3D DCA and chisq- T. Koblesky 9/2/2016
        float dca3d = sqrt( (new_vertex[0]-pos_at_ca[0])*(new_vertex[0]-pos_at_ca[0])
            + (new_vertex[1]-pos_at_ca[1])*(new_vertex[1]-pos_at_ca[1])
            + (new_vertex[2]-pos_at_ca[2])*(new_vertex[2]-pos_at_ca[2]) );

         if ( dca2d<0 ) { dca3d *= -1.; }   /// if dca2d is negative, dca3d is also negative.
         t_seg->setDCA(dca3d);
         vector<SvxCluster*> vcluster;
         SvxCluster* tmp = NULL;
         for(int ilayer = 0; ilayer < 4; ilayer++)
         {
            if (t_seg->getNhits(ilayer) > 0)
            {
                int cid = t_seg->getClusterID(ilayer,0);
                if(cid>=0) {
                  tmp = d_cluster->get_Cluster((unsigned int)cid);
                  if(tmp)
                      vcluster.push_back(tmp);
                }
            }
      }
      if(vcluster.size()>2)//only refit if i can find at least 3 clusters
      {
        float pt = sqrt(mom_at_ca[0]*mom_at_ca[0]+mom_at_ca[1]*mom_at_ca[1]);///<use new momentum as guess
        float chisq;
        int ndf;
        float tprob = d_tracker->TrackFit(vcluster, helicity, pt, chisq, ndf);
        t_seg->setQuality(tprob);
        t_seg->setChiSq(chisq);
        t_seg->setNDF(ndf);
        if(t_seg->getSegmentScore()!=-9999 && ndf > 0)
        {
          float linkScore = t_seg->getSegmentScore();
          float segment_quality = 1.0/(chisq/ndf + 2) + linkScore/100.;
          t_seg->setSegmentQuality(segment_quality);
        }
      }

   }
}

//! Construct SvxVertexTrack from SvxSegment
//! Approximate segment by straight line with momentum equal to the momentum of the segment at the vertex
    /*!
      \param segment segment to approximate by an SvxVertexTrack
      \return the SvxVertexTrack constructed for the argument
    */
SvxVertexTrack *SvxPrimVertexFinder::make_vertextrack(SvxSegment *segment)
{
    SvxVertexTrack *vtrack = new SvxVertexTrack(segment);
    for ( int i = 0; i < 3; i++ ) // i : x, y, z
    {
        vtrack->dv_pos[i] = segment->getClosestApproach(i);
        vtrack->dv_mom[i] = segment->get3MomentumAtPrimaryVertex(i);
    }
    float pT2 = vtrack->dv_mom[0] * vtrack->dv_mom[0] + vtrack->dv_mom[1] * vtrack->dv_mom[1];
    vtrack->d_weight = pT2 / (pT2 + 1.);

    return vtrack;
}

//! Calculate the distance of closest approach between an SvxVertexTrack and a point in the xy plane
    /*!
      \param vx x-coordinate of point to which the DCA should be calculated
      \param vy y-coordinate of point to which the DCA should be calculated
      \param ca_x variable to store the x-component of the DCA
      \param ca_y variable to store the y-component of the DCA
    */
void SvxPrimVertexFinder::calc_ClosestApproach(const SvxVertexTrack *vtrack,
        float vx, float vy,
        float &ca_x, float &ca_y)
{
    float x = vtrack->dv_pos[0];
    float y = vtrack->dv_pos[1];
    float px = vtrack->dv_mom[0];
    float py = vtrack->dv_mom[1];
    float L = (px * (vx - x) + py * (vy - y)) / (px * px + py * py);

    ca_x = x + L * px;
    ca_y = y + L * py;
}

//! Calculate the distance along z between an SvxVertexTrack its DCA2D point
    /*!
      \param vtrack SvxVertexTrack
      \param x0 x-coordinate of point to which the z-distance is required
      \param y0 y-coordinate of point to which the z-distance is required
    */
float SvxPrimVertexFinder::calc_z(const SvxVertexTrack *vtrack,
                                  float x0, float y0)
{
    float z0;
    if ( vtrack->dv_mom[0] != 0. )
    {
        float L = (x0 - vtrack->dv_pos[0]) / vtrack->dv_mom[0];
        z0 = vtrack->dv_pos[2] + L * vtrack->dv_mom[2];
    }
    else
    {
        float L = (y0 - vtrack->dv_pos[1]) / vtrack->dv_mom[1];
        z0 = vtrack->dv_pos[2] + L * vtrack->dv_mom[2];
    }

    return z0;
}
