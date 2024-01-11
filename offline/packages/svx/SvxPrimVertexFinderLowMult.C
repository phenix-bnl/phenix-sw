#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHIODataNode.h>
#include <getClass.h>

#include <svxDetectorGeo.hh>
#include <SvxPrimVertexFinderLowMult.h>
#include <SvxSegmentList.h>
#include <SvxSegment.h>
#include <RunHeader.h>
#include <VtxOut.h>
#include <VtxOrdercodes.h>
#include <PHPoint.h>
#include <SvxPrimVertexInfov1.h>

#include "TMath.h"
#include <cmath>


using namespace std;


//------------------------------------------------------------------------------
SvxPrimVertexFinderLowMult::SvxPrimVertexFinderLowMult(const string &name, int side) :
  SubsysReco(name),
  d_segment(NULL),
  d_vtxout(NULL),
  d_vtxinfo(NULL),
  m_side(side),
  _timer(PHTimeServer::get()->insert_new(name))
{

  //Initialize parameters for LOF algorithm
  K = 3;
  MINPTS = 3;
  MAXLOF = 1.2;

  // Intialize vertex
  dv_vertex.assign(3, 0.);
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
SvxPrimVertexFinderLowMult::~SvxPrimVertexFinderLowMult()
{
  for ( unsigned int i = 0; i < dv_goodSegmentList.size(); i++ )
  {
    delete dv_goodSegmentList.at(i);
  }
  dv_goodSegmentList.clear();

  intersectionPoints.clear();

  dv_lines.clear();
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
int SvxPrimVertexFinderLowMult::InitRun(PHCompositeNode *topNode)
{
  cout << "SvxPrimVertexFinderLowMult::InitRun()" << endl;

  // check magnet current
  RunHeader* runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if (runheader == NULL) {
    cout << PHWHERE << "Can't find runheader. " << endl;
    return EVENT_OK;
  }
  m_zeroFieldFlag = (runheader->get_currentCentral() == 0);

  return CreateNodes(topNode);
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
int SvxPrimVertexFinderLowMult::process_event(PHCompositeNode *topNode)
{
  _timer.get()->restart();

  intersectionPoints.clear();

  if ( verbosity > 0 )
  {
    cout << "SvxPrimVertexFinderLowMult::process_event-I: Execution started..." << endl;
  }

  //Get required nodes from the node tree
  // -> SvxSegmentList
  // -> SvxPrimVertexInfo
  // -> VtxOut
  bool status = GetNodes(topNode);
  if ( !status )
  {
    cout << "SvxPrimVertexFinderLowMult::process_event-I: Failed to get nodes..." << endl;
    return EVENT_OK;
  }

  //Require at least two tracks for collision vertex reconstruction
  int nseg = d_segment->get_nSegments();
  if ( nseg < 2 )
  {
    cout << "SvxPrimVertexFinderLowMult::process_event-I: Event has less than 2 segments..." << endl;
    return EVENT_OK;
  }

  //Set current primary vertex point
  //This is just whatever was in the VtxOut node
  PHPoint verpoint = d_vtxout->get_Vertex();
  set_vertex(verpoint.getX(), verpoint.getY(), verpoint.getZ());

  //Select only SvxSegments that pass certain quality cuts, and make sure that you have at least 2
  int nvtrk = load_vertextrack();
  if ( nvtrk < 2 )
  {
    cout << "SvxPrimVertexFinderLowMult::process_event-I: Event has less than 2 good segments..." << endl;
    return EVENT_OK;
  }

  //Create straight line approximations to those good SvxSegments found above
  createStraightSegments();

  //Run LOF algorithm
  findPrimaryVertexLOF();

  // update VtxOut
  fill_vertex(verpoint);

  cout << "SvxPrimVertexFinderLowMult::process_event-I: Execution completed..." << endl;

  _timer.get()->stop();

  return EVENT_OK;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
int SvxPrimVertexFinderLowMult::ResetEvent(PHCompositeNode *topNode)
{
  dv_goodSegmentList.clear();

  intersectionPoints.clear();

  dv_lines.clear();

  return EVENT_OK;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
bool SvxPrimVertexFinderLowMult::CreateNodes(PHCompositeNode *topNode)
{
  cout << "SvxPrimVertexFinderLowMult::CreateNodes(): Creating nodes..." << endl;

  PHNodeIterator iter(topNode);

  // Find SVX node.
  PHCompositeNode *svxNode =
    dynamic_cast<PHCompositeNode *> (iter.findFirst("PHCompositeNode", "SVX"));
  if (! svxNode)
  {
    cerr << PHWHERE << "SVX node missing, doing nothing." << endl;
    return EVENT_OK;
  }

  // Check for the SvxPrimVertexInfo Node and create if it doesn't exist
  PHIODataNode<PHObject>* SvxPrimVertexInfoNode = NULL;
  SvxPrimVertexInfoNode =
    (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", 
                                            "SvxPrimVertexInfoLowMult");
  if (!SvxPrimVertexInfoNode)
  {
    SvxPrimVertexInfo* msvxprimvertexinfo = new SvxPrimVertexInfov1();

    SvxPrimVertexInfoNode =
      new PHIODataNode<PHObject>(msvxprimvertexinfo,
                                 "SvxPrimVertexInfoLowMult",
                                 "PHObject");

    svxNode->addNode(SvxPrimVertexInfoNode);
  }


  cout << "SvxPrimVertexFinderLowMult::CreateNodes(): Finishing execution..." << endl;

  return EVENT_OK;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
bool SvxPrimVertexFinderLowMult::GetNodes(PHCompositeNode *topNode)
{
  //SvxSegmentList
  d_segment = NULL;
  d_segment = findNode::getClass<SvxSegmentList>(topNode, "SvxSegmentList");
  if ( d_segment == NULL )
  {
    cerr << PHWHERE << " ERROR: Can't find SvxSegmentList." << endl;
    return false;
  }

  //VtxOut
  d_vtxout = NULL;
  d_vtxout = findNode::getClass<VtxOut>(topNode, "VtxOut");
  if ( d_vtxout == NULL )
  {
    cerr << PHWHERE << " ERROR: Can't find VtxOut." << endl;
    return false;
  }

  //SvxPrimVertexInfo
  d_vtxinfo = NULL;
  d_vtxinfo = findNode::getClass<SvxPrimVertexInfo>(topNode,
              "SvxPrimVertexInfoLowMult");
  if ( d_vtxinfo == NULL )
  {
    cout << PHWHERE << "Can't find SvxPrimVertexInfoLowMult." << endl;
    return false;
  }

  return true;
}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
void SvxPrimVertexFinderLowMult::createStraightSegments()
{
  for (unsigned int i = 0; i < dv_goodSegmentList.size(); i++)
  {
    //Get good segments stored in dv_goodSegmentList

    segline lin;
    lin.mom[0] = dv_goodSegmentList[i]->get3MomentumAtPrimaryVertex(0);
    lin.mom[1] = dv_goodSegmentList[i]->get3MomentumAtPrimaryVertex(1);
    lin.mom[2] = dv_goodSegmentList[i]->get3MomentumAtPrimaryVertex(2);

    lin.pos[0] = dv_goodSegmentList[i]->getClosestApproach(0);
    lin.pos[1] = dv_goodSegmentList[i]->getClosestApproach(1);
    lin.pos[2] = dv_goodSegmentList[i]->getClosestApproach(2);

    dv_lines.push_back(lin);
  }
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
int SvxPrimVertexFinderLowMult::load_vertextrack()
{
  // Select good SvxSegments from those available in this event
  for (int iseg = 0; iseg < d_segment->get_nSegments(); iseg++ )
  {
    SvxSegment *t_seg = d_segment->get_segment(iseg);

    // segment selection
    float px = t_seg->get3Momentum(0);
    float py = t_seg->get3Momentum(1);
    float pT = sqrt(px * px + py * py);
    int hitsB0 = (int) t_seg->getNhits(0);
    int hitsB1 = (int) t_seg->getNhits(1);
    int hitsB2 = (int) t_seg->getNhits(2);
    int hitsB3 = (int) t_seg->getNhits(3);

    // choose side
    if ( (m_side == 1) || (m_side == 2))
    {
      if ((m_side == 1) && (px < 0)) continue; // choose west side only
      if ((m_side == 2) && (px > 0)) continue; // choose east side only
    }

    // chi2 cut
    // chi2/ndf < 6 is used for primary vertex reconstruction in field-ON
    // No chi2/ndf cut for zero-field (no chi2 calculation)
    float chi2 = t_seg->getChiSq();
    int   ndf  = t_seg->getNDF();
    if (!m_zeroFieldFlag && (ndf <= 0 || chi2 / ndf > 6))
      continue;

    // dca2d cut.
    // track must have DCA less than 2cm relative to the beam center
    //float dca2d_beamcenter = t_seg->getDCA2D();
    //if ( fabs(dca2d_beamcenter) > 2.0)
    //  continue;

    // pT cut
    // Require pT > 0.2 cut to require straightline tracks in VTX (arbitrary)
    if ( pT <= 0.2)
      continue;

    //Require 3- and 4-hit tracks
    if(!(hitsB0 > 0 && hitsB1 > 0 && hitsB2 > 0 && hitsB3 > 0) && !(hitsB0 > 0 && hitsB1 > 0 && (hitsB2 > 0 || hitsB3 > 0)))
    {
      continue;
    }

    dv_goodSegmentList.push_back(t_seg);

  } // iseg

  if ( verbosity > 0 )
  {
    float rat = (float)dv_goodSegmentList.size();
    rat /= (float) d_segment->get_nSegments();

    cout << PHWHERE << " Selecting good SvxSegments. "
         << " Kept " << dv_goodSegmentList.size()
         << " out of " << d_segment->get_nSegments()
         << " (" << rat << ")"
         << endl;
  }

  return dv_goodSegmentList.size();
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
void SvxPrimVertexFinderLowMult::fill_vertex(PHPoint verpoint)
{
  float vtx[3] = {dv_vertex[0], dv_vertex[1], dv_vertex[2]};
  float vtxerr[3] = {0.002, 0.0025, 0.005};

  // check if the primary x-y vertex is good and within 2cm from beam center
  //          the primary z vertex is good and within 10cm from seed vertex (or bbcz)
  float bc_xy  = sqrt(verpoint.getX() * verpoint.getX() + verpoint.getY() * verpoint.getY());
  float vtx_xy = sqrt(dv_vertex[0] * dv_vertex[0]   + dv_vertex[1] * dv_vertex[1]);

  if (fabs(vtx_xy - bc_xy) > 2.0 || fabs(dv_vertex[2] - verpoint.getZ()) > 10. ) {
    return;
  }

  // Values of vtxerr are temporary. unit is cm.
  // These values are almost correct if the number of segments>100.
  // If not, vtxerr are larger than them.
  if (m_side == 1)
  {
    d_vtxout->AddVtx("SVX_PRECISEW_LOF", vtx, vtxerr, 13);    // west
  }
  else if (m_side == 2)
  {
    d_vtxout->AddVtx("SVX_PRECISEE_LOF", vtx, vtxerr, 14);    // east
  }
  else
  {
    d_vtxout->AddVtx("SVX_PRECISE_LOF", vtx, vtxerr, 50);// both
  }

  // Fill SvxPrimVertexInfo object
  d_vtxinfo->Reset();

  d_vtxinfo->set_vertex(dv_vertex[0], dv_vertex[1], dv_vertex[2]);
  d_vtxinfo->set_vertexResolution(vtxerr[0], vtxerr[1], vtxerr[2]);

  for (unsigned int i = 0; i < dv_goodSegmentList.size(); i++)
    d_vtxinfo->add_segmentID(dv_goodSegmentList.at(i)->getSegmentID());
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
void SvxPrimVertexFinderLowMult::set_vertex(float vx, float vy, float vz)
{
  dv_vertex[0] = vx;
  dv_vertex[1] = vy;
  dv_vertex[2] = vz;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
void SvxPrimVertexFinderLowMult::findNearestNeighbors()
{
  std::vector<intpoint> neighbors;

  //Iterate over points
  //Use brute force to compute the distance to every other point
  for (unsigned int i = 0; i < intersectionPoints.size(); i++)
  {
    intpoint p1 = intersectionPoints[i];

    //Vector with distance from current point to every other point in the array
    std::vector<float> distances;
    //Vector with the indices of the points arranged in increasing distance to the current point
    std::vector<int> indices;

    for (unsigned int j = 0; j < intersectionPoints.size(); j++)
    {
      if (i == j) continue;

      intpoint p2 = intersectionPoints[j];

      float dist = computeDistance(p1, p2);

      //Insert first two elements in order
      if (distances.size() == 0)
      {
        distances.push_back(dist);
        indices.push_back(j);
      }
      else if (distances.size() == 1)
      {
        if (dist > distances[0])
        {
          distances.push_back(dist);
          indices.push_back(j);
        }
        else
        {
          distances.insert(distances.begin(), dist);
          indices.insert(indices.begin(), j);
        }
      }

      //Insert distance into array such that it is always ordered
      if (dist < distances[0])
      {
        distances.insert(distances.begin(), dist);
        indices.insert(indices.begin(), j);
      }
      else if (dist > distances[distances.size() - 1])
      {
        distances.insert(distances.begin() + (distances.size()), dist);
        indices.insert(indices.begin() + (indices.size()), j);
      }
      else
      {
        for (unsigned int i = 0; i < distances.size() - 1; i++)
        {
          if (dist > distances[i] && dist < distances[i + 1])
          {
            distances.insert(distances.begin() + (i + 1), dist);
            indices.insert(indices.begin() + (i + 1), j);
          }
        }
      }
    }

    //Having computed all distances, take the max distance to the k-nearest neighbors as the k-distance for the point
    intersectionPoints[i].kdistance = distances[K - 1];

    //Store the indices of the closest MINPTS neighbors in the intersectionPoints vector
    for (int k = 0; k < MINPTS; k++)
    {
      intersectionPoints[i].minPtsNeighbors.push_back(indices[k]);
    }
  }
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
float SvxPrimVertexFinderLowMult::computeDistance(intpoint p1, intpoint p2)
{
  return TMath::Sqrt(pow(p1.x - p2.x, 2) + pow(p1.y - p2.y, 2));
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
void SvxPrimVertexFinderLowMult::computeReachDensity()
{
  for (unsigned int i = 0; i < intersectionPoints.size(); i++)
  {
    intpoint p1 = intersectionPoints[i];
    std::vector<int> neighbors = p1.minPtsNeighbors;
    float summedReachDist = 0;

    //Iterate over the MINPTS neighbors to ith point
    for (unsigned int j = 0; j < neighbors.size(); j++)
    {
      intpoint p2 = intersectionPoints[j];

      float kDist      = p2.kdistance;
      float euclidDist = computeDistance(p1, p2);
      float reachDist  = std::max(kDist, euclidDist);

      summedReachDist += reachDist;
    }

    intersectionPoints[i].lrd = (float) neighbors.size() / summedReachDist;
  }
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
void SvxPrimVertexFinderLowMult::computeLOF()
{
  for (unsigned int i = 0; i < intersectionPoints.size(); i++)
  {
    intpoint p1 = intersectionPoints[i];
    std::vector<int> neighbors = p1.minPtsNeighbors;
    float lrd1 = p1.lrd;
    float summedLRDRatio = 0;

    for (unsigned int j = 0; j < neighbors.size(); j++)
    {
      intpoint p2 = intersectionPoints[j];
      float lrd2 = p2.lrd;
      summedLRDRatio += (lrd2 / lrd1);
    }

    intersectionPoints[i].lof = (float) summedLRDRatio / neighbors.size();
  }
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
float SvxPrimVertexFinderLowMult::findDCAz(float dcax, float dcay, float dcaz, float px, float py, float pz, float x0, float y0)
{
  //Initial point for track
  float a[3];
  a[0] = dcax;
  a[1] = dcay;
  a[2] = dcaz;

  //Direction vector for track
  float u_vec[3];
  u_vec[0] = px;
  u_vec[1] = py;
  u_vec[2] = pz;

  //Initial point for 'beam axis'
  float b[3];
  b[0] = x0;
  b[1] = y0;
  b[2] = 0;

  //Direction vector for horizontal line defined by (x,y) precise vertex
  float v_vec[3];
  v_vec[0] = 0;
  v_vec[1] = 0;
  v_vec[2] = 1;

  //Auxiliary variables
  float u_dot_v = u_vec[0] * v_vec[0] + u_vec[1] * v_vec[1] + u_vec[2] * v_vec[2];
  float norm_u_sq = u_vec[0] * u_vec[0] + u_vec[1] * u_vec[1] + u_vec[2] * u_vec[2];
  float norm_v_sq = v_vec[0] * v_vec[0] + v_vec[1] * v_vec[1] + v_vec[2] * v_vec[2];

  float b_m_a[3];
  b_m_a[0] = b[0] - a[0];
  b_m_a[1] = b[1] - a[1];
  b_m_a[2] = b[2] - a[2];

  //Find the parameter that defines the point of closest approach on the track to the "beam axis"
  float t_Mod = ((u_dot_v * (b_m_a[0] * v_vec[0] + b_m_a[1] * v_vec[1] + b_m_a[2] * v_vec[2])) - (norm_v_sq * (b_m_a[0] * u_vec[0] + b_m_a[1] * u_vec[1] + b_m_a[2] * u_vec[2]))) / (u_dot_v * u_dot_v - norm_v_sq * norm_u_sq);

  //Take the z component of this distance of closest approach
  float vtxz = a[2] + t_Mod * u_vec[2];

  return vtxz;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
void SvxPrimVertexFinderLowMult::find2DIntersection(float dcax1, float dcay1, float dcax2, float dcay2, float px1, float py1, float px2, float py2, float &x, float &y)
{
  //First, check that lines are not parallel (i.e., determinant = 0)
  if (py1 * px2 - px1 * py2 == 0)
  {
    x = -999;
    y = -999;

    return;
  }

  float aux = 1.0 / (py1 * px2 - px1 * py2);

  float t1 = aux * (-1 * (dcax2 - dcax1) * py2 + (dcay2 - dcay1) * px2);
  //float t2 = aux * (-1 * (dcax2 - dcax1) * py1 + (dcay2 - dcay1) * px1);

  x = dcax1 + t1 * px1;
  y = dcay1 + t1 * py1;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
void SvxPrimVertexFinderLowMult::findPrimaryVertexLOF()
{
  std::cout << "SvxPrimVertexFinderLowMult::findPrimaryVertexLOF()" << std::endl;
  std::cout << "---> Found " << dv_lines.size() << " good tracks" << std::endl;

  float vertX = -9999;
  float vertY = -9999;
  float vertZ = -9999;

  //STEP 1: FIND VERTEX POSITION IN X-Y PLANE
  // Take all straight-line approximations to good SvxSegments
  // Find all points where these lines intersect
  // Apply LOF algorithm to these points, and reject those with LOF > MAXLOF
  // Find centroid and take this to be the precise vertex in the transverse plane

  //Iterate over straight line tracks to find all points of intersection in x-y
  for (unsigned int i = 0; i < dv_lines.size(); i++)
  {
    //First line
    segline t1     = dv_lines[i];
    float dcax1 = t1.pos[0];
    float dcay1 = t1.pos[1];
    float px1   = t1.mom[0];
    float py1   = t1.mom[1];

    for (unsigned int j = 0; j < i; j++)
    {
      //Second line
      segline t2     = dv_lines[j];
      float dcax2 = t2.pos[0];
      float dcay2 = t2.pos[1];
      float px2   = t2.mom[0];
      float py2   = t2.mom[1];

      float xInt;
      float yInt;
      find2DIntersection(dcax1, dcay1, dcax2, dcay2, px1, py1, px2, py2, xInt, yInt);

      //Define intersection point
      intpoint p;
      p.x = xInt;
      p.y = yInt;
      intersectionPoints.push_back(p);
    }
  }

  if (verbosity > 0)
  {
    std::cout << "SvxPrimVertexFinderLowMult::findPrimaryVertexLOF(): Found " << intersectionPoints.size() << " points of intersection between tracks" << std::endl;
  }

  //In the case of only two tracks, take the primary vertex to be the intersection point
  //The z component of the vertex is found as the average z-component of the point of closest approach of each trach to the horizontal line defined by (x_precise, y_precise)
  if (intersectionPoints.size() == 1)
  {
    if ( verbosity > 0 )
    {
      std::cout << "SvxPrimVertexFinderLowMult::findPrimaryVertexLOF(): Only one point of intersection found" << std::endl;
    }

    vertX = intersectionPoints[0].x;
    vertY = intersectionPoints[0].y;

    float dcaZ1 = findDCAz(dv_lines[0].pos[0], dv_lines[0].pos[1], dv_lines[0].pos[2], dv_lines[0].mom[0], dv_lines[0].mom[1], dv_lines[0].mom[2], vertX, vertY);
    float dcaZ2 = findDCAz(dv_lines[1].pos[0], dv_lines[1].pos[1], dv_lines[1].pos[2], dv_lines[1].mom[0], dv_lines[1].mom[1], dv_lines[1].mom[2], vertX, vertY);

    vertZ = (dcaZ1 + dcaZ2) / 2.0;

    //Set the primary vertex
    set_vertex(vertX, vertY, vertZ);

    if ( verbosity > 0 )
    {
      std::cout << "SvxPrimVertexFinderLowMult::findPrimaryVertexLOF(): Vertex set to " << vertX << ", " << vertY << ", " << vertZ << std::endl;
    }

    return;
  }
  else if (intersectionPoints.size() == 2 || intersectionPoints.size() == 3)
  {
    if ( verbosity > 0 )
    {
      std::cout << "SvxPrimVertexFinderLowMult::findPrimaryVertexLOF(): Only two or three points of intersection found" << std::endl;
    }

    vertX = 0;
    vertY = 0;
    vertZ = 0;

    for (unsigned int i = 0; i < intersectionPoints.size(); i++)
    {
      vertX += intersectionPoints[i].x;
      vertY += intersectionPoints[i].y;
    }

    vertX = (float) vertX / intersectionPoints.size();
    vertY = (float) vertY / intersectionPoints.size();

    for (unsigned int i = 0; i < dv_lines.size(); i++)
    {
      vertZ += findDCAz(dv_lines[i].pos[0], dv_lines[i].pos[1], dv_lines[i].pos[2], dv_lines[i].mom[0], dv_lines[i].mom[1], dv_lines[i].mom[2], vertX, vertY);
    }

    vertZ = (float) vertZ / dv_lines.size();

    //Set the primary vertex
    set_vertex(vertX, vertY, vertZ);

    if ( verbosity > 0 )
    {
      std::cout << "SvxPrimVertexFinderLowMult::findPrimaryVertexLOF(): Vertex set to " << vertX << ", " << vertY << ", " << vertZ << std::endl;
    }

    return;
  }

  //Apply LOF algorithm if more than 3 points of intersection were found
  findNearestNeighbors();
  computeReachDensity();
  computeLOF();

  if ( verbosity > 0 )
  {
    std::cout << "SvxPrimVertexFinderLowMult::findPrimaryVertexLOF(): Applied LOF algorithm to " << intersectionPoints.size() << " points" << std::endl;
  }

  //Having found the intersection points and their LOF scores, find centroid rejecting points with LOF > MAXLOF
  float sumX      = 0;
  float sumY      = 0;
  int nGoodPoints = 0;

  for (unsigned int i = 0; i < intersectionPoints.size(); i++)
  {
    if (intersectionPoints[i].lof > MAXLOF) continue;

    sumX += intersectionPoints[i].x;
    sumY += intersectionPoints[i].y;
    nGoodPoints++;
  }

  //Set x,y vertex coordinates
  vertX = (float) sumX / nGoodPoints;
  vertY = (float) sumY / nGoodPoints;

  //Clear the points in the intersection container before computing z-vertex location
  intersectionPoints.clear();

  //STEP 2: FIND VERTEX POSITION IN THE Z DIRECTION
  // Having found (x_precise, y_precise), find the point of closest approach of all tracks to (x_precise, y_precise)
  // Then, use LOF algorithm in 1 dimension to discard outliers along z
  // Take z-precise to be the average of the remaining points

  //Iterate over straight line tracks
  for (unsigned int i = 0; i < dv_lines.size(); i++)
  {
    segline t = dv_lines[i];
    float dcax = t.pos[0];
    float dcay = t.pos[1];
    float dcaz = t.pos[2];

    float px   = t.mom[0];
    float py   = t.mom[1];
    float pz   = t.mom[2];

    intpoint p;
    p.x = findDCAz(dcax, dcay, dcaz, px, py, pz, dv_vertex[0], dv_vertex[1]);
    p.y  = 0;

    intersectionPoints.push_back(p);
  }

  //Apply LOF algorithm
  findNearestNeighbors();
  computeReachDensity();
  computeLOF();

  float sumZ = 0;
  nGoodPoints = 0;
  for (unsigned int i = 0; i < intersectionPoints.size(); i++)
  {
    if (intersectionPoints[i].lof > MAXLOF) continue;

    sumZ += intersectionPoints[i].x;
    nGoodPoints++;
  }

  vertZ = (float) sumZ / nGoodPoints;

  //Set vertex
  set_vertex(vertX, vertY, vertZ);

  if ( verbosity > 0 )
  {
    std::cout << "SvxPrimVertexFinderLowMult::findPrimaryVertexLOF(): Vertex set to " << vertX << ", " << vertY << ", " << vertZ << std::endl;
  }
}
//------------------------------------------------------------------------------

