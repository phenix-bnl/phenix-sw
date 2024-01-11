#ifndef __SvxPrimVertexFinderLowMult_H__
#define __SvxPrimVertexFinderLowMult_H__

#include <phool.h>
#include <SubsysReco.h>
#include <PHTimeServer.h>
#include <vector>

class PHCompositeNode;
class VtxOut;
class SvxSegmentList;
class SvxSegment;
class SvxPrimVertexInfo;
class PHPoint;

/**
 * @brief A SubsysReco module to reconstruct the primary vertex
 *        using SvxSegments (standalone SVX tracks).
 *
 * Calculate the primary vertex using tracks made from SVX clusters using
 * the local outlier factor algorithm as described in http://www.dbs.ifi.lmu.de/Publikationen/Papers/LOF.pdf
 * Result is stored in VtxOut object as "SVX_PRECISE" vertex.
 *
 * @author Darren McGlinchey
 * @author Javier Orjuela-Koop
 * @email darren.mcglinchey@colorado.edu
 * @date 10 May 2016
 */

//Structure to represent a 2D point object for the LOF algorithm
struct intpoint
{
  float x;
  float y;

  float kdistance;                    //k-Distance of point
  float lrd;                          //Local reachability distance of point
  float lof;                          //Local outlier factor score of point
  std::vector<int> minPtsNeighbors;   //Indices of closest neighbors to a given point
};

//Structure to represent the straight line approximation to an SvxSegment
struct segline
{
  float mom[3];    //SvxSegment momentum at primary vertex
  float pos[3];    //Point of closest approach of the SvxSegment
};

class SvxPrimVertexFinderLowMult : public SubsysReco
{

public :

  /*
   * The 'K' in the k-nearest neighbors algorithm
   */
  int K;

  /*
   * The number of points used in the calculation of the reachability density
   */
  int MINPTS;

  /*
   * Threshold for cutting on LOF scores to determine outliers
   */
  int MAXLOF;

  /**
   * Basic constructor.
   */
  SvxPrimVertexFinderLowMult(const std::string &name = "SvxPrimVertexFinderLowMult", int side = 0);


  /**
   * Basic destructor. Delete SvxSegment objects.
   */
  virtual ~SvxPrimVertexFinderLowMult();


  /**
   * Check if zero field and set flag if necessary.
   * Create required nodes on tree.
   */
  int InitRun(PHCompositeNode *topNode);


  /**
   * Do all the work for each event.
   */
  int process_event(PHCompositeNode *topNode);


  /**
   * Clear vectors with SvxVertexTrack objects.
   */
  int ResetEvent(PHCompositeNode *topNode);


  /**
   * No functionality.
   */
  int End(PHCompositeNode *topNode) { return 0; }


private :

  /**
   * Create Nodes (SvxPrimVertexInfo)
   */
  bool CreateNodes(PHCompositeNode *topNode);

  /**
   * Get input information from data nodes.
   */
  bool GetNodes(PHCompositeNode *topNode);


  /**
   * Select good SvxSegment and fill in SvxVertexTracks.
   *
   * Make a cut on the SvxSegment:
   *   DCA2D relative to the beam center
   *   chi2/ndf in field-ON (ignored for ZF running)
   *   pT of the track
   * See the function for hard coded cuts
   */
  int  load_vertextrack();


  /**
   * Fill the VtxOut object with found vertex
   *
   * Fill the VtxOut object with the vertex found in find_primaryVertex().
   * Also fill SvxPrimVertexInfo object with required information.
   *
   * \param[in] verpoint PHPoint object containg SVX beamcenter
   */
  void fill_vertex(PHPoint verpoint);


  /**
   * Set x-, y-, and z-position of vertex (stored as member variable dv_vertex).
   *
   * \param[in] vx X vertex position [cm]
   * \param[in] vy Y vertex position [cm]
   * \param[in] vz Z vertex position [cm]
   */
  void set_vertex(float vx, float vy, float vz);


  /*
   * Create straight line approximations to SvxSegments using their
   * point of closest approach to and momentum at the primary vertex
   */
  void createStraightSegments();


  /**
   * Find the k-nearest neighbors to each point ,where the points
   * are defined by the intersection of straight line approximations
   * to SvxSegments.
   */
  void findNearestNeighbors();


  /**
   * Compute the Euclidean distance between two points
   *
   * \param[in] p1 first point
   * \param[in] p2 second point
   */
  float computeDistance(intpoint p1, intpoint p2);


  /*
   * Find the point of closest approach between a line in 3D space and line parallel to the z axis
   *
   * \param[in] dcax x-coordinate of point parametrizing the line
   * \param[in] dcay y-coordinate of point parametrizing the line
   * \param[in] dcax z-coordinate of point parametrizing the line
   * \param[in] px x-coordinate of track momentum
   * \param[in] py y-coordinate of track momentum
   * \param[in] pz z-coordinate of track momentum
   * \param[in] x0 x-coordinate of point parametrizing the horizontal line parallel to z
   * \param[in] y0 y-coordinate of point parametrizing the horizontal line parallel to z
   */
  float findDCAz(float dcax, float dcay, float dcaz, float px, float py, float pz, float x0, float y0);


  /*
   * Compute the reachability density for every point by averaging the reach
   * distance over MINPTS neighbors
   */
  void computeReachDensity();


  /*
   * Compute primary vertex using LOF algorithm
   */
  void findPrimaryVertexLOF();

  /*
   * Find the intersection points of two lines parametrized by a point a momentum
   *
   * \param[in] dcax1 x-component of point parametrizing the first line
   * \param[in] dcay1 y-component of point parametrizing the first line
   * \param[in] dcax2 x-component of point parametrizing the second line
   * \param[in] dcay2 y-component of point parametrizing the second line
   * \param[in] x x-coordinate of intersection point to be determined
   * \param[in] y y-coordinate of intersection point to be determined
   */
  void find2DIntersection(float dcax1, float dcay1, float dcax2, float dcay2, float px1, float py1, float px2, float py2, float &x, float &y);

  /*
   * Compute and assign a local outlier factor (LOF) score to every point
   */
  void computeLOF();

  SvxSegmentList *d_segment; ///< Input data node providing SVX track segments.

  VtxOut *d_vtxout; ///< Data node for storing vertex position.
  SvxPrimVertexInfo *d_vtxinfo; ///< Data node for storing vertex information.

  std::vector<intpoint> intersectionPoints;   //Container with intersection points for LOF algorithm
  std::vector<SvxSegment*> dv_goodSegmentList; ///< Vector of SvxSegments passing quality criteria
  std::vector<segline> dv_lines; //Straight line approximations to good SvxSegments
  std::vector<float> dv_vertex; //< Vertex position in three dimensions (x,y,z).

  int   m_side; ///< Detector arm used 0:both, 1:west, 2:east
  bool  m_zeroFieldFlag; ///< Flag for zero field?

  PHTimeServer::timer _timer; ///< Timer.
};

#endif
