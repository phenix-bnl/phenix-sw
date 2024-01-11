#ifndef __SVXPRIMVERTEXFINDER_H__
#define __SVXPRIMVERTEXFINDER_H__

#include <phool.h>
#include <SubsysReco.h>
#include <PHTimeServer.h>
#include <vector>

class PHCompositeNode;
class VtxOut;
class SvxSegmentList;
class SvxSegment;
class SvxTracker;
class SvxClusterList;
class SvxComponentGeom;
class svxDetectorGeo;
/**
 * @brief Definition of struct SvxVertexTrack.
 *
 * This structure is for vertex reconstruction using SvxSegments.
 * VertexTrack is a track which approximates SvxSegment as a straight line.
 *
 * @todo Move to separate header SvxVertexTrack.h
 */
struct SvxVertexTrack
{
  SvxVertexTrack(SvxSegment *s=0) : segment(s) {}
  SvxSegment *segment;  ///< This SvxVertexTrack object is made from the segment. Pointer of SvxSegment which is approximated.
  float dv_pos[3];      ///< The closest approach point of segment. Corresponds to SvxSegment::diff_from_vertex. Which was the (position at the closest approach to the primary vertex) - (the primary vertex).
  float dv_mom[3];      ///< Corresponds to SvxSegment::vertex_mom3 (3 mom at primary vertex).
  float d_weight;       ///< Corresponds to inverse of resolution. Inverse of normalized DCA resolution (=1/sigma, sigma in (2-iii)).
};


/**
 * @brief A SubsysReco module to reconstruct the primary vertex using reconstructed tracks in the SVX subdetector.
 *
 * @todo Add more detailed class description.
 *
 * @author Ryohji Akimoto
 * @email R.AKIMOTO@CNS.S.U-TOKYO.AC.JP
 * @date August 2011
 */
class SvxPrimVertexFinder : public SubsysReco
{

public :

  /**
   * Basic constructor. Set hard-coded values for some member variables.
   *
   * @todo Explain hard-coded numbers in constructor. cut0 are cuts made on the first iteration. cut (w/o 0) are
   *       made on subsquent iterations. I think these form a search window?
   */
  SvxPrimVertexFinder(const std::string &name = "SVXPRIMVERTEXFINDER",int side = 0);


  /**
   * Basic destructor. Delete SvxVertexTrack objects.
   */
  virtual ~SvxPrimVertexFinder();


  /**
   * No functionality.
   */
  int Init(PHCompositeNode *topNode)    { return 0; }


  /**
   * Search SvxVtxOut node. If you can find it, use it. Otherwise, use VtxOut node.
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
  int End(PHCompositeNode *topNode)     { return 0; }


  /**
   * Set cut in x-y direction.
   */
  void set_xycut(float cut, float cut0) { d_xycut=cut; d_xycut0=cut0; }


  /**
   * Set cut in z direction.
   */
  void set_zcut (float cut, float cut0) { d_zcut =cut; d_zcut0 =cut0; }


  /**
   * Set flag for proton-proton collision data (TRUE = is pp data).
   */
  void set_ppflag(bool flag)            { d_ppflag=flag;              }

  /**
   * Set flag for indicating if electron recalibrator was run (TRUE = is pp data).
   */
  void set_recalflag(bool flag)            { d_recalflag=flag;              }


private :

  /**
   * Get input information from data nodes.
   */
  bool GetNodes(PHCompositeNode *topNode);


  /**
   * Set x-, y-, and z-position of vertex (stored as member variable dv_vertex).
   */
  void set_vertex(float vx, float vy, float vz);


  /**
   * Select good SvxSegment and fill in SvxVertexTracks made from the selected SvxSegments.
   *
   * Loads the best 25 tracks, starting with tracks that fulfill the following category
   * of cuts. If it doesn't find 25 it starts taking tracks from the next category:
   *
   * -# number of associated hits >= 4 && pT>0.2
   * -# number of associated hits == 3 && pT>0.2
   * -# number of associated hits == 3 && pT<0.2
   * -# number of associated hits >= 4 && pT<0.2
   *
   * @todo Explain hard-coded cut on pT>0.2.
   */
  int  load_vertextrack();


  /**
   * Calculation of new primary vertex is iteratively done by following 5 steps.
   *
   * -# Roughly calculate new primary vertex : done at update_PrimaryVertex()
   *   For the first iteration, this calculation is done with the same method as (iii below)
   *   with all tracks selected at (1).
   *   From second iteration, use the primary vertex calculated at the previous calculation.
   * -# Select SvxVertexTracks which pass closer to the primary vertex than cut
   *   parameters below: done at update_PrimaryVertex()
   *   Selection is done from tracks selected at (1).
   *   Cut parameters:
   *      For the first iteration, cut in XY : 1mm & in Z : 5mm
   *      From the second iteration, cut in XY : 0.5mm & in Z : 2.5mm
   * -# Calculate vertex in XY-plane (Vx, Vy) : done in find_center()
   *   Calculation is done with least chi-square method.
   *   chi2 = SUM (distance between the line and (Vx, Vy))^2 / sigma^2
   *      where sigma = normalized DCA resolution
   *      and sigma also = (multiple scattering term) (+) (position resolution term) =
   *      sqrt((1/pT^2) +1 )
   *   chi2 itself is not calculated but (Vx, Vy) is calculated from dchi^2/dVx=0 &
   *   dchi^2/dVy=0.
   * -# Calculate vertex position in Z-direction Vz : done at find_center()
   *    After calculation of (Vx, Vy), calculate closest approach to (Vx, Vy) of
   *    each SvxVertexTrack in XY-plane, and calculate corresponding Z position.
   *    Then, calculate Vz as weighted mean of the corresponding Z positions.
   *    weight : 1/sigma (sigma in (3))
   * -# update SvxVertexTrack : done at update_vertextracklist()
   *   The update is done with new primary vertex calculated at (3).
   * -# Go back to (1) unless the changing of the primary vertex point < 10μm.
   * -# is done before (5) since SvxVertexTracks are not used after the iteration.
   *
   * Maximum of the iteration : 10
   *
   * @todo Write documentation.
   * @todo in the line " update_PrimaryVertex(new_vertex, d_xycut*2., d_zcut*2.);"
   *       I think the d_xycut*2., d_zcut*2. is supposed to be d_xycut0, d_zcut0 ?
   */
  void find_PrimaryVertex();


  /**
   * Update dv_vtxtracklist with new vertex point of new_vertex.
   */
  void update_vertextracklist(std::vector<float> new_vertex);


  /**
   * Update the primary vertex point.
   * Does a rough  calculation of the vtx point as part of the find_PrimaryVertex() iteration loop
   * new_vertex : updated vertex point is filled in. (currennt vertex point is dv_vertex)
   * xycut, zcut : they are used for the selection whether track pass enough close to
   *               the seed. the primary vertex update is done only the selected tracks.
   */
  void update_PrimaryVertex(std::vector<float> &new_vertex, float xycut, float zcut);

  /**
   * Update diff_from_vertex and vertex_mom3 of SvxSegments.
   */
  void update_segment(std::vector<float> new_vertex);

  /**
   * Update primary vertex with the least chi-square method.
   *
   * chi^2 is sum of the distance between tracks and the primary vertex (vx, vy).
   * and it can be represented as
   *
   * chi^2 = Sum( ( (1-(py/pt)^2)*(vx-x)^2 + (1-(px/pt)^2)*(vy-y)^2 - 2*(px/pt)*(py/pt)*(vx-x)*(vy-y) )/sigma^2 ),
   *
   * where (x,y) and (px,py) is position and momentum of SvxVerteTrack, respectively.
   * In the following calculation, I used SvxVertexTrack::d_weight as 1/sigma^2.
   * Therefore, chi^2 should be minimum when dchi^2/dvx=0 and dchi^2/dvy=0.
   */
  void find_center(std::vector<SvxVertexTrack*> vtracklist, std::vector<float> &center);


  /**
   * This function converts an SvxSegment object to an SvxVertexTrack object.
   */
  SvxVertexTrack* make_vertextrack(SvxSegment *segment);


  /**
   * This function is to calculate the closest approach of vtrack to (vx, vy)
   * The closest approach (ca_x, ca_y) is output.
   *
   * When the closest approach (ca_x, ca_y) is represented as
   * (ca_x, ca_y) = (x, y) + L*(px, py), and then
   * (px, py) * ((ca_x, ca_y)-(vx, vy)) = 0,
   * where x=vtrack.dv_pos[0], y=vtrack.dv_pos[1],
   *       px=vtrack.dv_mom[0], and py=vtrack.dv_mom[1].
   * Therefore, L = (px*(vx-x)+py*(vy-y)) / (px*px+py*py)
   */
  void calc_ClosestApproach(const SvxVertexTrack *vtrack, float vx, float vy,
                            float &ca_x, float &ca_y);


  /**
   * Calculate z position of vtrack at x=x0 and y=y0.
   * Before you use this function, you should confirm that pT of vtrack is not 0.
   */
  float calc_z(const SvxVertexTrack *vtrack, float x0, float y0);

  int   m_side; // 0:both, 1:west, 2:east
  SvxSegmentList *d_segment; ///< Input data node providing SVX track segments.
  VtxOut *d_vtxout; ///< Data node for storing vertex information.

  SvxClusterList *d_cluster;///< Data node for reading in clusters to re calculate chisq after finding new vtx
  svxDetectorGeo *d_svxgeometry;///< contains detector geometry. used for tracking
  
  std::vector<SvxVertexTrack*> dv_vtxtracklist; ///< Vector of all SvxVertexTrack objects passing certain quality criteria (converted from SvxSegment objects).
  std::vector<SvxVertexTrack*> dv_neartracklist; ///< Vector of SvxVertexTrack objects within a certain x-y and z range around a vertex seed.
  SvxTracker *d_tracker; ///< SvxTracker object.
  std::vector<float> dv_vertex; ///< Vertex position in three dimensions (x,y,z).
  float d_xycut; ///< Cut parameter. Default hard-coded in constructor.
  float d_xycut0; ///< Cut parameter. Default hard-coded in constructor.
  float d_zcut; ///< Cut parameter. Default hard-coded in constructor.
  float d_zcut0; ///< Cut parameter. Default hard-coded in constructor.
  bool  d_use_SvxVtxOut; ///< Flag to handle different node names VtxOut (FALSE) or SvxVtxOut (TRUE).
  bool  d_ppflag; ///< Flag for proton-proton data. @todo Remove flag for pp data?
  bool  d_recalflag; ///< Flag for recalibrated running

  bool  d_zerofieldflag; ///< Flag for zero field?

  PHTimeServer::timer _timer; ///< Timer.

};

#endif
