#ifndef __SVXSTANDALONERECOLOWMULT_H__
#define __SVXSTANDALONERECOLOWMULT_H__

#include <SubsysReco.h>
#include <vector>
#include <PHTimeServer.h>
#include <SvxTracker.h>
#include <svxDetectorGeo.hh>

//class svxDetectorGeo;
class SvxCluster;
class SvxClusterList;
class SvxClusterContainer;
class SvxSegmentList;
class SvxComponentGeom;
class SvxPixelHotDeadMapv2;
class SvxDeadMap;
class svxAddress;

// D. McGlinchey
// this is really annoying and should be fixed before any real integration
// into libsvx, but SvxLinkNodeLM and SvxLinkLM clash with the definitions
// in SvxStandAloneReco.h, and therefore they're renamed here

/**
 * \brief Definition of class SvxLinkNodeLM
 *
 * Class, used only by SvxStandAloneRecoLowMult to
 * store cluster information.
 * Since this is used only by SvxStandAloneRecoLowMult
 * it is stored locally (i.e. this is sort of a
 * private class).
 *
 **/
struct SvxLinkNodeLM {
  SvxLinkNodeLM(SvxCluster *c, float xv, float yv, float zv, int lyr):
    cluster(c), x(xv), y(yv), z(zv), sublayer(lyr) {}
  SvxCluster* cluster;
  float x;
  float y;
  float z;
  int sublayer;
};

/**
 * \brief Definition of class SvxLinkLM
 *
 * Class, used only by SvxStandAloneRecoLowMult to store
 * information about linked clusters (Proto-segments).
 * Since this is used only by SvxStandAloneRecoLowMult it is
 * stored locally (i.e. this is sort of a private class).
 *
 **/
struct SvxLinkLM {
  SvxLinkLM() {
    Rrot = 0.;
    cx = 0.;
    cy = 0.;
    quality = -999.;
    is_real = true;
  }

  SvxLinkLM(SvxLinkNodeLM *node) {
    nodelist.push_back(node);
    posx.push_back(node->x);
    posy.push_back(node->y);
    posz.push_back(node->z);
    sublayerlist.push_back(node->sublayer);
    Rrot = 0.;
    cx = 0.;
    cy = 0.;
    quality = -999.;
    is_real = true;
  }

  SvxLinkLM(const SvxLinkLM *link) {
    nodelist = link->nodelist;
    posx = link->posx;
    posy = link->posy;
    posz = link->posz;
    sublayerlist = link->sublayerlist;
    Rrot = 0.;
    cx = 0.;
    cy = 0.;
    quality = -999.;
    is_real = link->is_real;
  }

  ~SvxLinkLM() {
    nodelist.clear();
    posx.clear();
    posy.clear();
    sublayerlist.clear();
  }

  int get_size() { return nodelist.size(); }

  void add_node(SvxLinkNodeLM *node) {
    nodelist.push_back(node);
    posx.push_back(node->x);
    posy.push_back(node->y);
    posz.push_back(node->z);
    sublayerlist.push_back(node->sublayer);
    Rrot = 0.;
    cx = 0.;
    cy = 0.;
    quality = -999.;
  }

  std::vector<SvxLinkNodeLM*> nodelist;
  std::vector<float> posx;
  std::vector<float> posy;
  std::vector<float> posz;
  std::vector<int> sublayerlist;
  float Rrot;
  float cx;
  float cy;
  float quality;
  bool is_real;

};


class PHCompositeNode;


/**
 * \brief Definition of class SvxStandAloneRecoLowMult
 *
 * Class which associates clusters from the VTX
 * into tracks. Fills associated clusters into
 * SvxSegment, and puts the list of SvxSegments
 * (SvxSegmentList) onto the node tree.
 *
 **/
class SvxStandAloneRecoLowMult : public SubsysReco
{
public:
  SvxStandAloneRecoLowMult(const std::string &name = "SvxStandAloneRecoLowMult");
  ~SvxStandAloneRecoLowMult();

  int Init(PHCompositeNode *topNode) {return 0;}
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode) {return 0;}

  void setVertexRecoFlag(int flag);
  void setZerofieldFlag(bool flag) {d_zerofieldflag = flag;}
  static int segmentID;

private:
  int run_patternRecognition(); // main routine for pattern recognition
  void run_linkFitting(); // main routine for link (track) fitting

  SvxClusterList *d_cluster;
  SvxSegmentList *d_segment;

  float d_vertex[3];    //primary vertex
  int d_vertexrecoflag;

  /*** private functions used by run_method1 ***/
  void LinkClusters(int sublayer, SvxLinkLM *link,
                    float x0, float z0, float dz);
  void quickCircleCalculator(SvxLinkLM *link);
  bool calc_projection(SvxLinkLM *link, float R,
                       float &z_projection, float &dz);


  svxDetectorGeo *d_svxgeometry;
  SvxClusterContainer *d_vtxclusters;
  SvxTracker d_tracker;
  std::vector<SvxLinkLM*> d_linklist;
  std::vector<SvxLinkLM*> d_goodlinklist;

  PHTimeServer::timer _timer;   ///< Timer

  bool d_Bpolarity;

  double Rsub[svxDetectorGeo::SVXNSUBLAYER]; // nominal sublayer radii

  bool d_use_SvxVtxOut;
  /// d_use_SvxVtxFlag=true  : reconstructed vertex by VTX is stored in SvxVtxOut node.
  /// d_use_SvxVtxFlag=false : reconstructed vertex by VTX is stored in VtxOut node.
  /// When reconstruction of VTX and CNT separately, this flag should be true.


  bool d_ppflag;
  /// When you analyze p+p events, set this flag true.

  bool d_zerofieldflag;
  /// When running over zero field, set this flag to true.

  SvxComponentGeom* d_svxCompGeom; // Detector model for tracking and dead-area checking
  SvxPixelHotDeadMapv2* d_pixelMap;
  SvxDeadMap* d_stripMap;
  svxAddress* d_address;


  /* analysis parameters */
  int d_minhit; //minimum number of hits associated to link
  bool d_recomode;




};

inline void SvxStandAloneRecoLowMult::setVertexRecoFlag(int flag)
{
  if (flag < 0 || flag > 2) {
    d_vertexrecoflag = 0;
  } else {
    d_vertexrecoflag = flag;
  }
  std::cout << "vertexrecoflag is set to " << d_vertexrecoflag << std::endl;
}

#endif
