#ifndef __SVXSTANDALONERECO_H__
#define __SVXSTANDALONERECO_H__

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



/**
 * \brief Definition of class SvxLinkNode
 *
 * Class, used only by SvxStandaloneReco to 
 * store cluster information.
 * Since this is used only by SvxStandaloneReco
 * it is stored locally (i.e. this is sort of a
 * private class).
 *
 **/
struct SvxLinkNode {
  SvxLinkNode(SvxCluster *c, float xv, float yv, float zv, int lyr):
    cluster(c),x(xv),y(yv),z(zv),sublayer(lyr){}
  SvxCluster* cluster;
  float x;
  float y;
  float z;
  int sublayer;
};

/**
 * \brief Definition of class SvxLink
 *
 * Class, used only by SvxStandaloneReco to store
 * information about linked clusters (Proto-segments).
 * Since this is used only by SvxStandaloneReco it is
 * stored locally (i.e. this is sort of a private class).
 *
 **/
struct SvxLink {
  SvxLink() {
    Rrot = 0.;
    cx = 0.;
    cy = 0.;
    quality = -999.;
    is_real = true;
  }

  SvxLink(SvxLinkNode *node) {
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

  SvxLink(const SvxLink *link) {
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

  ~SvxLink() {
    nodelist.clear();
    posx.clear();
    posy.clear();
    sublayerlist.clear();
  }

  int get_size() { return nodelist.size(); }

  void add_node(SvxLinkNode *node) {
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

  std::vector<SvxLinkNode*> nodelist;
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
 * \brief Definition of class SvxStandaloneReco
 * 
 * Class which associates clusters from the VTX
 * into tracks. Fills associated clusters into
 * SvxSegment, and puts the list of SvxSegments
 * (SvxSegmentList) onto the node tree.
 *
 **/
class SvxStandAloneReco : public SubsysReco
{
  public:
    SvxStandAloneReco(const std::string &name = "SVXSTANDALONERECO");
    ~SvxStandAloneReco();
    
    int Init(PHCompositeNode *topNode){return 0;}
    int InitRun(PHCompositeNode *topNode);
    int process_event(PHCompositeNode *topNode);
    int End(PHCompositeNode *topNode){return 0;}
    
    void setVertexRecoFlag(int flag);
    void setRecoMethod(int method = 1);
    void setWindowScale(float s){d_windowscale = s;}
    void setProjectionFlag(bool flag){d_use_vtx_projection_default = flag;}
    void setSwapFlag(int flag);
    void setPPFlag(bool flag){d_ppflag = flag;}
    void setB1hitFlag(bool flag){d_requireB1hit_default = flag;}
    void setBbcChargeCut(int cut){d_bbcqcut = cut;}
    void setMaxNClustCut(int cut){d_maxnclustcut = cut;}
    void setZerofieldFlag(bool flag){d_zerofieldflag = flag;}
    void setPhiWindow_default(float dphi0, float dphi1, float dphi2)
          {DPHI0_default = dphi0, DPHI1_default = dphi1, DPHI2_default = dphi2;}
    void setZWindow_default(float dz0, float dz1, float dz2)
          {DZ0_default = dz0, DZ1_default = dz1, DZ2_default = dz2;}
    void setPhiWindow_LowMult(float dphi0, float dphi1, float dphi2)
          {DPHI0_LowMult = dphi0, DPHI1_LowMult = dphi1, DPHI2_LowMult = dphi2;}
    void setZWindow_LowMult(float dz0, float dz1, float dz2)
          {DZ0_LowMult = dz0, DZ1_LowMult = dz1, DZ2_LowMult = dz2;}
    void setPhiWindow_PP(float dphi0, float dphi1, float dphi2)
          {DPHI0_PP = dphi0, DPHI1_PP = dphi1, DPHI2_PP = dphi2;}
    void setZWindow_PP(float dz0, float dz1, float dz2)
          {DZ0_PP = dz0, DZ1_PP = dz1, DZ2_PP = dz2;}

    static int segmentID;

  private:
    void run_link(float cut);  // main routine for pattern recognition
    
    SvxClusterList *d_cluster;
    SvxSegmentList *d_segment;
    
    float d_vertex[3];    //primary vertex
    int d_vertexrecoflag;
    float d_windowscale;

    /*** private functions used by run_method1 ***/
    void LinkClusters(int sublayer, SvxLink *link,
		      float phi0, float dphi,
		      float z0, float dz);

    void quickCircleCalculator(SvxLink *link);
    bool calc_projection(SvxLink *link, float r,
			 float &phi_projection, float &z_projection,
			 float &dphi, float &dz);
    
    void SelectLinks(const std::vector<SvxLink*> &inlist,
		     std::vector<SvxLink*> &outlist);
    SvxLink* compare_links(SvxLink *link1, SvxLink *link2);


    // origin offset
    void getOriginOffset(int weflag, double& xoff, double& yoff, double& zoff); // weflag : west=0, east=1


    /*** data members for new reco method *****/
    int d_reco_method; //select reconstruction method
                       // currently only one method is implemented.
                       // d_reco_method == 1 --> run_method1()
    double Rsub[svxDetectorGeo::SVXNSUBLAYER];

    svxDetectorGeo *d_svxgeometry;
    SvxClusterContainer *d_vtxclusters;    
    SvxTracker d_tracker;
    std::vector<SvxLink*> d_linklist;
    std::vector<SvxLink*> d_goodlinklist;
    std::vector<SvxLinkNode*> d_linknodelist;
    
    PHTimeServer::timer _timer;   ///< Timer

    bool d_Bpolarity;

    bool d_use_SvxVtxOut;
    /// d_use_SvxVtxFlag=true  : reconstructed vertex by VTX is stored in SvxVtxOut node.
    /// d_use_SvxVtxFlag=false : reconstructed vertex by VTX is stored in VtxOut node.
    /// When reconstruction of VTX and CNT separately, this flag should be true.

    bool d_use_vtx_projection;
    /// d_use_vtx_projection=true  : use seed vertex for projection.
    /// d_use_vtx_projection=false : does not use seed vertex for projection.

    bool d_swap[svxDetectorGeo::SVXNSUBLAYER];
    bool d_swapmode;
    /// d_swap[sublayer]=true  : hits on the sublayer are converted to fake hits.
    /// d_swap[sublayer] can be set only from d_swapmode.
    /// d_swapmode=1 : hits on Barrel-2 are converted to fake hits
    /// d_swapmode=2 : hits on Barrel-3 are converted to fake hits
    /// d_swapmode=3 : hits on Barrel-2 & Barrel-3 are converted to fake hits

    bool d_ppflag;
    /// When you analyze p+p events, set this flag true.

    bool d_requireB1hit;
    /// d_requireB1hit=true  : hit on B1 is required in track reconstruction.
    /// d_requireB1hit=false : hit on B1 is not required.

    bool d_use_vtx_projection_default;
    bool d_requireB1hit_default;

    int d_bbcqcut;
    /// When the BBC charge is less than d_bbcqcut, this event is thought as
    /// low multiplicity event and wider search window is used.

    int d_maxnclustcut;
    /// When the ncluster is larger than d_maxnclustcut this event issues ABORTEVET
    /// to prevent huge memory consumption resulting in job crash.

    bool d_zerofieldflag;

    SvxComponentGeom* d_svxCompGeom; // Detector model for tracking and dead-area checking
    SvxPixelHotDeadMapv2* d_pixelMap;
    SvxDeadMap* d_stripMap;
    svxAddress* d_address;


    /* analysis parameters */
    float DPHI0;  //phi acceptance for second hit
    float DPHI1;  //phi acceptance for third hit
    float DPHI2;  //phi acceptance for after fourth hit
    float DZ0;    //Z acceptance for second hit
    float DZ1;    //Z acceptance fot third hit
    float DZ2;    //Z acceptance for after fourth hit
    /// default size
    float DPHI0_default; // default size
    float DPHI1_default; // default size
    float DPHI2_default; // default size
    float DZ0_default; // default size
    float DZ1_default; // default size
    float DZ2_default; // default size
    /// low multiplicity
    float DPHI0_LowMult; // low multiplicity
    float DPHI1_LowMult; // low multiplicity
    float DPHI2_LowMult; // low multiplicity
    float DZ0_LowMult; // low multiplicity
    float DZ1_LowMult; // low multiplicity
    float DZ2_LowMult; // low multiplicity
    /// pp
    float DPHI0_PP; // low multiplicity
    float DPHI1_PP; // low multiplicity
    float DPHI2_PP; // low multiplicity
    float DZ0_PP; // low multiplicity
    float DZ1_PP; // low multiplicity
    float DZ2_PP; // low multiplicity
    int d_minhit; //minimum number of hits associated to link
    bool d_recomode;




};

inline void SvxStandAloneReco::setVertexRecoFlag(int flag)
{
  if(flag<0 || flag>2){
    d_vertexrecoflag=0;
  } else{
    d_vertexrecoflag=flag;
  }
  std::cout << "vertexrecoflag is set to "<<d_vertexrecoflag<<std::endl;
}

inline void SvxStandAloneReco::setRecoMethod(int method) {
  std::cout<<"RecoMethod is now fixed to 1 (run_link())"<<std::endl;
  std::cout<<"Other methods are not implmented at this point"<<std::endl;
  d_reco_method = 1;
}

inline void SvxStandAloneReco::setSwapFlag(int flag) {
  if(flag<1 || flag>3) {
    std::cout<<"No clusters are swapped."<<std::endl;
    d_swapmode = false;
  } else {
    d_swapmode = true;
    if(flag==1 || flag==3) {
      d_swap[2] = true;
      d_swap[3] = true;
      d_swap[4] = true;
      std::cout<<"Clusters on Barrel-2 are swapped."<<std::endl;
    }
    if(flag==2 || flag==3) {
      d_swap[5] = true;
      d_swap[6] = true;
      d_swap[7] = true;
      std::cout<<"Clusters on Barrel-3 are swapped."<<std::endl;
    }
  }
}

#endif
