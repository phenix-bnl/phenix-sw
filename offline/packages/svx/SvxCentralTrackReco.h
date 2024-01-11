#ifndef __SVXCENTRALTRACKRECO_H__
#define __SVXCENTRALTRACKRECO_H__

#include <SubsysReco.h>
#include <PHTimeServer.h>
#include <SvxTracker.h>

#include <vector>

class SvxCluster;
class SvxClusterContainer;
class PHSnglCentralTrack;
class VtxOut;
class svxDetectorGeo;
class SvxComponentGeom;
class ScgTrack;
class SvxPixelHotDeadMapv2;
class SvxDeadMap;
class svxAddress;
/**
 * \brief Definition of structure SvxClsLinkNode
 *
 * Structure, used only by SvxCentralTrackReco,
 * which stores linked cluster information.
 * Since this class is only used by SvxCentralTrackReco, it is
 * locally stored in this file (i.e. this is a sort of Private Class)
 *
 */
struct SvxClsLinkNode {
  SvxClsLinkNode(SvxCluster *c, float dp, float mb, float zp, short sub, bool fo, short id):
    cluster(c), dproj(dp), mag_bend(mb), zproj(zp), sublayer(sub), found(fo), clsid(id),
    fit_dphi(-9999.), fit_dz(-9999.),
    scatter(-9999.), scatter_xy(-9999.), scatter_rz(-9999.), sscatter(-9999.),
    pxout(-9999.), pyout(-9999.), pzout(-9999.) {}

  SvxCluster* cluster;

  float dproj;
  float mag_bend;
  float zproj;
  short sublayer;
  bool  found;
  short clsid;

  float fit_dphi;
  float fit_dz;
  float scatter;
  float scatter_xy;
  float scatter_rz;
  float sscatter;

  float pxout;
  float pyout;
  float pzout;

  float get_zdiff();
  float get_pdiff();
  int   get_layer();
};

/**
 * \brief Definition of structure SvxClsLink
 *
 * Structure, used only by SvxCentralTrackReco,
 * which stores all possible linked clusters for a given central arm track
 * Since this class is only used by SvxCentralTrackReco, it is
 * locally stored in this file (i.e. this is a sort of Private Class)
 *
 */
struct SvxClsLink {
  SvxClsLink() :
    m_previdx(-1), m_nhit(0), m_chi2(0.0), m_ndf(0), m_chi2_2(0.0),
    m_chi2_dphi(0.0), m_chi2_dz(0.0), m_ndf_dphi(0), m_ndf_dz(0),
    m_d2dca(-9999.0), m_dL(0), m_dbend(0),
    m_d2dca0(-9999.0), m_d2dca1(-9999.0), m_d2dca2(-9999.0),
    m_dca_z(-9999.0),
    m_d2dca_precise(-9999.0), m_dca_z_precise(-9999.0),
    m_d2dca_bc(-9999.0), m_dca_z_bc(-9999.0),
    m_bestlink(false), m_id(-1),
    m_pt(0.0), m_phi0(0.0), m_the0(0.0),
    m_linkValue(0), m_linkScore(0.0), m_linkQuality(0),
    m_chi2_nocnt(0), m_ndf_nocnt(0)
  {
    m_nhitlayer[0] = 0; m_nhitlayer[1] = 0; m_nhitlayer[2] = 0; m_nhitlayer[3] = 0;
    m_dcapos[0] = -9999.0; m_dcapos[1] = -9999.0; m_dcapos[2] = -9999.0;
  };
  // use default copy constructor
  SvxClsLink(const SvxClsLink& link) { copy(link); }

  SvxClsLink& operator=(const SvxClsLink& link) { copy(link); return *this;}

  void copy(const SvxClsLink &link) {
    m_nodelink = link.m_nodelink;
    m_chi2     = link.m_chi2;
    m_ndf      = link.m_ndf;
    m_chi2_2   = link.m_chi2_2;
    m_chi2_dphi = link.m_chi2_dphi;
    m_ndf_dphi = link.m_ndf_dphi;
    m_chi2_dz  = link.m_chi2_dz;
    m_ndf_dz   = link.m_ndf_dz;
    m_dcapos[0] = link.m_dcapos[0];
    m_dcapos[1] = link.m_dcapos[1];
    m_dcapos[2] = link.m_dcapos[2];
    m_nhit     = link.m_nhit;
    m_nhitlayer[0] = link.m_nhitlayer[0];
    m_nhitlayer[1] = link.m_nhitlayer[1];
    m_nhitlayer[2] = link.m_nhitlayer[2];
    m_nhitlayer[3] = link.m_nhitlayer[3];
    m_previdx  = link.m_previdx;
    m_bestlink = link.m_bestlink;
    m_d2dca    = link.m_d2dca;
    m_dL       = link.m_dL   ;
    m_dbend    = link.m_dbend;
    m_d2dca0   = link.m_d2dca0;
    m_d2dca1   = link.m_d2dca1;
    m_d2dca2   = link.m_d2dca2;
    m_dca_z     = link.m_dca_z;
    m_d2dca_precise = link.m_d2dca_precise;
    m_dca_z_precise = link.m_dca_z_precise;
    m_d2dca_bc = link.m_d2dca_bc;
    m_dca_z_bc = link.m_dca_z_bc;

    m_id       = link.m_id;

    m_pt = link.m_pt;
    m_phi0 = link.m_phi0;
    m_the0 = link.m_the0;

    m_linkValue = link.m_linkValue;
    m_linkScore = link.m_linkScore;
    m_linkQuality = link.m_linkQuality;

    m_chi2_nocnt = link.m_chi2_nocnt;
    m_ndf_nocnt  = link.m_ndf_nocnt;
  }

  void addnode(SvxClsLinkNode &node);

  int getPrevHitIdx() { return m_previdx; } // return previous hit index in the node link.
  // If no hit found, return -1

  int getNHit() { return m_nhit; }
  int getNHitLayer(int layer) { return (0 <= layer && layer < 4) ? m_nhitlayer[layer] : -1; }
  int getNHitSum() {
    int sum = 0;
    for (int ilay = 0; ilay < 4; ilay++) {
      if (m_nhitlayer[ilay] > 0) sum++;
    }
    return sum;
  }

  int getInnerMostLayer();

  void setLinkValue(ScgTrack &geoTrack);
  int getLinkValue() {return m_linkValue;};
  void setLinkScore(ScgTrack &geoTrack);
  float getLinkScore() {return m_linkScore;};

  void print();

  //////////////////
  // link information
  std::vector<SvxClsLinkNode> m_nodelink;
  int  m_previdx; // index of previous hit node in the nodelink
  int  m_nhit;    // N associated hit sum at 8 sublayer
  int  m_nhitlayer[4]; // N associated hit at layer

  float m_chi2;
  int   m_ndf;
  float m_chi2_2;

  float m_chi2_dphi, m_chi2_dz;
  int   m_ndf_dphi,  m_ndf_dz;

  float m_dcapos[3];

  float m_d2dca;
  float m_dL;     // temporary
  float m_dbend;  // temporary
  float m_d2dca0; // temporary
  float m_d2dca1; // temporary
  float m_d2dca2; // temporary

  float m_dca_z;

  float m_d2dca_precise;
  float m_dca_z_precise;
  float m_d2dca_bc;
  float m_dca_z_bc;

  bool  m_bestlink; // flag showing bestAssociation
  int   m_id;

  float m_pt;
  float m_phi0;
  float m_the0;

  // Bitmask classifying current link for use in
  // SvxCentralTrackReco::chooseBestLink(). See
  // SvxClsLink::setLinkValue() for details
  unsigned int m_linkValue;

  // Link score for use in chooseBestLink()
  float m_linkScore;

  // Link quality rating for use in chooseBestLink()
  float m_linkQuality;

  // chi2 w/o cnt angle
  float m_chi2_nocnt;
  int   m_ndf_nocnt;
};

/**
 * \brief Definition of class SvxTrackPart
 *
 * Class, used only by SvxCentralTrackReco,
 * which stores relevant central arm track information.
 * Since this class is only used by SvxCentralTrackReco, it is
 * locally stored in this file (i.e. this is a sort of Private Class)
 *
 */
class SvxTrackPart {
public:
  SvxTrackPart(): mom(-999.), charge(0), phi0(-999.), the0(-999.), pt(-999.), ux(0), uy(0), R(0) {}
  SvxTrackPart(float Mom, int Chrg, float Phi0, float The0);

public:
  float mom;
  int   charge;
  float phi0;
  float the0;

  float pt;
  float ux, uy;

  float R;
};

/**
 * \brief Definition of Class SvxCentralClusterLink
 *
 * Class, used only by SvxCentralTrackReco,
 * which stores central arm track information along with linked cluster information
 * (container of all the possible links)
 * Since this class is only used by SvxCentralTrackReco, it is
 * locally stored in this file (i.e. this is a sort of Private Class)
 *
 */
/////////////////////////////////
// 2012.04.05 rotation angle stores the phi0 and the0 instead of the rotation angle
// 2012.04.11 rotation angle is moved to the SvxTrackPart
class SvxCentralClusterLink
{
public:
  SvxCentralClusterLink(int id, PHSnglCentralTrack *trk):
    m_trkid(id), m_central(trk), m_bestLinkId(-1)
  {
    for (int i = 0; i < 8; i++) {
      m_multihit[i] = 0;
    }

    for (int i = 0; i < 4; i++) {
      m_live_percent[i] = -1;
    }

  };

  void print();
  void setBestLinkId(int bestId);

  void setTrackPart(float mom, int charge, float phi0, float the0)
  {
    m_trkpart = SvxTrackPart(mom, charge, phi0, the0);
  }

  static void Verbosity(int val) { m_verbosity = val; }

public:
  SvxTrackPart        m_trkpart;

  int                 m_trkid;
  PHSnglCentralTrack *m_central;
  std::vector<SvxClsLink> m_vlink;
  int  m_bestLinkId;
  int  m_multihit[8]; ///< count how many tracks use this cluster. idx:sublayer

  // Percentage of channels in a region surrounding (expected) hit
  // position that are live
  float m_live_percent[4];

  static int m_verbosity;
};

/**
 * \brief Definition of class SvxCentralTrackReco
 *
 * Class which associates clusters in the VTX with central arm tracks.
 *
 *******************************************************************************
 *
 * 2012.04.11 add utility function
 *
 * 2014.05.07 D. McGlinchey -
 *  Added a temporary fix for zero field running.
 *  Added a flag to signal ZF running (m_zfflag) which can be set by
 *  SvxCentralTrackReco::setZeroFieldFlag(true).
 *  When "true", this sets:
 *    SvxTracker::set_ZeroFieldFlag(true).
 *    DC mom to 100 GeV (default in ZF is "inf")
 *    DC charge to (-)1 (default in ZF is (-)9999).
 *  This is meant to be a temporary solution which bypasses
 *  the problems in SvxCentralTrackReco::calculateChisquareByMultiCircle()
 *  and SvxTracker::TrackFit_SvxCNT().
 */
class SvxCentralTrackReco : public SubsysReco
{
public:

  // utility
  /**
   * Calculate residual value (dphi and dz)
   */
  static void calculate_dphidz(SvxTrackPart& trkpart,       // track info
                               float svxx, float svxy, float svxz,        // cluster position
                               float xvtx, float yvtx, float zvtx,        // prim. vertex position;
                               float bDir,                                // direction of B-field
                               float* dproj, float* magbend, float* zproj // "output"
                              );

  SvxCentralTrackReco(const std::string &name = "SVXCENTRALTRACKRECO");
  virtual ~SvxCentralTrackReco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  inline int Reset(PHCompositeNode *topNode)            {return 0;}
  inline int ResetEvent(PHCompositeNode *topNode)       {return 0;}
  inline void Print(const std::string&) const {}

  void PrintCsvLines(int ievent, int itrack, SvxCentralClusterLink* trkseed, ScgTrack& geoTrack);

  /**
   * Set the Verbosity Level (for SvxCentralTrackReco as well as SvxCentralClusterLink)
   * - 0 : no message
   * - 1 : function name
   * - 2 : result of the calculation and judgement
   * - 3 : object identity
   */
  virtual void Verbosity(const int ival) { SubsysReco::Verbosity(ival); SvxCentralClusterLink::Verbosity(ival);}


  /**
   * Choose which vertex is used in the SvxCntTrack reconstruction
   *   comment updated : 2021/1/13 T. Hachiya
   * Possible values:
   * - 0: Choose most precise vertex available. (default)
   * - 1: Use "SIM" : The vertex from SIM for simulation
   * - 2: Use "SVX_PRECISE" : The vertex determined by the VTX
   * - 3: Use "SVS" : The seed vertex determined by the VTX
   * - 4: Use "BBC" : The vertex determined by the BBC's
   * - 5: Use "Combined" : The vertex determined by combining the VTX precise and seed
   * - 6: Use "Centrality" : The seed vertex for peripheral(cent>60, bbcq<160) otherwise the precise vertex
   *
   */
  void setVertexFlag(int flag) { m_vtxflag = flag; }


 /**
   * Indicate that reconstruction is to be run in p+p mode
   * Namely, tracks are reconstructed relative to the seed vertex
   * i.e., (bc_x, bc_y, seed_z)
   */
  void setPPFlag(int flag);

  /**
   * Flag for keeping/clearing all links made in LinkCluster except the best.
   *
   * Possible Values:
   * - true: clear all other links in LinkCluster other than the best one.
   * - false: keep all links in LinkCluster.
   *
   */
  void setClearOtherLinkFlag(bool flag = true) { m_clearOtherLink = flag; }

  /**
   * Return the full tracklist. This would be useful for debugging and analysis.
   */
  std::vector<SvxCentralClusterLink*>& getTrackList() { return m_vtrklist; }

  /**
   * Flag for turning on/off BG tracking mode.
   *
   * Possible Values:
   * - 0: do not use BG tracking mode. (default)
   * - else: Use BG tracking mode.
   *
   */
  void RndmAssocDchFlag(int flag) { m_rndmassocflag = flag; return; }

  /**
   * Set the search window for dz cut.
   *
   * Possible values:
   * - 0: default
   * - 1: tight cut (1cm)
   * - 2: middle (3cm)
   * - 3: loose cut (5cm)
   *
   */
  void setSearchWindowFlag(int flag) { m_windowflag = flag; };

  /**
   * Set the rotation angle for BG tracks. The default value is 10 degrees.
   */
  void setRotationAngleBG(float angle) { m_rotAngle = angle; }


  // use for studying the cut parameter
  /**
   * Set the width of the search window in dphi at sublayer (0-7). Default values are 0.4cm.
   */
  void setSearchWindowDPHI(int sublayer, float val) { if (0 <= sublayer && sublayer < 8) { m_cutDPHI[sublayer] = val; }}

  /**
   * Set the width of the search window in dz for all sublayers. The values are dependent on BBC charge
   * - id=0 : Used if primary or seed vertex is found or if no SVX vertex and bbccharge>200. (default=0.5 cm)
   * - id=1 : Used if no Svx vertex and 50<bbccharge<200. (default=2.0 cm)
   * - id=2 : Used if primary or seed vertex is found.(default=5.0 cm)
   *
   */
  void setSearchWindowDZ(int id, float val)         { if (0 <= id && id < 3)             { m_cutDZ[id] = val; }}

  /**
   * Set the factor to make the width size bigger/smaller
   */
  void setWindowFactor(float val) {
    m_windowFactor = val;
    for (int isub = 0; isub < 8; isub++) { m_cutDPHI[isub] *= m_windowFactor; }
    for (int iset = 0; iset < 3; iset++) { m_cutDZ[iset]   *= m_windowFactor; }
  }

  void setSphiCNTout(double sphi)   { m_tracker.set_sphiCNTout(sphi);   }
  void setSthetaCNTout(double sthe) { m_tracker.set_sthetaCNTout(sthe); }
  void setMisAlignmentPhi(double phi) { m_tracker.set_MisAlignmentPhi(phi); }
  void setMisAlignmentZ(double z)     { m_tracker.set_MisAlignmentZ(z);     }

  /**
   * Print comma-separated lines containing link candidate info
   */
  void setPrintLinkInfo(bool b) { m_printCsv = b; }


  /**
   * Set zero field flag
   */
  void setZeroFieldFlag(bool flag) { m_zfflag = flag; }


  /**
   * Set flag for shifting PHCentralTrack into vtx coordinate system
   */
  void setRotatePHCentralTrack(bool flag) { m_shiftphcentral = flag; }

  /**
   * Set the rotations for the PHCentralTrack phi/theta for a given arm
   * arm must be 0 (East) or 1 (West)
   */
  void setPHCentralTrackDphiDtheta(int arm, float dphi, float dtheta);

  /**
   * Get the PHCentralTrack dphi values for a given arm
   * 0: East
   * 1: West
   */
  float getPHCentralTrackDphi(int arm)
  {
    return (arm >= 0 && arm < 2) ? m_rotPHcntPhi[arm] : -9999.;
  }

  /**
   * Get the PHCentralTrack dtheta values for a given arm
   * 0: East
   * 1: West
   */
  float getPHCentralTrackDtheta(int arm)
  {
    return (arm >= 0 && arm < 2) ? m_rotPHcntTheta[arm] : -9999.;
  }

  /**
   * Update the PHCentralTrack dphi & dtheta rotation parameters
   * in the DB.
   * Valid from beginrun to endrun
   */
  bool updateDBPHCentralTrackDphiDtheta(const int beginrun, const int endrun, const char *desc);

  /**
   * Set flag for reading PHCentralTrack rotations from the DB
   * true : Read from the DB
   * fals : Use set values
   *
   * Since rotation values will attempt to be fetched from the DB during
   * InitRun(), this is needed to make sure values set in a macro
   * (ex. during calibration) are not overwritten.
   */
  void setFetchPHCentralTrackRotFromDB(bool flag) { m_readphcntrotdb = flag;}

  /**
   * Fetch the parameters for the PHCentralTrack rotations from the DB
   * for runnumber.
   */
  bool fetchDBPHCentralTrackDphiDtheta(const int runnumber);

  /**
   * set flag to enable the link splitting algorithm in LinkCluster.
   */
  void setEnableLinkSplitting(bool flag) { m_enableLinkSplitting = flag; }

  /**
   * set flag to enable the requirement of B2 or B3 hit.
   */
  void setRequireB2B3hit(bool flag) { m_requireB2B3hit = flag; }

  /**
   * set flag to fit without CNT angle : false:use CNT, true: not use CNT
   */
  void setFitNoCNT(bool flag) { m_isFitNoCNT = flag; }


  SvxTracker* getTracker() { return &m_tracker; }

protected:
  int CreateNodeTree(PHCompositeNode *topNode);

  /**
   * Make all the possible cluster links for a single CNT track
   */
  void LinkClusters(SvxTrackPart& part,                    // track info
                    float xvtx, float yvtx, float zvtx,    // primary vtx
                    SvxClsLink&          link,             // link data
                    SvxClusterContainer* d_svxcls,         // ClusterContainer
                    int block, float dphi, float dz        // search condition
                   );

private:

  /**
   * Clear all the temporary information stored in the array of SvxCentralClusterLink.
   */
  void clearTrackList();

  /**
   * Get the cluster position.
   */
  void getClusterPos(SvxCluster* cls, float* sx, float* sy, float* sz);

  /**
   * Get the vertex position.
   * Which vertex is used depends on the vertex flag (see setVertexFlag())
   */
  void getPrimaryVtx(VtxOut* vtxout, float* vx, float *vy, float *vz);

  /**
   * Choose the best link from all possible links for the given CNT track.
   */
  int optimalLink(SvxCentralClusterLink *trk, float vx, float vy, float vz);

  /**
   * Calculate the track chi2 by a multi circle fit. This is the main chi2 function stored with the CNT track information,
   */
  void calculateChisquareByMultiCircle(SvxCentralClusterLink *trk,
                                       SvxClsLink *link,
                                       double vx, double vy, double vz, // primary vertex
                                       double *d2dca                    // return value
                                      );

  /**
   * Apply correlation cut when looking for the cluster link.
   * \todo This might have to be implemented in SvxClsLinkNode.
   */
  bool applyCorrelationCut(SvxClsLinkNode *hit, SvxClsLinkNode *prevhit);

  /**
   * Apply dz cut when looking for the cluster link.
   *
   * Currently: if passed a NULL SvxClsLinkNode and a variable it will set the dz cut. If passed a SvxClsLinkNode and a Null float, it will apply the dz cut.
   * \todo Split this function into a setDZCut() and applyDZCut() function.
   */
  //bool applyDZCut(SvxClsLinkNode *hit, float* dzcut_out=NULL);
  bool applyDZCut(SvxClsLinkNode *hit);
  // this is the function to choose the associated cluster at the most outer layer

  float getDZCut(bool update = false);


  /**
   * Create the SvxCntralTrack, fill it with the best association information, and put it on the DST.
   */
  int fillCentralTrack(PHCompositeNode *topNode);



  //////////////////////////////
  /**
   * core subroutine for calculateDCA with circleProjection method
   */
  void calcDCAbyCircleProjection(double pt, double phi, int charge,// momenturm, phi at innermost layer
                                 double hx, double hy,             // hit position at inner most layer
                                 double vx, double vy,             // primary vertex
                                 double* dx, double* dy,           // DCA position
                                 double* d2dca                     // return
                                );

  /**
   * core subroutine for calculateDCA with circleProjection method
   */
  void calcDCAbyCircleProjection(double pt, double phi, double the, // momenturm, phi at innermost layer
                                 int charge,                        // charge of the track
                                 double hx, double hy, double hz,   // hit position at inner most layer
                                 double vx, double vy,              // primary vertex
                                 double* dx, double* dy, double* dz,// DCA position
                                 double* phi0, double* d2dca        // return
                                );


  // utility
  /**
   * Get a correlation index determined by two hits between layers.
   */
  int getCorrelationIdx(int layer, int layer_prev);

  // updated method estimated by B->e simulation
  /**
   * Return the correlation slope for dphi using correlation index.
   */
  float getCorrelationSlopeDPParallel(int layer, int layer_prev);
  /*
   * Return the correlation slope for dz using correlation index.
   */
  float getCorrelationSlopeDZParallel(int layer, int layer_prev);


  // nominal radius of sublayer
  double Rsub(const int isub); ///< return radius for sublayers

  /**
   * Clear all the links in the container except for the best.
   */
  void clearOtherLink(SvxCentralClusterLink *trk);

  /**
   * Shift PHCentralTrack into vtx coor system
   */
  void shiftPHCentralTrack(float *phi0, float *the0);


protected:
  std::vector<SvxCentralClusterLink*> m_vtrklist; ///< central track based information in a event
  int m_ppflag;        ///< flag to indicate reconstruction in p+p mode, which determines pattern reco relative to the the seed vertex
  int m_vtxflag;       ///< flag which vertex is used for the association
  int m_rndmassocflag; ///< flag if you check random BG association
  int m_windowflag;    ///< which dphi & dzcut

  float m_fieldScale; ///< factor of the magnetic field

  svxDetectorGeo*       m_geometry;
  SvxComponentGeom*     m_svxCompGeom; // Detector model for tracking and dead-area checking
  SvxPixelHotDeadMapv2* m_pixelMap;
  SvxDeadMap*           m_stripMap;
  svxAddress*           m_address;

  bool m_printCsv;       ///< Print comma-separated output for best-link studies
  bool m_clearOtherLink; ///< if true, clear NotBestLink in track by track. it is useful to minimize the momeory consumption

  // association cut at most outer layer
  float m_cutDPHI[8]; // 0-7: sublayer. currently use same cutvalue for all sublayers
  float m_cutDZ[3];   // 0: tight, 1:middle, 2:loose
  float m_windowFactor;

  float m_rotAngle;   ///< rotation angle for rndmBG


  // event by event info
  //bool  m_vtx_found; ///< if true, vertex (SVX/SVX_PRECISE) is found
  int   m_vtx_found; ///< 1: precise/sim found, 2: seed found, 0: BBC vertex
  float m_bbccharge; ///< bbcchargesum, if no bbcout, -9999.0 is filled
  float m_centrality; ///< centrality, if no PHGlobal, -9999.0 is filled

  SvxTracker m_tracker; ///< utility class used for fitting track

  PHTimeServer::timer m_timer;   ///< Timer

  bool m_zfflag; //true: Zero field running, false: Field on running

  bool m_shiftphcentral; //true: shift PHCentralTrack into vtx coor system
  bool m_readphcntrotdb; //true: read PHCentralTrack rotations from DB
  const char *m_phcntrotdbname; //table name for PHCentralTrack rotations

  float m_rotPHcntPhi[2];
  float m_rotPHcntTheta[2];

  bool  m_enableLinkSplitting;
  bool  m_requireB2B3hit;

  bool  m_isFitNoCNT;

  bool   m_newdataset_for_run16;
  float  m_vtxPrecise[3];
  float  m_vtxBeamCenter[3];
};

#endif
