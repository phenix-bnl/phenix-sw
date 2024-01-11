// $Id: Tools.h,v 1.15 2012/09/14 21:39:29 slash Exp $
#ifndef tools_hxx
#define tools_hxx

/*!
  \file    Tools.h
  \brief   calculation formulae for picoDST filling
  \version $Revision: 1.15 $
  \date    $Date: 2012/09/14 21:39:29 $
*/

// Tools.h is a utility routine for calculations from Muon nanoDSTs and is located in offline/packages/MWGpico/tools/
#include <TROOT.h>
#include <map>
#include <list>
#include <string>
#ifndef __CINT__
#include <boost/array.hpp>
#endif
#include <Lvl2OutArray.h>

#include <MWGL2.h>

class TFile;
class PHGlobal;
class headerWrapper;
class PHMuoTracksOut;
class TH1;
class PHCompositeNode;
class L2MuidData; 
class Lvl2OutArray;

//! calculation formulae for picoDST filling
class Tools {
  
  public: 

  //! converts "new framework" muid hit pattern into "old framework" hit pattern
  static int get_muid_hit_pattern( int muioo_hit_pattern );
  
  /*! 
    gets muid pannel associatedt to a given point
    returns -1 if non found
  */
  static int get_muid_panel( Float_t x, Float_t y, Float_t z );
  
  /*! 
    gets muid pannel associated to a given road
    returns -1 if non found
  */
 	static int get_muid_panel( int imu, int iroad, PHMuoTracksOut* muo );
  
  //! angle between two momentum vector
  static Float_t angle(Float_t p0[3], Float_t p1[3]);
  
  //! angle between two momentum vector
  static Float_t angle(Float_t px0, Float_t py0, Float_t pz0,
                       Float_t px1, Float_t py1, Float_t pz1);

  //! distance track vertex is from nominal vertex (0,0,BbcZVertex)
  static Float_t dist_zvtx(PHMuoTracksOut* &muo, Int_t itrk);

  //! hitbit to number of hits
  static Float_t sumbit(Float_t hitp);

  //! cos(theta) in the Collins-Soper reference frame 
  static Float_t costhetaCS(Float_t MASS, Float_t P0[3], Float_t P1[3]);
  
  //! phi in the Collins-Soper reference frame 
  static Float_t phiCS(Float_t MASS, Float_t P0[3], Float_t P1[3], Float_t beamP[3]);

  //! cos(theta) in the Gottfried-Jackson reference frame
  static Float_t costhetaGJ(Float_t MASS, Float_t P0[3], Float_t P1[3], Float_t beamP[3]);
  
  //! phi in the Gottfried-Jackson reference frame
  static Float_t phiGJ(Float_t MASS, Float_t P0[3], Float_t P1[3], Float_t beamP[3]);

  //! cos(theta) in the Helicity reference frame 
  static Float_t costhetaHX(Float_t MASS, Float_t P0[3], Float_t P1[3]);
    
  //! phi in the Helicity reference frame
  static Float_t phiHX(Float_t MASS, Float_t P0[3], Float_t P1[3], Float_t beamP[3]);

  //! angular difference between tracks position vector and momentum vector at MuTr st1
  static Float_t delta_theta(PHMuoTracksOut* &muo, Int_t itrk);
  
  //! angular difference between tracks position vector and momentum vector at MuTr st1 multiplied by momentum
  static Float_t p_delta_theta(PHMuoTracksOut* &muo, Int_t itrk);
  
  //! MC run number
  static int runNumberMC( headerWrapper*, bool& );
  
  //! MC event number
  static long int eventNumberMC( headerWrapper*, bool& );
  
  //! MC vertex
  static double zVertexMC( headerWrapper*, bool& );
  
  //! dAu centrality calculation
  static void dAuCentrality(
          TFile *fclZdcFile, 
          TFile *bbcFile, 
          PHGlobal* &evt, 
          const Float_t TRIGGER_FRACTION, 
          Float_t &bbc, 
          Float_t &fcl, 
          Float_t &zdc);
  
  //! distance of closest approach between two tracks  
  static void dca(
      PHMuoTracksOut* &muo, 
      Int_t idx1, Int_t idx2, 
      Float_t &r_dca, Float_t &x_dca, 
      Float_t &y_dca, Float_t &z_dca);
  

  //! distance of closest approach between a track and the beam line 
  static void DCA(
      PHMuoTracksOut* &muo, 
      Int_t idx1, 
      Float_t &r_dca, Float_t &x_dca, 
      Float_t &y_dca, Float_t &z_dca);
  
  //!
  static void distcls(float x1[3],    float x2[3],
          float tanx1[2], float tanx2[2],
          float x1c[3],   float x2c[3],
          int ifail);
  
  //! distance from the track to the road extrapolated at station3
  static Float_t DS3(PHMuoTracksOut* &muo, Int_t idx);
  
  //! distance from the track to the road extrapolated at station3
  static Float_t DS3(PHMuoTracksOut* &muo, Int_t idx, Int_t iroad);
  
  //! distance from the track to the road assuming both come from (0,0,0)
  static Float_t DS3ctp(PHMuoTracksOut* &muo, Int_t idx); 
  
  //! distance from the track to the road assuming both come from (0,0,0)
  static Float_t DS3ctp(PHMuoTracksOut* &muo, Int_t idx, Int_t iroad ); 
  
  //! distance from the road to the track extrapolated to the gap0
  static Float_t DG0( PHMuoTracksOut* &muo, Int_t idx);
  
  //! distance from the road to the track extrapolated to the gap0 for a given road (new framework)
  static Float_t DA( PHMuoTracksOut* &muo, Int_t idx, Float_t zvtx); 

  //! distance from the road to the track extrapolated to the gap0 for a given road (new framework)
  static Float_t DG0( PHMuoTracksOut* &muo, Int_t idx, Int_t iroad); 
  
  //! distance from the road to the track extrapolated to the gap0
  static Float_t DG0x( PHMuoTracksOut* &muo, Int_t idx); 
  
  //! distance from the road to the track extrapolated to the gap0 for a given road (new framework)
  static Float_t DG0x( PHMuoTracksOut* &muo, Int_t idx, Int_t iroad); 
  
  //! distance from the road to the track extrapolated to the gap0
  static Float_t DG0y( PHMuoTracksOut* &muo, Int_t idx); 
  
  //! distance from the road to the track extrapolated to the gap0 for a given road (new framework)
  static Float_t DG0y( PHMuoTracksOut* &muo, Int_t idx, Int_t iroad); 
  
  //! angle between track and road vectors
  static Float_t DDG0( PHMuoTracksOut* &muo, Int_t idx); 
  
  //! angle between track and road vectors for a given road (new framework)
  static Float_t DDG0( PHMuoTracksOut* &muo, Int_t idx, Int_t iroad); 
  
  //! reference vertex position
  static Float_t DS0( PHMuoTracksOut* &muo, Int_t idx);
  
  //! distance from the track to the road extrapolated at station3
  static Float_t DS0(PHMuoTracksOut* &muo, Int_t idx, Int_t iroad);

  //! particle energy from mass/momentum quadrivector
  static Float_t E(Float_t e[4]);
  
  //! particle energy from mass/momentum quadrivector
  static Float_t E(Float_t mass, Float_t p0[3]);
  
  //! particle energy from mass/momentum
  static Float_t E(Float_t mass, Float_t px, Float_t py, Float_t pz);
  
  //! invariant mass from mass momentum of two particles
  static Float_t invMass(Float_t mass1, Float_t P1[3], Float_t mass2, Float_t P2[3]);
  
  //! invariant mass from mass momentum of two particles
  static Float_t invMass(
             Float_t mass1, Float_t px1, Float_t py1, Float_t pz1,
             Float_t mass2, Float_t px2, Float_t py2, Float_t pz2);

  //! returns true if there is a road of gap depth 2,3,4 associated with the track
  static Bool_t is_road(PHMuoTracksOut* &muo, Int_t itrk);
  
  //! returns true if the deepest road associated with the track is to gap 2 or 3
  static Bool_t is_road_shallow(PHMuoTracksOut* &muo, Int_t itrk);
  
  //! normalized integral of an histogram up to the obs value [between 0 and 1]
  static double flattened(TH1 * histo, Double_t obs); 
  
  //! depth of deepest road associated with the track
  static Int_t max_road_depth(PHMuoTracksOut* &muo, Int_t itrk);
  
  //! number of hits in the muid for the specified road
  static Int_t muid_nhits(PHMuoTracksOut* &muo, Int_t itrk, Int_t iroad);
  
  //! number of hits in the muid for the deepest road
  static Int_t muid_nhits(PHMuoTracksOut* &muo, Int_t itrk);
  
  //! total momentum
  static Float_t p(Float_t p0[3]);
  
  //! total momentum
  static Float_t p(Float_t px, Float_t py, Float_t pz);
  
  //! total momentum
  static Float_t p(PHMuoTracksOut* &muo, Int_t idx);
  
  //! transverse momentum
  static Float_t pT(Float_t p[3]);
  
  //! transverse momentum
  static Float_t pT(Float_t px, Float_t py);
  
  //! pseudo rapidity
  static Float_t pseudorap(Float_t p0[3]);
  
  //! pseudo rapidity
  static Float_t pseudorap(Float_t px, Float_t py, Float_t pz);
  
  //! rapidity
  static Float_t rapidity(Float_t e[4]); // e[0]=mass, e[i]=p[i]
  
  //! rapidity
  static Float_t rapidity(Float_t mass, Float_t p[3]);
  
  //! rapidity
  static Float_t rapidity(Float_t mass, Float_t px, Float_t py, Float_t pz);
  
  //! rapidity
  static Float_t rapidity(
        Float_t mass1, Float_t px1, Float_t py1, Float_t pz1, 
        Float_t mass2, Float_t px2, Float_t py2, Float_t pz2);
        
  //! verify that road is shallow and not just outside next gaps spacial coordinates
  static Bool_t verify_shallow(PHMuoTracksOut* &muo, Int_t itrk);
  
  //! bend plane vertex fit from old framework tracks
  static void vtxBP(
        PHMuoTracksOut* &muo, 
        Int_t idx1, Int_t idx2, Float_t &dca, 
        Float_t &xvtx, Float_t &yvtx, Float_t &zvtx);
  
  //! bend plane vertex fit from old framework tracks
  static void vtxBP(Float_t xbp0[3], Float_t pbp0[3], Float_t xbp1[3], Float_t pbp1[3], // input variables
        Float_t &dca, Float_t &xvtx, Float_t &yvtx, Float_t &zvtx); // output variables
  
  //! x1
  static Float_t x1(Float_t invMass, Float_t p[3], Float_t E_CMS);
  
  //! x1
  static Float_t x1(Float_t invMass, Float_t px, Float_t py, Float_t pz, Float_t E_CMS);
  
  //! x2
  static Float_t x2(Float_t invMass, Float_t p[3], Float_t E_CMS);
  
  //! x2
  static Float_t x2(Float_t invMass, Float_t px, Float_t py, Float_t pz, Float_t E_CMS);
  
  //! x Feynman
  static Float_t xF(Float_t X1, Float_t X2);
  
  //! x Feynman
  static Float_t xF(Float_t invMass, Float_t P[3], Float_t E_CMS);
  
  //! x Feynman
  static Float_t xF(Float_t invMass, Float_t px, Float_t py, Float_t pz, Float_t E_CMS);
  
  //!@name BLT utilities
  //@{
  
  //! stores pseudoBLT 2D decision for each event/arm in corresponding array
  static void LoadBLTDecision( PHCompositeNode *top_node );
  
  //! return pseudoBLT 1S decision for a given arm
  static bool BLT_1S_Decision( const unsigned int& arm, const bool& use_reco = false );

  //! return pseudoBLT 1D decision for a given arm
  static bool BLT_1D_Decision( const unsigned int& arm, const bool& use_reco = false );

  //! return pseudoBLT 2D decision for a given arm
  static bool BLT_2D_Decision( const unsigned int& arm, const bool& use_reco = false );

  //! return pseudoBLT 1D1S decision for a given arm
  static bool BLT_1D1S_Decision( const unsigned int& arm, const bool& use_reco = false );
  //@}
  
  //!@name LL1 utilities
  //@{
  
  //! stores pseudoLL1 2D decision for each event/arm in corresponding array
  static void LoadLL1Decision( PHCompositeNode *top_node );
  
  //! return pseudoLL1 1S decision for a given arm
  static bool LL1_1S_Decision( const unsigned int& arm );

  //! return pseudoLL1 1D decision for a given arm
  static bool LL1_1D_Decision( const unsigned int& arm );

  //! return pseudoLL1 2D decision for a given arm
  static bool LL1_2D_Decision( const unsigned int& arm );

  //! return pseudoLL1 1D1S decision for a given arm
  static bool LL1_1D1S_Decision( const unsigned int& arm );
  //@}
 
  
  //!@name Level2 utilities
  //@{
  
  //! stores level2 decision for each event/arm in corresponding array
  static void LoadL2Decision( PHCompositeNode *top_node );
  
  //! return level2 dimuon mutr trigger decision for a given arm
  static bool L2MutrDecision( const unsigned int& arm );
  
  //! return level2 dimuon muid trigger decision for a given arm
  static bool L2MuidDecision( const unsigned int& arm );
  
  //! return emulated level2 decision for a given dimuon
  static bool L2MuID_dimuOK(int idimu, PHMuoTracksOut* muo, Int_t RUN);
  
  //! return emulated level2 decision for a given track pair/road pair
  static bool L2MuID_dimuOK(
    int idx0, int iroad0, 
    int idx1, int iroad1,
    PHMuoTracksOut* muo, Int_t RUN);

  //! return emulated level2 decision for a given dimuon
  static bool L2MuID_dimuOK(
          Int_t idhits0, Int_t idhits1, 
          Float_t gap0_dxdz0, Float_t gap0_dxdz1,
          Float_t gap0_dydz0, Float_t gap0_dydz1, 
          Int_t RUN );
  
  //! shortcut for list of l2 muid road data
  typedef std::list<MWGL2::L2MuidData> L2MuidDataList;

  //! get lvl2 muid valid primitive informations ( theta and phi ) associated to a given road
  static L2MuidDataList GetMuidPrimitives( int imu, int iroad, PHMuoTracksOut* muo );

  //! returns true if one pair amongst all combinations matching level2 muid triggers
  static bool Find_accepted_l2_pair( const L2MuidDataList& l2_road0, const L2MuidDataList& l2_road1 );

  //! load level2 primitives from dst_node
  static void LoadL2Primitives( PHCompositeNode *top_node );
  
  //! returns true if offline road has at least one valid associated level2 muid road
  static bool L2MuidRoadOK( int imu, int iroad, PHMuoTracksOut* muo );
  
  //! returns true if dimuon has at least one valid associated level2 muid pair
  static bool L2MuidPairOK( int idimu, PHMuoTracksOut* muo );

  //@}
  
  //! set verbosity //AR
  static void Verbosity( int verbose )
  { _verbosity = verbose; }

  private:
  
  //! run number enumeration used in L2MuiD_dimuOK only for RUN4 and RUN5,
  //it is a replica of RunType in MWGpico. Cesar 09-14-2012
  enum ToolsRunType {
    Unknown,
    RUN3,
    RUN4,
    RUN5,
    RUN6,
    RUN7,
    RUN8,
    RUN8pp,
    RUN9,
    RUN10,
    RUN11,
    RUN11pp,
    RUN12,
    RUN12pp

  };

  // hide complicated stuff from CINT
#ifndef __CINT__
  //!@name BLT decisions
  //@{
  //! pseudoblt 1S trigger decision
  static boost::array< bool, 2 > _1S_blt_decision;

  //! pseudoblt 1D trigger decision
  static boost::array< bool, 2 > _1D_blt_decision;

  //! pseudoblt 2D trigger decision
  static boost::array< bool, 2 > _2D_blt_decision;

  //! pseudoblt 1D1S trigger decision
  static boost::array< bool, 2 > _1D1S_blt_decision;
   
  //! pseudoblt 1S trigger decision
  static boost::array< bool, 2 > _1S_blt_reco_decision;

  //! pseudoblt 1D trigger decision
  static boost::array< bool, 2 > _1D_blt_reco_decision;

  //! pseudoblt 2D trigger decision
  static boost::array< bool, 2 > _2D_blt_reco_decision;

  //! pseudoblt 1D1S trigger decision
  static boost::array< bool, 2 > _1D1S_blt_reco_decision;
 
  //@}
  
  //!@name LL1 decisions
  //@{
  
  //! pseudoLL1 1S trigger decision
  static boost::array< bool, 2 > _1S_ll1_decision;

  //! pseudoLL1 1D trigger decision
  static boost::array< bool, 2 > _1D_ll1_decision;

  //! pseudoLL1 2D trigger decision
  static boost::array< bool, 2 > _2D_ll1_decision;

  //! pseudoLL1 1D1S trigger decision
  static boost::array< bool, 2 > _1D1S_ll1_decision;

  //@}
  
  //!@name level2 decisions
  //@{
  
  //! level2 muid dimuon trigger decision
  static boost::array< bool, 2 > _l2_muid_decision;
  
  //! level2 mutr dimuon trigger decision
  static boost::array< bool, 2 > _l2_mutr_decision;

  //! level2 out array node. Updated at each event 
  static Lvl2OutArray* _lvl2_out_array;
#endif

  //@}
  
  //! verbosity
  static int _verbosity; 


};
#endif
