#ifndef __MFVTXFINDHOUGHTRACKS_H__
#define __MFVTXFINDHOUGHTRACKS_H__
// $Id: mFvtxFindHoughTracks.h,v 1.31 2017/10/12 18:08:27 shlim Exp $

/*!
  \file		mFvtxFindHoughTracks.h
  \ingroup modules
  \brief	 Combinatorial Hough transform based pattern recognition module for the FVTX
  \author	 Aaron Key, updated by Matt Snowball
  \version $Revision: 1.31 $
  \date		$Date: 2017/10/12 18:08:27 $
*/

#include <iostream>
#include <iomanip>
#include <set>
#include <vector>
#include <map>
#include <stdexcept>
#include <cmath>
#include <numeric>
#include <string>

#include <TObject.h>
#include <PHTimer.h>
#include <PHCylPoint.h>

#include <boost/bind.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/connected_components.hpp>
#include <boost/graph/graph_utility.hpp>
#include <boost/graph/incremental_components.hpp>
#include <boost/foreach.hpp>

#include <FVTXOO.h>
#include <mFvtxModuleBase.h>
#include <TFvtxTrkMap.h>
#include <TFvtxCompactTrkMap.h>
#include <TFvtxMCHitMap.h>

#include <TTree.h>
#include <TFile.h>
#include <TString.h>



#define NSLICES 48
#define NSECTOR 24
#define NCAGE 2

class PHCompositeNode;
class TFvtxCoordMap;
class TFvtxSvxClusterMap;
class mFvtxFindTrackPar;
class TTree;
class VtxOut;
class TrigLvl1;




/*-------------------------------------EVHit-----------------------------------------------*/

class EVHit
{
 public:
  EVHit();
  virtual ~EVHit() {}
  
  std::ostream& print(std::ostream& os = std::cout) const;
  
 public:
  int id;
  int event;
  int layer; // Used only for VTX hits
  int arm;
  int cage;
  int station;
  int sector;
  int column;
  int halfwedge;
  int slice;
  int coordnum;     // id of cluster this hit represents (FVTX)
  int svx_hitnum;  // id of cluster this hit represents (VTX)
  double x_hit;
  double y_hit;
  double z_hit;
  double x_hit_beam; // relative to beam xy
  double y_hit_beam; // relative to beam xy
  double r_hit;
  double phi_hit;
	double r_hit_beam; // relative to beam xy
	double phi_hit_beam; // relative to beam xy
  double phi_start;
  double phi_end;
  int ntracks;  // Number of tracks that use this hit
  std::vector<int> trackNum; // track ids that use this hit
  
 public:
  int getLayer() const { return layer; }
  int getCage() const { return cage; }
  int getSector() const { return sector; }
  int getStation() const { return station; }
  int getColumn() const { return column; }
  int getHalfWedgeId() const { return halfwedge; }
  
  int getSlice() const
  {
    if ( arm == -1 ) return slice;
    unsigned int _slice = sector+cage*24;
    return ( arm == 0 ) ? _slice : (69 - _slice)%48;
  }
  
  double get_x() const { return x_hit; }
  double get_y() const { return y_hit; }
  double get_z() const { return z_hit; }
  double get_r() const { return r_hit; }
  double get_phi() const { return phi_hit; }
  double get_x_beam() const { return x_hit_beam; }
  double get_y_beam() const { return y_hit_beam; }
  double get_r_beam() const { return r_hit_beam; }
  double get_phi_beam() const { return phi_hit_beam; }
  double getPhiStart() const { return phi_start; }
  double getPhiEnd() const { return phi_end; }
  int getNtracks() const { return ntracks; }
  
  void* getMCHitPtr() const { return _mc_hit_ptr; }
  void* getCoordPtr() const { return _coord_ptr; }
  void* getSvxPtr() const { return _svx_ptr; }
  
  void setMCHitPtr(void* p) { _mc_hit_ptr = p; }
  void setCoordPtr(void* p) { _coord_ptr = p; }
  void setSvxPtr(void* p) { _svx_ptr = p; }
  
  private:
  
  // These are void* in order to keep this class decoupled from 
  // the phenix code.  The user needs to cast them back the appropriate
  // mutoo framework type in order to use them.
  //
  void* _mc_hit_ptr; //! opaque pointer to the TFvtxMCHit for this EVHit
  void* _coord_ptr; //! opaque pointer to the TFvtxCoord for this EVHit
  void* _svx_ptr; //! opaque pointer to the TFvtxSvxCluster for this EVHit
  
};
typedef EVHit* EVHit_ptr;



/*-------------------------------------EVHoughPair-----------------------------------------------*/

class EVHoughPair
{
  friend class EVHit;
 public:
  EVHoughPair();
  EVHoughPair(const int ID, EVHit* hit1,EVHit* hit2, const int arm, const double vz);
  ~EVHoughPair() {};
  
  static const int z_proj = 20;

  // private:
 public:
  bool _usedFlag; //! Local cached used status

  int id;
  double m; //! Slope of the line formed by the hit pair
  double rInt; //! r intercept of the pair at the reference plane
  double sinAlpha; //! Sin of the angle formed by the pair with the z axis
	double m_beam; //! relative to beam xy
	double rInt_beam; //! relative to beam xy
	double sinAlpha_beam; //! relative to beam xy
  double dcaZ; //! Absolute value of the distance of closest approach in z to the event vertex
  double dcaR; //! Absolute value of the distance of closest approach in r to the event vertex
  double _vtx_z; //! Z vertex position associated with this pair 
  PHPoint _vtx;  //! Pointer to keep copies light
	int _arm;
  
  bool isDupe;
  int ntracks;  //! Number of tracks that use this hit pair
  
  EVHit* h1; //! Hit pointer one of the pair
  EVHit* h2; //! Hit pointer two of the pair
  
 public:
  bool isDuplicate() const {return isDupe;}
  void set_isDuplicate(bool d){isDupe = d;}
  void setHitPtrs(const EVHit*,const EVHit*); //! Assign the hit pointers to the pair
  void setUsed() { _usedFlag = true; }
  inline bool evaluateHitsUsed();    
  
  EVHit_ptr getFirstHitPtr() const { return h1; } //! Retrieve hit pointer one
  EVHit_ptr getSecondHitPtr() const { return h2; } //! Retrieve hit pointer two
  inline bool hitsUnused(); //! Check if BOTH of the hits in the pair are not used in a track
  inline double calcDcaZ(const double vtx_z, const int arm); //! Calculate the distance of closest approach in Z to the provided vertex
  inline double calcDcaR(const double vtx_z, const int arm); //! Calculate the distance of closest approach in R to the provided vertex
  void recalcDcaVals(PHPoint vtx,
		     PHPoint vtx2,int arm); //! Recalculate the best DCA values to the two provided vertices
  double getDcaZ() const { return dcaZ; } //! Retrieve distance of closest approach in Z (abs)
  double getDcaR() const { return dcaR; } //! Retrieve distance of closest approac in R (abs)
  int getNtracks() const { return ntracks; } //! Retrieve the number of tracks that this pair is associated with
  double getrInt() const { return rInt; } //! Retrieve the r intercept of the pair at reference plane
  double getsinAlpha() const { return sinAlpha; }  //! Retrieve the sin of the angle formed with the z axis 
  bool hasSVXCluster() const { return (this->h1->getStation() < 0 || this->h2->getStation() < 0); }
  
  bool isAdjacent(EVHoughPair *otherPair, double rCut, double sinAlphaCut, double rCutFactor, bool useHICuts) const
  {
    //std::cout << "isAdjacent: " << rCut << "  " << sinAlphaCut << "  " 
    //<< fabs(this->rInt - otherPair->rInt) << "  " << fabs(this->sinAlpha - otherPair->sinAlpha) << std::endl;
    if(fabs(this->rInt - otherPair->rInt) == 0 && fabs(this->sinAlpha - otherPair->sinAlpha) == 0) return false;
    /*
    if(this->hasSVXCluster() || otherPair->hasSVXCluster())
      {
	if(useHICuts == true)
	  {
	    rCut *= 1.25;
	    sinAlphaCut *= 1.25;
	  }
	else{
	  rCut *= 1.1;
	  sinAlphaCut *= 1.1;
	}
      }
    */

		// use xrCutFactor (default is x2) r-window cuts for Hough pairs including VTX hits or FVTX ST4
		// now only for pp/pAu
    if((this->hasSVXCluster() || otherPair->hasSVXCluster() || 
					(this->h2->getStation())==3 || (this->h1->getStation())==3 || (otherPair->h2->getStation())==3 || (otherPair->h1->getStation())==3)  
				&& !useHICuts )
		{
			rCut *= rCutFactor;
			sinAlphaCut *= rCutFactor;
	  }

    if(fabs(this->rInt - otherPair->rInt) > rCut) return false; 
    if(fabs(this->sinAlpha - otherPair->sinAlpha) > sinAlphaCut) return false; 
    return true;
  }
  
  std::ostream& print(std::ostream& os = std::cout) const
    {
      std::streamsize ss = std::cout.precision();
      std::setprecision(18);
      os << " EVHoughPair - m: " << this->m
         << " rInt: " <<  this->rInt
         << " sinAlpha: " << this->sinAlpha
         << " dcaZ: " << this->dcaZ
         << " dcaR: " << this->dcaR
         << " vtx_z: " << this->_vtx_z
         << std::endl;
      std::setprecision(ss);

      return os;
    }
  
 private: 
  void calcTransform(); //! Calculate the transform parameters for the hit pointers that have been assigned
};
typedef EVHoughPair* EVPair_ptr;


class EVHoughPairVec 
{

  friend class EVHoughPair;

 public:
  EVHoughPairVec(const int NArms, const int NSlices);
  ~EVHoughPairVec();

  //! Add pointer to pair to pairs vector
  void Add(const int arm,const int slice, EVHit* hit1, EVHit* hit2,const double vertex_z,const bool checkForDuplicate, const bool isHI, const bool isUPC, const bool use_precise_vtx);
  //! Get size of a vector of pointer to pairs for one slice
  inline unsigned int SizeSliceVec(const int arm,const int slice);
  //! Get size of vector for one arm
  inline unsigned int SizeArmVec(const int arm);
  //! Access pointer to pair in pairs at index
  inline EVHoughPair *Get(const int arm,const int slice, int index);
  //! Get the vector of pointers to pairs for a certain slice
  std::vector<EVHoughPair*> GetSliceVec(const int arm, const int slice);
  //! Get vector of vector of pointers to pairs
  std::vector<std::vector<EVHoughPair*> > GetArmVec(const int arm);
  //! Get the actual pair object at index for given arm and slice
  inline EVHoughPair GetObject(const int arm,const int slice, int index);
  //! Duplicate check before adding to pairs vector - uses pairKeys to track
  bool isUnique(EVHit* hit1, EVHit* hit2, const int arm);
  //! Clear pair keys - turns out not to be faster 
  void clearPairKeys(){ pairKeys.clear();}



 private:

  //! Check sin alpha for phase space narrowing
  double getSinAlpha(EVHit* hit1, EVHit* hit2);

  //! Check sin alpha for phase space narrowing
  double getRInt(EVHit* hit1, EVHit* hit2, int arm);

  //! Check to see if the r-sinAlpha is outside decent phase space
  bool failedRSinAlphaCheck(const double rInt, const double sinAlpha);

  //! Check DCAz  
  double getDcaZ(EVHit* hit1, EVHit* hit2, const int arm, const double vertex_z);

  //! Vectors of pairs for hough transform
  std::vector<std::vector<std::vector<EVHoughPair*> > > pairs;
  //! Keys of pairs to check for duplicates
  std::set<std::pair<int,int> > pairKeys;
  //! Max number of arms
  const unsigned int maxArms;
  //! Max number of slices
  const unsigned int maxSlices;
  //! Pair ID 
  int pairId;
  //! Cut on sin alpha for HI events
  const double HISinAlphaCutHigh;//0.8
  const double HISinAlphaCutLow;//0.05

};  

/*-------------------------------------EVTrk-----------------------------------------------*/

class EVTrk
{
  friend class EVHit;
  friend class EVHoughPair;

 public:
  EVTrk();
  EVTrk(const int armIn, const EVPair_ptr& p1);
  virtual ~EVTrk();
  
  // private:
 public:
  int event;                      // Event number
  int track;                      // Id of this track
  int trk_arm;                    // Arm assigned to the track
  int nstationsHit;               // Number of stations hit
  int nlayersHit;                 // Number of VTX layers hit
  int size;                       // Number of hits
  double vtx_z;                   // Z vertex associated with this track
  PHPoint vtx;
  
  unsigned int nshared; // number of hits shared with other tracks
  
  // The hits that make up this track
  std::vector<EVHit_ptr> hits;      //->
  
 public:
  unsigned int getNumHits() const { return hits.size(); }
  unsigned int getNShared() const; 
  
  // Methods to access the underlying hits' properties
  int get_hitid(const int i) const { return hits.at(i)->id; }
  int get_coordnum(const int i) const { return hits.at(i)->coordnum; }
  int get_svx_hitnum(const int i) const { return hits.at(i)->svx_hitnum; }
  int get_layer(const int i) const { return hits.at(i)->layer; }
  int get_arm(const int i) const { return hits.at(i)->arm; }
  int get_arm() const { return trk_arm; }
  int get_cage(const int i) const { return hits.at(i)->cage; }
  int get_station(const int i) const { return hits.at(i)->station; }
  int get_sector(const int i) const { return hits.at(i)->sector; }
  int get_column(const int i) const { return hits.at(i)->column; }
  int get_halfwedge(const int i) const { return hits.at(i)->halfwedge; }
  double get_r(const int i) const { return hits.at(i)->r_hit; }
  double get_phi(const int i) const { return hits.at(i)->phi_hit; }
  double get_x(const int i) const { return hits.at(i)->x_hit; }
  double get_y(const int i) const { return hits.at(i)->y_hit; }
  double get_z(const int i) const { return hits.at(i)->z_hit; }
  double get_r_beam(const int i) const { return hits.at(i)->r_hit_beam; }
  double get_phi_beam(const int i) const { return hits.at(i)->phi_hit_beam; }
  double get_x_beam(const int i) const { return hits.at(i)->x_hit_beam; }
  double get_y_beam(const int i) const { return hits.at(i)->y_hit_beam; }
  
  double getPhiStart() const { return hits.front()->phi_start; }
  double getPhiEnd() const { return hits.front()->phi_end; }
  
  // Get a raw pointer to an EVHit
  const EVHit_ptr getHitPtr(const int i) const { return hits.at(i); }
  
  // Get an opaque pointer to the underlying MCHit (or Coord)
  //
  void* getCoordPtr(const int i) const { return hits.at(i)->getCoordPtr(); }
  void* getSvxPtr(const int i) const { return hits.at(i)->getSvxPtr(); }
  void* getMCHitPtr(const int i) const { return hits.at(i)->getMCHitPtr(); }        
  void sortHitsZ();
  
  // Use the current hits to calculate residuals
  void calculateNShared();
  void clear();
  
  // Use with extreme caution, this contains a sort that will break all of the
  // internal EVTrk bookeeping.
  void uniquify(); 
  
  void addHit(EVHit_ptr h);      // Add a hit to this track
  void addHitPair(EVPair_ptr p); // Add a hit pair to this track
  void addHitNoRef(EVHit_ptr h); // Add a hit to this track, but don't increment the ref count
};
typedef EVTrk* EVTrk_ptr;

/*-------------------------------------type defs-----------------------------------------------*/
typedef std::vector<EVHit_ptr> EVHitVec;
typedef std::vector<EVTrk_ptr> EVTrkVec;
typedef std::vector<EVPair_ptr> EVPairVec;
typedef std::vector<EVPairVec> EVPairVecVec;
//! Track phi to slice correspondence by using an int,double pair
typedef std::pair<int,double> PairIntDbl;
typedef std::vector<PairIntDbl> PairIntDblVec;
typedef PairIntDblVec::iterator PairVecItr;


/*-------------------------------------mFvtxFindHoughTracks-----------------------------------------------*/

class mFvtxFindHoughTracks : public mFvtxModuleBase
{
  friend class EVHit;
  friend class EVHoughPair;
  friend class EVTrk;


 public:
  
  mFvtxFindHoughTracks();
  virtual ~mFvtxFindHoughTracks(){delete _hough_pairs;}
  static mFvtxFindHoughTracks *GetInstance();

  
  void init(PHCompositeNode* top_node);
  void init_run(PHCompositeNode* top_node);
  void end(PHCompositeNode* top_node);

  //! Public event processor
  PHBoolean event(PHCompositeNode* topNode);

  //! Recalculate the DCA values for pairs in the supplied EVPairVec
  void recalcPairDcaVals(EVPairVec v, int arm);
  
  //! Process two hit tracks for the event after precise vertex is known
  bool processTwoHitTracks();

    //! Process two hit tracks for the pixels before a precise vertex is known
  bool processTwoHitSVXTracks();

  //! set using svx cluster
  void set_use_svx_cluster(bool svxcl = true) { _use_svx_cluster = svxcl;}

  //! set number of svx layers to use
  void set_n_svx_layers(int nlay) { _n_svx_layers = nlay; }

  void do_evaluation(bool v) { std::cout << "In mFvtxFindHoughTracks::do_evaluation" << std::endl; }//_do_evaluation = v; }
  //void debug(bool v) { _debug_ = v; _do_evaluation = true; }

  const bool doDebug(){return false;}

  void set_r_window(double dr) { _dR = dr; }
  void set_phi_window(double dphi) { _dPhi = dphi; }

  std::pair<double,double> getPhiStartEnd(int arm, int cage, int station, int sector, int column) const;

  void set_do_muon_quick_reco(bool q = true)
  {
    std::cout << std::endl;
    std::cout << "================== WARNING WARNING WARNING WARNING WARNING WARNING WARNING ================== " << std::endl;
    std::cout << "    mFvtxFindHoughTracks::set_do_muon_quick_reco --- Setting Quick Muon Reco to TRUE!!!" << std::endl;
    std::cout << "  This will only reconstruct FVTX tracks in the region where a muon points from the MuTr." << std::endl;
    std::cout << "         You MUST reconstruct the MUT and MUI before running the FvtxReco module!!!" << std::endl;
    std::cout << "================== WARNING WARNING WARNING WARNING WARNING WARNING WARNING ================== " << std::endl;
    std::cout << std::endl;

    _doQuickMuonReco = true;
  }

 private:

  ///////////Private Functions///////////
    
  //! retrieve pointer to needed nodes
  void setInterfacePtrs(PHCompositeNode* top_node); 

  void clear();
  void clearHitVecs();
  void clearTrackVec();

  void convertCoords();
  void convertSvxClusters();
  
  void findTracks(PHCompositeNode* top_node);

  void applyVertexCut(EVHitVec& v, const int arm);
  
  int fillHoughVec(const int arm, std::vector<bool> quickMuonRecoSlices);

  int findTwoHitCandidates(const int arm, const bool twoHitSVX = false);

  void fillUnusedPairs(const int arm, const int curOffset, const double maxDcaZ);

  void findCandidateHitsInRoad(EVPairVec& passedVec, const int curOffset,
                               const double rClusCut, const double alphaClusCut);  
  
  int graphCluster(const int arm, const double maxDcaZ,
                   const double rClusCut, const double alphaClusCut,
									 std::vector<bool> quickMuonRecoSlices);

  void findClustersHough(const double rClusCut,  const double sinAlphaCut);
  
  void makeEVTrks(EVPairVec& pairVec, const int arm, const int curOffset,
                  const double rClusCut, const double alphaClusCut);  
      
  EVTrk_ptr makeEVTrkFromCandidates(const int arm);
  
  void removeNonOverlapCandidates(EVHitVec& v, EVHit_ptr ref);
  
  void fillOutputNodes();
  void associateHits(EVTrk_ptr t);
  void associatePtrToTrack(EVHit_ptr h, TFvtxTrkMap::iterator& trk_iter);

  void pushNodePars(const PHPoint& pt, TFvtxTrkMap::pointer trk_ptr,
                    const double pz_init, const double pxpz, const double pypz);
  
  void loadExtVertex();
  void loadPreciseVertex();
	void checkUPCTrigger(PHCompositeNode* top_node);
  void setTrkPar(TFvtxTrkMap::pointer trk_ptr, const double _vtx_z, const PHPoint& _vtx);

  void bookTrees();

  std::vector<bool> getGoodMuonCandidateSlices(PHCompositeNode* top_node, int theArm);

  /////////////Variables//////////////
 private:
  static mFvtxFindHoughTracks *HFInstance;

 protected:
  bool _doEval;
  //const bool _doDebug;

 private:
  bool _use_svx_cluster;
  int  _n_svx_layers;
  bool _reverseTracking;
  bool _twoHitMode;
  
  double _dR;
  double _dZ;
  double _dPhi;
  double _dPhiHitsMax;

  // ! Maximum separation in r for cluster pairs to be considered for a track
  double _rClusCutFixed;
  // ! Maximum separation in sin(alpha) for cluster pairs to be considered for a track
  double _alphaClusCutFixed;

  //! Event z vertex position
  double _vertex_z;
  //! Event vertex position
  PHPoint _vertex;
  //! VtxOut object pointer
  VtxOut* _vtxout;
  //! TrigLvl1 object pointer
  TrigLvl1* _triglvl1;
  //! average position
  double _beam_x;
  double _beam_y;

  //! Precise event z vertex position
  double _precise_z;
  //! Precise secondary event z vertex position
  double _precise_z2;

  //! PHPoint for the selected precise vertex
  PHPoint _precise_vtx;
  //! PHPoint for the secondary precise vertex
  PHPoint _precise_vtx2;

	bool _use_precise_vtx;
	bool _isUPC;

  mFvtxFindTrackPar* _mod_par;
  //! tracks
  TFvtxTrkMap* _trk_map; 
  //! compact tracks
  TFvtxCompactTrkMap* _ctrk_map;  
  //! coordinates
  TFvtxCoordMap* _coord_map; 
  //! VTX clusters
  TFvtxSvxClusterMap* _svx_map;  
  //! Pair Manager
  EVHoughPairVec *_hough_pairs;
  //@}

  EVHitVec _candidateHits;
  EVPairVec _unusedPairs; //! EVHoughPair container
  EVPairVecVec _passedRoads; //! Vector of pair vectors passing PR
  EVTrkVec _trk_vec; //! EVTrk track candidate container

  //! Fvtx hit container, indexed by arm and slice
  EVHitVec _hitsFvtx[FVTXOO::MAX_ARM][NSLICES];
  bool _hitsFvtx_limit[FVTXOO::MAX_ARM][NSLICES];

  //! SVX hit container, indexed by slice
  EVHitVec _hitsSvx[NSLICES];

  std::vector<PairIntDbl> _phiLimits;

  //! module timer
  PHTimer _timer;
  int _ievent;

  //! Quick Production
  bool _doQuickMuonReco;
  
  // ----------------------------------
  // Diagnostic evaluation trees
  TFile *evalFile;
  TTree *_hough_cluster_tree;
  int _houghCT_nHits;
  int _houghCT_nClusters;
  int _houghCT_slice;
  int _houghCT_nPairs;
  int _houghCT_pass;
  int _houghCT_arm;
  std::vector<int> *_houghCT_clusID;
  std::vector<float> *_houghCT_hit1_x;
  std::vector<float> *_houghCT_hit1_y;
  std::vector<float> *_houghCT_hit1_z;
  std::vector<float> *_houghCT_hit1_r;
  std::vector<float> *_houghCT_hit1_phi;
  std::vector<float> *_houghCT_hit2_x;
  std::vector<float> *_houghCT_hit2_y;
  std::vector<float> *_houghCT_hit2_z;
  std::vector<float> *_houghCT_hit2_r;
  std::vector<float> *_houghCT_hit2_phi;
  std::vector<float> *_houghCT_pair_rInt;
  std::vector<float> *_houghCT_pair_sinAlpha;
  std::vector<bool> *_houghCT_hasSVXCluster;
  int _houghCT_nMCHitsFromSameTrk;
  int _houghCT_nMCTrksInCluster;
  int _houghCT_nUniqueHits;


  TTree *_found_pairs_tree;
  int _foundPT_slice;
  int _foundPT_pass;
  int _foundPT_arm;
  float _foundPT_hit1_x;
  float _foundPT_hit1_y;
  float _foundPT_hit1_z;
  float _foundPT_hit1_r;
	int _foundPT_hit1_station;
	int _foundPT_hit1_layer;
  float _foundPT_hit1_phi;
  float _foundPT_hit2_x;
  float _foundPT_hit2_y;
  float _foundPT_hit2_z;
  float _foundPT_hit2_r;
	int _foundPT_hit2_station;
	int _foundPT_hit2_layer;
  float _foundPT_hit2_phi;
  float _foundPT_pair_rInt;
  float _foundPT_pair_sinAlpha;
  float _foundPT_pair_rInt_beam;
  float _foundPT_pair_sinAlpha_beam;
	float _foundPT_pair_DcaZ;

  TTree *_found_tracks_tree;
  int _foundTT_track_id;
  int _foundTT_arm;
  std::vector<float> *_foundTT_pair_rInt;
  std::vector<float> *_foundTT_pair_sinAlpha;
  std::vector<int> *_foundTT_pair_id;


  // ----------------------------------

  ///////////Counters//////////
  int _hitId;  //! Hit Id counter, for assigning internal Id to the hits
  int _trackId; //! Track Id counter, same idea

};




namespace myspace{ // namespace for my utility templates

  //Comparison structs
  struct EVPairVecSize :
    public std::binary_function<EVPairVec,EVPairVec,bool> {
    bool operator()(const EVPairVec& h1, const EVPairVec& h2) const 
    {
      //return h1.size() > h2.size();
			//Sanghoon
			//in order to have consistent sequence of sorted results,
			//add another condition with radius
			//to find tracks from outer radius (lower occupancy)
			unsigned int s1 = h1.size();
			unsigned int s2 = h2.size();
			if ( s1==s2 ){
				return (h1[0]->rInt) > (h2[0]->rInt);
			}else{
				return s1 > s2;
			}
    }
  };

  //Comparison structs
  struct EVHitLessZ :
    public std::binary_function<EVHit_ptr,EVHit_ptr,bool> {
    bool operator()(const EVHit_ptr& h1, const EVHit_ptr& h2) const 
    {
      if(h1->get_z() == h2->get_z()) return h1->get_phi() < h2->get_phi();
      return h1->get_z() < h2->get_z();
    }
    bool operator()(const EVHit_ptr& h, const double val) const
    {
      return h->get_z() < val;
    }
  };

  struct EVHitEqualZ :
    public std::binary_function<EVHit_ptr,EVHit_ptr,bool> {
    bool operator()(const EVHit_ptr& h1, const EVHit_ptr& h2) const 
    {
      return h1->get_z() == h2->get_z();
    }
    bool operator()(const EVHit_ptr& h, const double val) const
    {
      return h->get_z() == val;
    }
  };

  struct EVHitEqualXYZ :
    public std::binary_function<EVHit_ptr,EVHit_ptr,bool> {
    bool operator()(const EVHit_ptr& h1, const EVHit_ptr& h2) const 
    {
      return h1->get_z() == h2->get_z() && h1->get_y() == h2->get_y() && h1->get_x() == h2->get_x();
    }
  };
  
  struct EVHitGreaterZ :
    public std::binary_function<EVHit_ptr,EVHit_ptr,bool> {
    bool operator()(const EVHit_ptr& h1, const EVHit_ptr& h2) const 
    {
      if(h1->get_z() == h2->get_z()) return h1->get_phi() > h2->get_phi();
      return h1->get_z() > h2->get_z();
    }
    bool operator()(const EVHit_ptr& h, const double val) const
    {
      return h->get_z() > val;
    }
  };
  
  struct EVHitLessR :
    public std::binary_function<EVHit_ptr,EVHit_ptr,bool> {
    bool operator()(const EVHit_ptr& h1, const EVHit_ptr& h2) const 
    {
      if(h1->get_r() == h2->get_r()) return h1->get_phi() < h2->get_phi();
      return h1->get_r() < h2->get_r();
    }
    bool operator()(const EVHit_ptr& h, const double val) const
    {
      return h->get_r() < val;
    }
  };

  struct EVHitEqualR :
    public std::binary_function<EVHit_ptr,EVHit_ptr,bool> {
    bool operator()(const EVHit_ptr& h1, const EVHit_ptr& h2) const 
    {
      return h1->get_r() == h2->get_r();
    }
    bool operator()(const EVHit_ptr& h, const double val) const
    {
      return h->get_r() == val;
    }
  };

  struct EVHitEqualRPhi :
    public std::binary_function<EVHit_ptr,EVHit_ptr,bool> {
    bool operator()(const EVHit_ptr& h1, const EVHit_ptr& h2) const 
    {
      return h1->get_r() == h2->get_r() && h1->get_phi() == h2->get_phi();
    }
    bool operator()(const EVHit_ptr& h, const double rVal, const double phiVal) const
    {
      return h->get_r() == rVal && h->get_phi() == phiVal;
    }
  };

  struct EVHitGreaterR :
    public std::binary_function<EVHit_ptr,EVHit_ptr,bool> {
    bool operator()(const EVHit_ptr& h1, const EVHit_ptr& h2) const 
    {
      if(h1->get_r() == h2->get_r()) return h1->get_z() > h2->get_z();
      return h1->get_r() > h2->get_r();
    }
    bool operator()(const EVHit_ptr& h, const double val) const
    {
      return h->get_r() > val;
    }
  };
  
  struct EVHoughLessR :
    public std::binary_function<EVPair_ptr,EVPair_ptr,bool> {
    bool operator()(const EVPair_ptr& h1, const EVPair_ptr& h2) const 
    {
      if(h1->rInt == h2->rInt) return h1->sinAlpha < h2->sinAlpha;
      return h1->rInt < h2->rInt;
    }
    bool operator()(const EVPair_ptr& h, const double val) const
    {
      return h->rInt < val;
    }
  };

  struct EVHoughSimR :
    public std::binary_function<EVPair_ptr,EVPair_ptr,bool> {
    bool operator()(const EVPair_ptr& h1, const EVPair_ptr& h2) const 
    {
      return fabs(h1->rInt - h2->rInt) < 0.1;
    }
    bool operator()(const EVPair_ptr& h, const double val) const
    {
      return fabs(h->rInt - val) < 0.1;
    }
    bool operator()(const double val, const EVPair_ptr& h) const
    {
      return fabs(h->rInt - val) >= 0.1; 
    }   
  };
  
  struct EVHoughLessAlpha :
    public std::binary_function<EVPair_ptr,EVPair_ptr,bool> {
    bool operator()(const EVPair_ptr& h1, const EVPair_ptr& h2) const 
    {
      if(h1->sinAlpha == h2->sinAlpha) return h1->rInt < h2->rInt;
      return h1->sinAlpha < h2->sinAlpha;
    }
    bool operator()(const EVPair_ptr& h, const double val) const
    {
      return h->sinAlpha < val;
    }
  };

  struct EVHoughSimAlpha :
    public std::binary_function<EVPair_ptr,EVPair_ptr,bool> {
    bool operator()(const EVPair_ptr& h1, const EVPair_ptr& h2) const 
    {
      return fabs(h1->sinAlpha - h2->sinAlpha) < 0.01;
    }
    bool operator()(const EVPair_ptr& h, const double val) const
    {
      return fabs(h->sinAlpha - val) < 0.01;
    }
    bool operator()(const double val, const EVPair_ptr& h) const
    {
      return fabs(h->sinAlpha - val) >= 0.1; 
    }   
  };


  struct EVHoughEqualRAlpha :
    public std::binary_function<EVPair_ptr,EVPair_ptr,bool> {
    bool operator()(const EVPair_ptr& h1, const EVPair_ptr& h2) const 
    {
      return  h1->sinAlpha == h2->sinAlpha && h1->rInt == h2->rInt;
    }
    bool operator()(EVPair_ptr& h1,EVPair_ptr& h2) const 
    {
      return  h1->sinAlpha == h2->sinAlpha && h1->rInt == h2->rInt;
    }
  };


  struct rIntSum :
    public std::binary_function<double,EVPair_ptr,double> {
    double operator()(double& value, const EVPair_ptr& p)
    {
      return value + p->rInt;
    }
  };

  struct sinAlphaSum :
    public std::binary_function<double,EVPair_ptr,double> {
    double operator()(double& value, const EVPair_ptr& p)
    {
      return value + p->sinAlpha;
    }
  };
  
  struct EVHoughLessRandAlpha :
    public std::binary_function<EVPair_ptr,EVPair_ptr,bool> {
    bool operator()(const EVPair_ptr& h1, const EVPair_ptr& h2) const 
    {
      if ( fabs(h1->rInt - h2->rInt) < 0.1 ) // Transform pairs have similar rInt
        return h1->sinAlpha < h2->sinAlpha; // So sort by sinAlpha
      else
        return h1->rInt < h2->rInt; // Else sort by rInt
    }
  };


  struct NotPixelPair :
    public std::unary_function<EVPair_ptr,bool> {
    bool operator()(const EVPair_ptr& h) const 
    {
      return !((h->getFirstHitPtr()->getLayer() == 0
                && h->getSecondHitPtr()->getLayer() == 1)
               ||
               (h->getFirstHitPtr()->getLayer() == 1
                && h->getSecondHitPtr()->getLayer() == 0));
    }
  };


  struct numOverlapHits :
    public std::binary_function<EVHit_ptr,EVHitVec,int> {
    int operator()(const EVHit_ptr& h,
                   const EVHitVec& v,
                   const double _dPhi) const
    {
      return std::count_if(v.begin(),
                           v.end(),
                           boost::bind<bool>(FVTXOO::AngleOverlap<double>(),
					     boost::bind(&EVHit::getPhiStart,_1),
					     boost::bind(&EVHit::getPhiEnd,_1),
					     h->getPhiStart()-_dPhi,
					     h->getPhiEnd()+_dPhi) &&
                           boost::bind(std::not_equal_to<EVHit_ptr>(),h,_1));
    }
  };


  /////////////////////////////////////////////////////////////////////////////////


  /// Shorthand for squaring a number
  template<typename T> inline
  T SQ(const T& x) { return x*x; }

  /// Shorthand for getting the sign of a number
  template <typename T> inline
  int sgn(T val) { return (T(0) < val) - (val < T(0)); }


  /// Utility to all STL access to delete
  template<typename T>
  struct Deleter:
      public std::unary_function<void,T>
  {
    void operator()(const T v) { delete v; }
  };

  /// Utility to all nested STL access to delete
  template<typename T>
  struct DeleteEach:
      public std::unary_function<void,T>
  {
    void operator()(T& v)
    {
      std::for_each(v.begin(),v.end(),
                    Deleter<typename T::value_type>());
    }
  };

  /// Utility template for clearing STL containers: good for use with for_each
  template<typename T>
  struct Clearer:
      public std::unary_function<void,T>
  {
    void operator()(T& v) { v.clear(); }
  };

  /// Utility to all nested STL access to clearstart
  template<typename T>
  struct ClearEach:
      public std::unary_function<void,T>
  {
    void operator()(T& v)
    {
      std::for_each(v.begin(),v.end(),
                    Clearer<typename T::value_type>());
    }
  };

  template<typename T>
  struct Dereference:
      public std::unary_function<T,T*>
  {
    T operator()(T* p) { return *p; }
  };


  /// Flexible template for comparing two STL pairs by their second member and with a 
  /// user selectable predicate: defaults to std::less<U>
  template<typename T, typename U, typename V = std::less<U> >
  struct Pair2ndComp:
      public std::binary_function<std::pair<T,U>,
                                  std::pair<T,U>,
                                  bool>
  {
    bool operator()(const std::pair<T,U>& p1,
                    const std::pair<T,U>& p2,
                    V v = V() ) const
    {
      return v(p1.second, p2.second);
    }
    bool operator()(const std::pair<T,U>& p1,
                    const double val,
                    V v = V() ) const
    {
      return v(p1.second, val);
    }
  };

  // Use with std::accumulate to find the size of containers within a container
  template<typename T>
  struct addSize : 
      public std::binary_function<int,T,int>
  {
    int operator()(int & size, const T& v) 
    {
      return size + v->size();
    }
  };

  template< typename Inputlterator, typename Outputlterator, typename Predicate>
  Outputlterator copy_if( Inputlterator begin, Inputlterator end,
                          Outputlterator destBegin, Predicate p) 
  {
    while( begin != end ) {
      if( p(*begin) ) *destBegin++ = *begin;
      ++begin;
    }
    return destBegin;
  }
}



#endif // __MFVTXFINDHOUGHTRACKS_H__
