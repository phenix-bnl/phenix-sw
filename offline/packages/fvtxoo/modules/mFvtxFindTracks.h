#ifndef __MFVTXFINDTRACKS_H__
#define __MFVTXFINDTRACKS_H__

#include <iostream>
#include <set>
#include <vector>
#include <map>
#include <stdexcept>
#include <cmath>
#include <numeric>
#include <TObject.h>
#include <PHTimer.h>



#ifndef __CINT__
#include <boost/smart_ptr.hpp>
#endif


#include <mFvtxModuleBase.h>

#define NARMS 2

#define NSVX_LAYERS 4
#define NFVTX_LAYERS 4

#define NPASSES 4
#define NPASSES_DR 2

class PHCompositeNode;
class TMutMCTrkMap;
class TFvtxHitMap;
class TFvtxTrkMap;
class TFvtxMCHitMap;
class TFvtxCoordMap;
class TFvtxSvxClusterMap;
class mFvtxFindTrackPar;
class TTree;

class mFvtxFindTracks : public mFvtxModuleBase
{
 public:

  class EVHit
  {
  public:
    EVHit();
    virtual ~EVHit() {}
    
    std::ostream& print(std::ostream& os = std::cout) const;
    
    int id;
    int event;
    int layer; // Used only for VTX hits
    int arm;
    int cage;
    int station;
    int sector;
    int column;
    int halfwedge;
    int mc_hitnum;   // id of mc hit associated with this hit
    int mc_tracknum; // id of mc track that generated this hit
    int coordnum;     // id of cluster this hit represents (FVTX)
    int svx_hitnum;  // id of cluster this hit represents (VTX)
    double r_hit;
    double phi_hit;
    double x_hit;
    double y_hit;
    double z_hit;
    double phi_start;
    double phi_end;
    int ntracks;  // Number of tracks that use this hit
    std::vector<int> trackNum; // track ids that use this hit
    double px;
    double py;
    double pz;
    double ptot;

    int getLayer() const { return layer; }
    int getCage() const { return cage; }
    int getSector() const { return sector; }
    int getStation() const { return station; }
    int getColumn() const { return column; }
    int getHalfWedgeId() const { return halfwedge; }

    double get_x() const { return x_hit; }
    double get_y() const { return y_hit; }
    double get_z() const { return z_hit; }
    double get_r() const { return r_hit; }
    double get_phi() const { return phi_hit; }
    double getPhiStart() const { return phi_start; }
    double getPhiEnd() const { return phi_end; }
    int getNtracks() const { return ntracks; }

    void* getMCHitPtr() const { return _mchit_ptr; }
    void* getCoordPtr() const { return _coord_ptr; }
    void* getSvxClusPtr() const { return _svx_ptr; }

    void setMCHitPtr(void* p) { _mchit_ptr = p; }
    void setCoordPtr(void* p) { _coord_ptr = p; }
    void setSvxPtr(void* p) { _svx_ptr = p; }

  private:

    // These are void* in order to keep this class decoupled from 
    // the phenix code.  The user needs to cast them back the appropriate
    // mutoo framework type in order to use them.
    //
    void* _mchit_ptr; //! opaque pointer to the TFvtxMCHit for this EVHit
    void* _coord_ptr; //! opaque pointer to the TFvtxCoord for this EVHit
    void* _svx_ptr; //! opaque pointer to the TFvtxSvxCluster for this EVHit

  };



#ifndef __CINT__
  typedef boost::shared_ptr<EVHit> EVHit_ptr;
#else
  typedef EVHit * EVHit_ptr;
#endif

  class EVTrk
  {
  public:
    EVTrk();
    virtual ~EVTrk();

    int event;                      // Event number
    int track;                      // Id of this track
    int trk_arm;                    // Arm assigned to the track
    double chi_2;
    int nstationsHit;               // Number of stations hit
    int nlayersHit;                 // Number of VTX layers hit
    int size;                       // Number of hits
    std::vector<int> hitnum;        // Id of EVHit associated with this EVTrk
    std::vector<int> mc_hitnum;     // MC id of geant hit corresponding to the EVHit
    std::vector<int> mc_track_id;   // MC track id of track that generated the MC hit
    std::vector<int> coord_id;      // coord id of track that generated the MC hit
    std::vector<int> svx_hitnum;    // coord id from VTX
    std::vector<int> layer;         // Layers of VTX hits
    std::vector<int> arm;           // Arms of hits
    std::vector<int> cage;          // Cages of hits
    std::vector<int> station;       // Stations of hits 
    std::vector<int> sector;        // Sectors of hits
    std::vector<int> halfwedge;     // Halfwedge of hits
    std::vector<double> r;          // R of hits
    std::vector<double> phi;        // Phi of hits
    std::vector<double> zed;        // Z of hits
  
    unsigned int nshared; // number of hits shared with other tracks

    double r_slope;     // slope of straight line in r-projection
    double r_offset;    // offset of straight line in r-projection
    double phi_slope;   // slope of straight line in phi-projection
    double phi_offset;  // offset of straight line in phi-projection

    double r_slopeFit;     // Fitted slope of straight line in r-projection
    double r_offsetFit;    // Fitted offset of straight line in r-projection
    double r_chi2;         // Chi2 of R-Z linear fit

    std::vector<double> r_resid;   // Road R residual associated with the EVHit
    std::vector<double> phi_resid; // Road Phi residual associated with the EVHit

    std::vector<double> r_residFit;   // Fit R residual associated with the EVHit
    std::vector<double> phi_residFit; // Fit Phi residual associated with the EVHit

    // The hits that make up this track
    std::vector<EVHit_ptr> hits;      //->

    std::set<int> _hitIds;         //! Internal bookkeeping for duplicate rejection

    unsigned int getNumHits() const { return hits.size(); }
    unsigned int getNShared() const; 
    unsigned int getNStaHit() const { 
      unsigned int nStaHit[NFVTX_LAYERS] = { 0 };
      for (unsigned int i=0; i<hits.size(); i++) nStaHit[hits[i]->getStation()] = 1;
      return std::accumulate(nStaHit,nStaHit+NFVTX_LAYERS,0); 
    }
    unsigned int getNLayHit() const {
      unsigned int nLayHit[NSVX_LAYERS] = { 0 };
      for (unsigned int i=0; i<hits.size(); i++) nLayHit[hits[i]->getLayer()] = 1;
      return std::accumulate(nLayHit,nLayHit+NSVX_LAYERS,0);
    }

    // Methods to access the underlying hits' properties
    int get_hitid(const int i) const { return hits.at(i)->id; }
    int get_mc_hitnum(const int i) const { return hits.at(i)->mc_hitnum; }
    int get_mc_track_id(const int i) const { return hits.at(i)->mc_tracknum; }
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

    double getPhiStart() const { return hits.front()->phi_start; }
    double getPhiEnd() const { return hits.front()->phi_end; }

    // Get a raw pointer to an EVHit
    const EVHit_ptr getHitPtr(const int i) const { return hits.at(i); }

    // Get an opaque pointer to the underlying MCHit (or Coord)
    //
    void* getMCHitPtr(const int i) const { return hits.at(i)->getMCHitPtr(); }
    void* getCoordPtr(const int i) const { return hits.at(i)->getMCHitPtr(); }

    void sortHitsZ();

    // Use the current hits to calculate residuals
    void calculateResids();

    void calculateNShared();

    void linearFit();

    void clear();

    // Use with caution! Deletes hits, which exposes the user to a double delete...
    void deleteHits() {
//      for (std::vector<EVHit_ptr>::iterator i=hits.begin(); i!=hits.end(); i++) delete *i;
      hits.clear();
    }

    void addHit(EVHit_ptr h);      // Add a hit to this track
    void addHitNoRef(EVHit_ptr h); // Add a hit to this track, but don't increment the ref count
    void removeHit(EVHit_ptr h);      // Remove this hit
    void removeHitNoRef(EVHit_ptr h); // Remove this hit, but don't decrement the ref count
    void prepare();

  };

#ifndef __CINT__
  typedef boost::shared_ptr<EVTrk> EVTrk_ptr;
#else
  typedef EVTrk * EVTrk_ptr;
#endif

  typedef std::vector<EVHit_ptr> EVHitVec;
  typedef std::vector<EVTrk_ptr> EVTrkVec;

  mFvtxFindTracks();
  virtual ~mFvtxFindTracks();
  
  void init(PHCompositeNode* top_node);
  void init_run(PHCompositeNode* top_node) {}
  void end(PHCompositeNode* top_node);

  // Public event processor
  PHBoolean event(PHCompositeNode* topNode);

  void setMode(unsigned int m)
  {
    if ( m > 1 ) throw std::runtime_error("Invalid mode in mFvtxFindTracks::setMode");
    _mode = m; 
    std::cout << "mFvtxFindTracks::setMode: mode is now " << _mode << std::endl;
  }

  //! set using svx cluster
  void set_use_svx_cluster(bool flag) { _use_svx_cluster = flag;}

  //! set number of svx layers to use
  void set_n_svx_layers(int nlay) { _n_svx_layers = nlay; }

  // Set the name of the eval tree output file
  void set_file_name(std::string name) { _evalFilename = name; }

  void do_evaluation(bool v) { _do_evaluation = v; }

  void set_r_window(double dr) { _dR = dr; }
  void set_phi_window(double dphi ) { _dPhi = dphi; }

  // Receive a vector of hits.  Useful for cases where the hits have 
  // been saved to a user file.
  void set_hits(const EVHitVec& v) {
    _hit_vec.clear();
    _hit_vec.insert(_hit_vec.end(),v.begin(),v.end());
  }

  // Copy (actually, append) the hits to a vector
  void get_hits(EVHitVec& v) const { 
    v.insert(v.end(),_hit_vec.begin(),_hit_vec.end());
  }
  
  // Copy (actually, append) the tracks to a vector
  void get_tracks(EVTrkVec& v) const {
    v.insert(v.end(),_trk_vec.begin(),_trk_vec.end());
  }

  std::pair<double,double> getPhiStartEnd(int arm, int cage, int station, int sector, int column) const;

private:
  class TrackEvaluator {
  public:
    virtual ~TrackEvaluator() {}
    virtual bool checkStartHit(const EVHit_ptr h) const = 0;
    virtual bool checkEndHit(const EVHit_ptr h) const = 0;
    //virtual bool checkHits(const EVHitVec& start, const EVHitVec& mid, const EVHitVec& end) const = 0;
    // Overload needed for the adaptable nMidStation version
    virtual bool checkHits(const int n_svx_layers, const int nMidStations, const EVHitVec& start, const EVHitVec& mid, const EVHitVec& end) const = 0;
  };

  class NoCuts : public TrackEvaluator
  {
  public:
    virtual ~NoCuts() {}
    virtual bool checkStartHit(const EVHit_ptr h) const { return true; }
    virtual bool checkEndHit(const EVHit_ptr h) const { return true; }
    virtual bool checkHits(const int n_svx_layers, const int nMidStations, const EVHitVec& start, const EVHitVec& mid, const EVHitVec& end) const { return true; }
  };

  class RequireNMidStations : public TrackEvaluator
  {
  public:
    virtual ~RequireNMidStations() {}
    virtual bool checkStartHit(const EVHit_ptr h) const { return true; }
    virtual bool checkEndHit(const EVHit_ptr h) const { return true; }
    virtual bool checkHits(const int n_svx_layers, const int nMidStations, const EVHitVec& start, const EVHitVec& mid, const EVHitVec& end) const;
  };

  class RequireNMidStationsUnusedSeeds : public RequireNMidStations
  {
  public:
    virtual ~RequireNMidStationsUnusedSeeds() {}
    virtual bool checkStartHit(const EVHit_ptr h) const { return h->ntracks==0; }
    virtual bool checkEndHit(const EVHit_ptr h) const { return h->ntracks==0; }
  };

#ifndef __CINT__
  template<typename T> struct Line1D : public std::unary_function<T,T> {
    T _m;
    T _b;
    Line1D(const T inM, const T inB) : _m(inM), _b(inB) {}
    T operator()(const T x) const { return _m * x + _b; }
  };
#endif // __CINT__

  //! retrieve pointer to needed nodes
  void set_interface_ptrs(PHCompositeNode* top_node);

//   // Internal event processor
//   void event();

  void clear();

  void convert_mchits();
  void convert_coords();

  void sort_hits();

  void find_tracks(int iarm);
  void find_nStation_tracks(int iarm, int nStations);

  template<typename T> int searchTracks(int arm, int staStart, int staEnd, int nStations);

  void removeNonOverlapCandidates(EVHitVec& v, EVHit_ptr ref);
  void removeDuplicates(EVTrkVec& v);
  void removeSimilar(EVTrkVec& v);

  //EVHitVec findHits(const int arm, const int station, const double phiStart, const double phiEnd);
  EVHitVec findHits(const EVHitVec& v, const double phiStart, const double phiEnd);
#ifndef __CINT__
  //EVHitVec findHits(const int arm, const int station, const Line1D<double>& lower, const Line1D<double>& upper);
  EVHitVec findHits(const EVHitVec& v, const Line1D<double>& lower, const Line1D<double>& upper);
#endif
  EVHitVec findHitsR(const EVHitVec& v, double rStart, double rEnd);
  EVHitVec findHitsDeltaR(const EVHitVec& v, double slope, double offset, double dR);

  //void promote_associations() const;
  void fillOutputNodes();
  void load_ext_vertex(PHCompositeNode* top_node);
  void set_trk_par();

  // For producing evaluation ntuples
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };
  void book_trees();
  void fill_trees();

  bool _do_evaluation;
  bool _use_svx_cluster;
  int  _n_svx_layers;
  std::string _evalFilename;

  int _mode;  // Mode (0==use mc hits, 1==use coords)
  double _dR;
  double _dPhi;

  //! Event z vertex position
  double _vertex_z;

  mFvtxFindTrackPar* _mod_par;

  //! MC hits
  TFvtxMCHitMap* _mc_hit_map; 

  //! tracks
  TFvtxTrkMap* _trk_map; 
  
  //! coordinates
  TFvtxCoordMap* _coord_map; 

  //! VTX clusters
  TFvtxSvxClusterMap* _svx_map;
  
  //! hits
  TFvtxHitMap* _hit_map; 
  //@}

  EVHitVec _hit_vec; // Hits to be processed
  EVTrkVec _trk_vec; // Track candidates

  // Hits, indexed by arm and station
  std::vector<EVHit_ptr> _hitsByStation[NARMS][NFVTX_LAYERS];
  // VTX hits, indexed by layer
  std::vector<EVHit_ptr> _hitsByLayer[NSVX_LAYERS];

  // Storage for currently unused hits by layer
  EVHitVec _unusedHits[NSVX_LAYERS+NFVTX_LAYERS];

  //! module timer
  PHTimer _timer;

  std::map<int,int> _nfound; // Stats of avg nfound per pass

  int _ievent;

  // Diagnostic evaluation trees
  std::vector<TTree*> _eval_trees;

  int _hitId;  // Hit Id counter, for assiging internal Id to the hits
  int _trackId; // Track Id counter, same idea

  double get_phiSlope(
		      double sta0phi, 
		      double sta0zed,
		      double sta3phi,
		      double sta3zed
		      );

};

#endif // __MFVTXFINDTRACKS_H__
