#ifndef PHPyDimuonTrigger_h
#define PHPyDimuonTrigger_h

#include <vector>
#include <set>

#include <TTree.h>
#include <TFile.h>
#include <PHPyVertexShift.h>
#include <TMCParticle.h>
#include "PHPyTrigger.h"
#include "PHPyVertexShift.h"
#include "TH1F.h"

//!  trigger module to select events with a dimuon pair
//! output ntuple will have all single muon information which can form dimuon(s)
class PHPyDimuonTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyDimuonTrigger( const std::string& name = "PHPyDimuonTrigger" );
  
  //! destructor 
  virtual ~PHPyDimuonTrigger( void );
  
  //! mode
  enum Mode
  {
    //4*pi acceptance
    All = 1<<0,
    //! south arm
    South = 1<<1, 
    //! north arm
    North = 1<<2,
    //! all
    MUON_ARM = North|South    
  };

  enum ParentMode
  {
    DirectParent = 0,
    Ancestor = 1,
    NotRequired = 2
  };

  //! set selection on direct parents or ancestors
  void set_parent_mode( ParentMode mode ) { _parent_mode = mode; }  
    
  //! set arm selection
  /*! value must be bitwise or of the above */
  void set_arm_selection( unsigned int value )
  { _mode = value; }
  
  //! minimum muon pT (GeV/c)
  void set_muon_pt_min( double value )
  { _muon_pt_min = value; }
  
  //! run initialization
  virtual int Init( PHCompositeNode* );
  
  //! event method
  int process_event(PHCompositeNode* );
  
  //! end of job
  int End(PHCompositeNode* ); 
  
  //! normalization ntuple
  void set_trigger_filename( const char* file )
  { if( file ) _trigger_filename = file; }
  
  // set vertex distribution histogram
  // if histogram is not set, it will use vertex 0
  void use_real_vertex( bool b)
   { _use_vtx = b; }

  void set_vertex_hist( TH1F* h)
  { _vertex_hist = (TH1F*) h->Clone("h_vtx"); }

  void reset_variables();

  void set_verbosity(bool v){ _verbosity = v; }

  double square(double d){ return d*d; }

  void set_accepted_parents(const std::vector<int>& parIn);

  void set_accepted_particles(const std::vector<int>& parIn);

  //! Provide an overloaded interface for ease of use in ROOT
  void set_accepted_parents(int *parIn, int npar);
  void set_unacceptable_parents(int *parIn, int npar);
  void set_accepted_particles(int *parIn, int npar);

  //! Minimum single muon pz
  void set_minimum_pz(double pz) { _min_pz = pz ;}

  protected:
  
  //! true if open charm(beauty) is found in final state that enters muon arm acceptance
  bool dimuon_trigger( PHPythiaContainer* );

  //! true if muon is found in given muon arm
  bool muon_in_muon_arm( const TLorentzVector&, Mode );
  
  //! true if the parent particle is accepted
  bool accepted_parent(PHPythiaContainer* phpylist,
                       TMCParticle* part);

  //! true if the particle is in accepted list
  bool accepted_particle(TMCParticle* part);
  
  //! muon ntuple
  void fill_muon_tree( void );

  //! book muon ntuple
  void book_muon_tree( void );

  bool _use_vtx;

  //vertex z distribution
  TH1F* _vertex_hist; 

  //! mode
  unsigned int _mode;
  
  double _muon_pt_min;

  //! Mimumum allowed single muon pz
  double _min_pz;  

  //! true if only specified parents are accepted
  bool _cut_on_parent;

  //! Choose if we want direct parents or just ancestors
  ParentMode _parent_mode;

  //! Hold a list of accepted parents
  std::set<int> _accepted_parents;
  std::set<int> _unacceptable_parents;
  
  //! Hold a list of accepted particles
  std::set<int> _accepted_particles;

  //verbosity
  bool _verbosity;

  //! muon file 
  std::string _trigger_filename;
  
  //! trigger file
  TFile* _trigger_tfile;
  
  //vector for muons
  std::vector<TMCParticle> muon_container;
  
  //! trigger tree
  TTree* _muon_tree;

  int ntriggered;
  int nconsidered;

  //!@name muon ntuple variables
  //@{
  //event level
  int _eventnumber;
  double _Evt_bbcZ;
  int _num_muons;

  //dimuon variables
  double mass;
  int charge;
  double E;
  double rapidity;
  double Px;
  double Py;
  double Pz;
  double Pt;
  
  // first muon
  int    Tr0_kF;
  double Tr0_Px;
  double Tr0_Py;
  double Tr0_Pz;
  double Tr0_E;
  double Tr0_pT;
  double Tr0_rapidity;
  int    Tr0_ancKF;
  char   Tr0_history[128];  
  double Tr0_Vx;
  double Tr0_Vy;
  double Tr0_Vz;
  int    Tr0_parentKF;
  int    Tr0_parentKS;
  double Tr0_parentPx;
  double Tr0_parentPy;
  double Tr0_parentPz;
  double Tr0_parentE;
  double Tr0_parentPt;
  double Tr0_parentRap;
  int    Tr0_ancKS;
  double Tr0_ancPx;
  double Tr0_ancPy;
  double Tr0_ancPz;
  double Tr0_ancE;
  double Tr0_ancPt;
  double Tr0_ancRap; 

 

  //second muon
  int Tr1_kF;
  double Tr1_Px;
  double Tr1_Py;
  double Tr1_Pz;
  double Tr1_E;
  double Tr1_pT;
  double Tr1_rapidity;
  int    Tr1_ancKF;
  char   Tr1_history[128];
  double Tr1_Vx;
  double Tr1_Vy;
  double Tr1_Vz;
  int    Tr1_parentKF;
  int    Tr1_parentKS;
  double Tr1_parentPx;
  double Tr1_parentPy;
  double Tr1_parentPz;
  double Tr1_parentE;
  double Tr1_parentPt;
  double Tr1_parentRap;
  int    Tr1_ancKS;
  double Tr1_ancPx;
  double Tr1_ancPy;
  double Tr1_ancPz;
  double Tr1_ancE;
  double Tr1_ancPt;
  double Tr1_ancRap;
 

};


#endif
