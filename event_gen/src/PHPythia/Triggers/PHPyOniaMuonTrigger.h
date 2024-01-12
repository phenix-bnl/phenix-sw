#ifndef PHPyOniaMuonTrigger_h
#define PHPyOniaMuonTrigger_h

// $Id: PHPyOniaMuonTrigger.h,v 1.6 2011/12/15 03:48:59 kblee Exp $

#include <TTree.h>
#include <TFile.h>
#include <PHPyVertexShift.h>

#include "PHPyTrigger.h"
#include "PHPyVertexShift.h"

/*!
   \file PHPyOniaMuonTrigger.h
   \brief trigger module to select events that contains a J/Psi in the muon arm acceptance
   \author Hugo Pereira
   \version $Revision: 1.6 $
   \date $Date: 2011/12/15 03:48:59 $
*/

//!  trigger module to select events that contains a J/Psi in the muon arm acceptance
class PHPyOniaMuonTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyOniaMuonTrigger( const std::string& name = "PHPyOniaMuonTrigger" );
  
  //! destructor 
  virtual ~PHPyOniaMuonTrigger( void );
  
  //! mode
  enum Arm
  {
    //! south arm
    South = 1<<0,
    
    //! north arm
    North = 1<<1,
    
    //! back-to-back
    BackToBack = 1<<2,
    
    //! all
    All = North|South|BackToBack
    
  };
    
  //! set arm selection
  /*! value must be bitwise or of the above */
  void set_arm_selection( unsigned int value )
  { _mode = value; }
  
  //! minimum pT (GeV/c)
  void set_pt_min( double value )
  { _pt_min = value; }
  
  //! parent particle type
  enum ParentType
  {
    JPsi = 1<< 0,
    PsiPrime = 1 <<1,
    Upsilon1S = 1<<2,
    Upsilon2S = 1<<3,
    Upsilon3S = 1<<4,
    Z = 1<<5,
    Psi = JPsi | PsiPrime,
    Upsilon = Upsilon1S | Upsilon2S | Upsilon3S,
    AllOnia = Psi|Upsilon
  };
  
  //! select parent particle
  /*! must be a bitwise or of the above */
  void set_parent_selection( unsigned int parent )
  { _parent = parent; }
  
  //!@name Methods Derived from SubsysReco
  //@{
  
  //! run initialization
  virtual int Init( PHCompositeNode* );
  
  //! event method
  int process_event(PHCompositeNode* );
  
  //! end of job
  int End(PHCompositeNode* ); 
  
  //@}
  
  //! normalization ntuple
  void set_normalization_filename( const char* file )
  { if( file ) _normalization_file = file; }
  
  //! normalization tree filling
  void set_fill_normalization_tree( bool value )
  { _fill_normalization_tree = value; }
  
  //! set to true to use internal vertex shift module
  /*! 
  this is needed so that vertex used in the output also appears in the normalisation ntuple
  however, it might conflict with possible vertex shift used in the macro 
  */
  void set_use_internal_vertex_shift( bool value )
  { _use_internal_vertex_shift = value; }

  void set_chi_c( bool value )
  { _add_chi_c_photon = value; }

  //! pointer to internal vertex shift
  PHPyVertexShift* vertex_shift_module( void )
  { return _vertex_shift; }
  
  protected:
  
  //! true if parent particle is accepted
  bool accept_parent( int ) const;
  
  //! true if J/psi is found in final state that enters muon arm acceptance
  bool onia_in_muon_arm( PHPythiaContainer* );
  
  //! true if muon is found in given muon arm
  bool muon_in_muon_arm( const TLorentzVector&, Arm );
  
  //! normalization ntuple
  void fill_normalization_tree( void );

  //! book normalization ntuple
  void book_normalization_tree( void );

  //! mode
  unsigned int _mode;
  
  //! parent particle
  unsigned int _parent;
  
  //! minimum pT
  /*! used to only select particles produced with a minimum pT */
  double _pt_min;
  
  //! normalization file 
  std::string _normalization_file;
  
  //! normalization file
  TFile* _normalization_tfile;
  
  //! fill_normalization_tree
  bool _fill_normalization_tree;
  
  //! internal vertex shift
  bool _use_internal_vertex_shift; 

  //! chi_c flag to select Chi_c photon 
  bool _add_chi_c_photon;

  //! normalization tree
  TTree* _normalization_tree;
  
  //!@name normalization ntuple variables
  //@{
  double _Px;
  double _Py;
  double _Pz;
  double _E;
  double _pT;
  double _rapidity;

  //!@can be added for Chi_c photon
  double _gPx;
  double _gPy;
  double _gPz;
  double _gE;
  double _gpT;
  double _grapidity;

  //! vertex position
  double _Evt_bbcZ;
  
  //! centrality. This variable makes sense only if internal vertex shift is used
  double _Centrality;
  
  //! true if event is accepted
  int _accepted;
  
  //@}
  
  //! internal vertex shift module
  PHPyVertexShift* _vertex_shift;  
};

#endif
