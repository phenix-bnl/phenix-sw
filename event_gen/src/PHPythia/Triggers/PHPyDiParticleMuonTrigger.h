#ifndef PHPyDiParticleMuonTrigger_h
#define PHPyDiParticleMuonTrigger_h

// $Id: PHPyDiParticleMuonTrigger.h,v 1.4 2009/05/27 21:27:54 youzy Exp $

#include <TTree.h>
#include <TFile.h>
#include <PHPyVertexShift.h>
#include <vector>

#include "PHPyTrigger.h"
#include "PHPyVertexShift.h"

/*!
   \file PHPyDiParticleMuonTrigger.h
   \brief trigger module to select events that contains two required particles in the muon arm acceptance
   \author Zhengyun You
   \version $Revision: 1.4 $
   \date $Date: 2009/05/27 21:27:54 $
*/

//!  trigger module to select events that contains a J/Psi in the muon arm acceptance
class PHPyDiParticleMuonTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyDiParticleMuonTrigger( const std::string& name = "PHPyDiParticleMuonTrigger" );
  
  //! destructor 
  virtual ~PHPyDiParticleMuonTrigger( void );
  
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
  void SetPtMin( double value )
  { _pt_min = value; }

  //! minimum p (GeV/c)
  void SetPMin( double value )
  { _p_min = value; }  

  //! set number of particles required at least in event.
  void SetNPartMin( int value)
  { _npart_min = value; }

  // Add the particle to be selected
  virtual void AddParticleChoice( int particle_id );

  virtual void AddParticleChoiceAbs( int particle_id );
  
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

  //! pointer to internal vertex shift
  PHPyVertexShift* vertex_shift_module( void )
  { return _vertex_shift; }
  
  protected:

  //! true if two required paritcles are found in give muon arm
  bool diparticle_in_muon_arm( PHPythiaContainer* ); 
  
  //! true if muon is found in given muon arm
  bool muon_in_muon_arm( const TLorentzVector&, Arm );
  
  //! normalization ntuple
  void fill_normalization_tree( void );

  //! book normalization ntuple
  void book_normalization_tree( void );

  //! mode
  unsigned int _mode;
  
  //! minimum pT
  /*! used to only select particles produced with a minimum pT */
  double _pt_min;

  //! minimum p
  /*! used to only select particles produced with a minimum p */
  double _p_min;

  //! number of particles minimum
  int _npart_min;
  
  //! normalization file 
  std::string _normalization_file;
  
  //! normalization file
  TFile* _normalization_tfile;
  
  //! fill_normalization_tree
  bool _fill_normalization_tree;
  
  //! internal vertex shift
  bool _use_internal_vertex_shift; 
  
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
  
  //! vertex position
  double _Evt_bbcZ;
  
  //! centrality. This variable makes sense only if internal vertex shift is used
  double _Centrality;
  
  //! true if event is accepted
  int _accepted;
  
  //@}
  
  //! internal vertex shift module
  PHPyVertexShift* _vertex_shift;
 
  std::vector<int> _particle_choice;
 
};

#endif
