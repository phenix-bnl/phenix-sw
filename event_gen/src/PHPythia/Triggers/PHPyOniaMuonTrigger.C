// $Id: PHPyOniaMuonTrigger.C,v 1.8 2011/12/15 03:48:15 kblee Exp $

/*!
   \file PHPyOniaMuonTrigger.C
   \brief trigger module to select events that contains a J/Psi in the muon arm acceptance
   \author Hugo Pereira
   \version $Revision: 1.8 $
   \date $Date: 2011/12/15 03:48:15 $
*/

#include <assert.h>
#include <cmath>

#include <getClass.h>
#include <Fun4AllReturnCodes.h>
#include <TLorentzVector.h>

#if ROOT_VERSION_CODE >= ROOT_VERSION(5,15,8) 
#include <TMCParticle.h>
#else
#include <TMCParticle6.h>
#endif

#include "PHPyOniaMuonTrigger.h"
#include "PHPyTrigger.h"
#include "PHPythiaHeader.h"
#include "PHPythiaContainer.h"
#include "PHPyCommon.h"

using namespace std;

//___________________________________________________________________________
PHPyOniaMuonTrigger::PHPyOniaMuonTrigger(const string &name): 
  PHPyTrigger(name),
  _mode( All ),
  _parent( AllOnia ),
  _pt_min( 0 ),
  _normalization_file( "pythia.root" ),
  _normalization_tfile( 0 ),
  _fill_normalization_tree( true ),
  _use_internal_vertex_shift( true ),
  _add_chi_c_photon(false),
  _normalization_tree( 0 )
{ 
  // trigger type (?)
  trigger_type = MINBIAS;
  
  // create vertex shift
  _vertex_shift = new PHPyVertexShift( "INTERNAL_VERTEX_SHIFT" );
}

//___________________________________________________________________________
PHPyOniaMuonTrigger::~PHPyOniaMuonTrigger( void )
{ if( _vertex_shift ) delete _vertex_shift; }
  
//___________________________________________________________________________
int PHPyOniaMuonTrigger::Init(PHCompositeNode *top_node)
{
  

  cout << "PHPyOniaMuonTrigger::Init" << endl;
  
  cout << "_mode: " << _mode << endl;
  cout << "  south: " << (_mode&South ? "true":"false" ) << endl; 
  cout << "  north: " << (_mode&North ? "true":"false" ) << endl; 
  cout << "  back to back: " << (_mode&BackToBack ? "true":"false" ) << endl; 
  cout << "_pt_min: " << _pt_min << endl;
  
  cout << "_normalization_file: " << _normalization_file << endl;
  cout << "_fill_normalization_tree: " << _fill_normalization_tree << endl;
  cout << "_use_internal_vertex_shift: " << _use_internal_vertex_shift << endl;
  cout << "_add_chi_c_photon: "<< (_add_chi_c_photon ? "true":"false")<<endl;
 
  cout << "_parent: " << endl;
  cout << " jpsi: " << (accept_parent( PY_JPSI ) ? "true":"false" ) << endl;
  cout << "psiprime: " << (accept_parent( PY_PSIPRIME ) ? "true":"false" ) << endl;
  cout << " upsilon 1S: " << (accept_parent( PY_UPSILON1S ) ? "true":"false" ) << endl;
  cout << " upsilon 2S: " << (accept_parent( PY_UPSILON2S ) ? "true":"false" ) << endl;
 
  cout << " Drell-Yan" << (accept_parent( PY_Z ) ? "true":"false" ) << endl;

  if( _use_internal_vertex_shift ) return  _vertex_shift->Init( top_node );
  else return EVENT_OK;
  
} 
  
//___________________________________________________________________________
int PHPyOniaMuonTrigger::End(PHCompositeNode *top_node)
{

  // close TFile
  if( _fill_normalization_tree && _normalization_tfile )
  {
    _normalization_tfile->Write();
    _normalization_tfile->Close();
    delete _normalization_tfile;
  }
  
  // dump out trigger statistics
  cout << "PHPyOniaMuonTrigger::End - number of triggered events: " << ntriggered << "/" << nconsidered << endl;
  if ( nconsidered>0 ) cout << "PHPyOniaMuonTrigger::End - ratio: " << float(ntriggered)/nconsidered << endl;
  return EVENT_OK;
  
}

//___________________________________________________________________________
int PHPyOniaMuonTrigger::process_event(PHCompositeNode *top_node)
{
  
  static int counter(0);
  
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(top_node,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cout << "PHPyOniaMuonTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(top_node,"PHPythia");
  if (!phpythia)
  {
    cout << "PHPyOniaMuonTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  _Evt_bbcZ = -999;
  _Centrality = -999;
  
  // check if particles fullfil trigger conditions
  _accepted = onia_in_muon_arm(phpythia);
  
  // check pT range
  /* 
  if pT is not in range, the normalization ntuple is not filled and one returns directly.
  This is to minimize the size of the normalization file 
  */
  if( _pt_min > 0 && _pT < _pt_min )
  { 
    _accepted = false;
    return ABORTEVENT;
  }
  
  
  if( _accepted ) ++ntriggered;

  if( _accepted && _use_internal_vertex_shift )
  { 
    
    int out( _vertex_shift->process_event( top_node ) );
    if( !( out == EVENT_OK && _vertex_shift->event_valid() ) ) 
    {
    
      cout << "PHPyOniaMuonTrigger::process_event - event: " << ++counter << " vertex_shift event invalid" << endl;
      return out; 
    
    } else {
      
      _Evt_bbcZ = _vertex_shift->event_vertex_z();
      _Centrality = _vertex_shift->event_centrality();
      cout << "PHPyOniaMuonTrigger::process_event - event: " << ++counter << " _Evt_bbcZ: " << _Evt_bbcZ << endl;
      
    }
  }
  
  // fill normalization ntuple for all events
  // must be called after vertex_shift is called, so that accepted events have correct vertex and centrality
  if( _fill_normalization_tree ) fill_normalization_tree();
  
  // output
  return (_accepted) ? EVENT_OK:ABORTEVENT;
  
}

//_________________________________________________________________________
bool PHPyOniaMuonTrigger::accept_parent( int parent ) const
{
  
  if( (_parent & JPsi) && parent == PY_JPSI ) return true;
  if( (_parent & PsiPrime) && parent == PY_PSIPRIME ) return true;
  
  // upsilon 1S and 3S are handled the same, because there is no such particle as upsilon 3S
  // in pythia particle list. What is usually done is to use 1S and modify the mass
  if( (_parent & (Upsilon1S | Upsilon3S)) && parent == PY_UPSILON1S ) return true;
  if( (_parent & Upsilon2S) && parent == PY_UPSILON2S ) return true;

  if( (_parent & Z) && parent == PY_Z ) return true;  
  return false;
}

//_________________________________________________________________________
bool PHPyOniaMuonTrigger::onia_in_muon_arm(PHPythiaContainer *phpylist)
{
  
  // store momentum and flags whether or not muons have been found.
  TLorentzVector momentum_minus;
  TLorentzVector momentum_plus;
  TLorentzVector momentum_parent;
  TLorentzVector momentum_gamma; 

  bool found_minus = false;
  bool found_plus = false;
  bool found_parent = false;
   
  // loop over particles, store mu_plus and mu_minus candidate
  for( unsigned int ipart=0; ipart<phpylist->size(); ipart++)
  {
    
    TMCParticle *part = phpylist->getParticle(ipart);
    
    // check if partices are muons
    int kf = part->GetKF();
    
    // check chi_c photon if chi_c flag is on
    if(_add_chi_c_photon)
    {  
      if ( kf==PY_GAMMA )
      {
        TMCParticle *part3 =  phpylist->getParticle( part->GetParent()-1 );
        Int_t kfparent2 =  part3->GetKF();

        if( kfparent2 == PY_CHI0C0 || kfparent2 == PY_CHI1C0 || kfparent2 == PY_CHI2C0)
        {
        // get momentum and energy
        TLorentzVector momentum_gamma_local( part->GetPx(), part->GetPy(), part->GetPz(), part->GetEnergy());

        momentum_gamma = momentum_gamma_local;
        }
      }
    }
    if ( kf==PY_MU || kf==-PY_MU )
    {
      
      // get momentum and energy
      TLorentzVector momentum( part->GetPx(), part->GetPy(), part->GetPz(), part->GetEnergy());
      
      // THIS IS A PAIN, you have to subtract 1 from the line number of the parent!	  
      TMCParticle *part2 =  phpylist->getParticle( part->GetParent()-1 );
      Int_t kfparent =  part2->GetKF();
            
      if( accept_parent( kfparent ) )
      {
        // store parent momentum
        TLorentzVector momentum_parent_local( part2->GetPx(), part2->GetPy(), part2->GetPz(), part2->GetEnergy() ); 
      
        if( !found_parent )
        {
          
          found_parent = true;
          momentum_parent = momentum_parent_local;
          
        } else if( momentum_parent != momentum_parent_local ) {
          
          cout << "PHPyOniaMuonTrigger::onia_in_muon_arm - warning: multiple parents found." << endl;
          return false;
          
        }
      }       
      
      // negative muon
      if( accept_parent( kfparent ) && kf==PY_MU )
      { 
        if( found_minus ) 
        {
          
          cout << "PHPyOniaMuonTrigger::onia_in_muon_arm - warning: multiple negative muon candidates found." << endl;
          return false;
        
        }
        
        found_minus = true;
        momentum_minus = momentum;        
      }
        
      // negative muon
      if( accept_parent( kfparent ) && kf==-PY_MU )
      { 
        if( found_plus ) 
        {
          
          cout << "PHPyOniaMuonTrigger::onia_in_muon_arm - warning: multiple positive muon candidates found." << endl;
          return false;
        
        }
        
        found_plus = true;
        momentum_plus = momentum; 
      }
          
    }
    
  }
  
  // check if muons have been found
  if( !( found_minus && found_plus ) )
  { 
    cout << "PHPyOniaMuonTrigger::onia_in_muon_arm - warning: missing muon candidated." << endl;
    return false;
  }

  // check if parent has been found
  if( ! found_parent )
  {
    cout << "PHPyOniaMuonTrigger::onia_in_muon_arm - warning: no particle you intended to generate." << endl;
    return false;
  }
  
  // store relevant ntuple variables
  _Px = momentum_parent.Px();
  _Py = momentum_parent.Py();
  _Pz = momentum_parent.Pz();
  _E = momentum_parent.E();
  _pT = momentum_parent.Pt();
  _rapidity = momentum_parent.Rapidity();

  // check chi_c flag is on
  if(_add_chi_c_photon)
  {
  _gPx = momentum_gamma.Px();
  _gPy = momentum_gamma.Py();
  _gPz = momentum_gamma.Pz();
  _gE = momentum_gamma.E();
  _gpT = momentum_gamma.Pt();
  _grapidity = momentum_gamma.Rapidity();
  }

  // check momentum against selected mode
  if( ( _mode & South ) && muon_in_muon_arm( momentum_minus, South ) && muon_in_muon_arm( momentum_plus, South ) ) return true;
  if( ( _mode & North ) && muon_in_muon_arm( momentum_minus, North ) && muon_in_muon_arm( momentum_plus, North ) ) return true;
  if( ( _mode & BackToBack ) && (
    ( muon_in_muon_arm( momentum_minus, South ) && muon_in_muon_arm( momentum_plus, North ) ) ||
    ( muon_in_muon_arm( momentum_minus, North ) && muon_in_muon_arm( momentum_plus, South ) ) ) ) return true;
  
  return false;
    
}

//_________________________________________________________________________
bool PHPyOniaMuonTrigger::muon_in_muon_arm( const TLorentzVector& momentum, PHPyOniaMuonTrigger::Arm arm )
{
  
  const double DEG_TO_RAD = M_PI/180.0;
  const double south_theta_range[2] = { DEG_TO_RAD*143, DEG_TO_RAD*171 };
  const double north_theta_range[2] = { DEG_TO_RAD*9, DEG_TO_RAD*37 };
  
  return (
    (arm == South && std::abs( momentum.Theta() ) >= south_theta_range[0] && std::abs( momentum.Theta() ) <= south_theta_range[1] ) ||
    (arm == North && std::abs( momentum.Theta() ) >= north_theta_range[0] && std::abs( momentum.Theta() ) <= north_theta_range[1] ) );
  
};

//_________________________________________________________________________
void PHPyOniaMuonTrigger::book_normalization_tree( void )
{
  assert( !( _normalization_file.empty() || _normalization_tree ) );
  
  cout << "PHPyOniaMuonTrigger::book_normalization_tree - writing to " << _normalization_file << endl;

  _normalization_tfile = new TFile( _normalization_file.c_str(), "RECREATE" ); 

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };
  _normalization_tree = new TTree(  "h10", "J/Psi before selection" );	
  _normalization_tree->Branch( "Px", &_Px, "Px/D", BUFFER_SIZE );
  _normalization_tree->Branch( "Py", &_Py, "Py/D", BUFFER_SIZE );
  _normalization_tree->Branch( "Pz", &_Pz, "Pz/D", BUFFER_SIZE );
  _normalization_tree->Branch( "E", &_E, "E/D", BUFFER_SIZE );
  _normalization_tree->Branch( "pT", &_pT, "pT/D", BUFFER_SIZE );
  _normalization_tree->Branch( "rapidity", &_rapidity, "rapidity/D", BUFFER_SIZE );
  _normalization_tree->Branch( "Evt_bbcZ", &_Evt_bbcZ, "Evt_bbcZ/D", BUFFER_SIZE );
  _normalization_tree->Branch( "Centrality", &_Centrality, "Centrality/D", BUFFER_SIZE );
  _normalization_tree->Branch( "accepted", &_accepted, "accepted/I", BUFFER_SIZE );

  if (_add_chi_c_photon) // check Chi_c flag
  {
  _normalization_tree->Branch( "gPx", &_gPx, "gPx/D", BUFFER_SIZE );
  _normalization_tree->Branch( "gPy", &_gPy, "gPy/D", BUFFER_SIZE );
  _normalization_tree->Branch( "gPz", &_gPz, "gPz/D", BUFFER_SIZE );
  _normalization_tree->Branch( "gE", &_gE, "gE/D", BUFFER_SIZE );
  _normalization_tree->Branch( "gpT", &_gpT, "gpT/D", BUFFER_SIZE );
  _normalization_tree->Branch( "grapidity", &_grapidity, "grapidity/D", BUFFER_SIZE );
  }
  _normalization_tree->SetAutoSave( AUTO_SAVE );
  cout << "PHPyOniaMuonTrigger::book_normalization_tree - h10 ntuple created" << endl;
  
}

//_________________________________________________________________________
void PHPyOniaMuonTrigger::fill_normalization_tree( void )
{ 
  
  if( _normalization_file.empty() ) return;
  if( !( _normalization_tree ) ) book_normalization_tree();
  _normalization_tree->Fill();
  
}
