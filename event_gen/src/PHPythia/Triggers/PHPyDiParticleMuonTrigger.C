/*!
   \file PHPyDiParticleMuonTrigger.C
   \brief trigger module to select events that contains two required particltesa in the muon arm acceptance
   \author Zhengyun You
   \version $Revision: 1.3 $
   \date $Date: 2009/05/27 21:27:54 $
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

#include "PHPyDiParticleMuonTrigger.h"
#include "PHPyTrigger.h"
#include "PHPythiaHeader.h"
#include "PHPythiaContainer.h"
#include "PHPyCommon.h"

using namespace std;

//___________________________________________________________________________
PHPyDiParticleMuonTrigger::PHPyDiParticleMuonTrigger(const string &name): 
  PHPyTrigger(name),
  _mode( All ),
  _pt_min( 0 ),
  _p_min( 0 ),
  _npart_min( 2 ), 
  _normalization_file( "pythia.root" ),
  _normalization_tfile( 0 ),
  _fill_normalization_tree( true ),
  _use_internal_vertex_shift( true ),
  _normalization_tree( 0 )
{ 
  // trigger type (?)
  trigger_type = MINBIAS;
  
  // create vertex shift
  _vertex_shift = new PHPyVertexShift( "INTERNAL_VERTEX_SHIFT" );
}

//___________________________________________________________________________
PHPyDiParticleMuonTrigger::~PHPyDiParticleMuonTrigger( void )
{ if( _vertex_shift ) delete _vertex_shift; }
  
//___________________________________________________________________________
int PHPyDiParticleMuonTrigger::Init(PHCompositeNode *top_node)
{
  

  cout << "PHPyDiParticleMuonTrigger::Init" << endl;
  
  cout << "_mode: " << _mode << endl;
  cout << "  south: " << (_mode&South ? "true":"false" ) << endl; 
  cout << "  north: " << (_mode&North ? "true":"false" ) << endl; 
  cout << "  back to back: " << (_mode&BackToBack ? "true":"false" ) << endl; 
  cout << "_pt_min: " << _pt_min << endl;
  
  cout << "_normalization_file: " << _normalization_file << endl;
  cout << "_fill_normalization_tree: " << _fill_normalization_tree << endl;
  cout << "_use_internal_vertex_shift: " << _use_internal_vertex_shift << endl;
  
  if( _use_internal_vertex_shift ) return  _vertex_shift->Init( top_node );
  else return EVENT_OK;
  
} 
  
//___________________________________________________________________________
int PHPyDiParticleMuonTrigger::End(PHCompositeNode *top_node)
{

  // close TFile
  if( _fill_normalization_tree && _normalization_tfile )
  {
    _normalization_tfile->Write();
    _normalization_tfile->Close();
    delete _normalization_tfile;
  }
  
  // dump out trigger statistics
  cout << "PHPyDiParticleMuonTrigger::End - number of triggered events: " << ntriggered << "/" << nconsidered << endl;
  if ( nconsidered>0 ) cout << "PHPyDiParticleMuonTrigger::End - ratio: " << float(ntriggered)/nconsidered << endl;
  return EVENT_OK;
  
}

//___________________________________________________________________________
int PHPyDiParticleMuonTrigger::process_event(PHCompositeNode *top_node)
{
  
  static int counter(0);
  
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(top_node,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cout << "PHPyDiParticleMuonTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(top_node,"PHPythia");
  if (!phpythia)
  {
    cout << "PHPyDiParticleMuonTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  _Evt_bbcZ = -999;
  _Centrality = -999;
  
  // check if particles fullfil trigger conditions
  _accepted = diparticle_in_muon_arm(phpythia);
  
  if( _accepted ) ++ntriggered;

  if( _accepted && _use_internal_vertex_shift )
  { 
    
    int out( _vertex_shift->process_event( top_node ) );
    if( !( out == EVENT_OK && _vertex_shift->event_valid() ) ) 
    {
    
      cout << "PHPyDiParticleMuonTrigger::process_event - event: " << ++counter << " vertex_shift event invalid" << endl;
      return out; 
    
    } else {
      
      _Evt_bbcZ = _vertex_shift->event_vertex_z();
      _Centrality = _vertex_shift->event_centrality();
      cout << "PHPyDiParticleMuonTrigger::process_event - event: " << ++counter << " _Evt_bbcZ: " << _Evt_bbcZ << endl;
      
    }
  }
  
  // fill normalization ntuple for all events
  // must be called after vertex_shift is called, so that accepted events have correct vertex and centrality
  if( _fill_normalization_tree ) fill_normalization_tree();
  
  // output
  return (_accepted) ? EVENT_OK:ABORTEVENT;
  
}

//_________________________________________________________________________
bool PHPyDiParticleMuonTrigger::diparticle_in_muon_arm(PHPythiaContainer *phpylist)
{
  int n_accept = 0;
  
  // loop over particles, store mu_plus and mu_minus candidate
  for( unsigned int ipart=0; ipart<phpylist->size(); ipart++)
  {
    
    TMCParticle *part = phpylist->getParticle(ipart);

    bool pid_ok = false;
    for (unsigned int i=0; i<_particle_choice.size(); i++)
    {
      if ( pid_ok ) break;
      if ( part->GetKF() == _particle_choice[i] || _particle_choice[i] == 0 ) pid_ok = true;
    }
    if ( !pid_ok ) continue;       


    TLorentzVector momentum( part->GetPx(), part->GetPy(), part->GetPz(), part->GetEnergy() );
    if ( momentum.P() < _p_min || momentum.Pt() < _pt_min ) continue;

    if ( ( _mode & South ) && muon_in_muon_arm( momentum, South ) && muon_in_muon_arm( momentum, South ) ) n_accept++;
    else if ( ( _mode & North ) && muon_in_muon_arm( momentum, North ) && muon_in_muon_arm( momentum, North ) ) n_accept++;
    else if ( ( _mode & BackToBack ) && (
       ( muon_in_muon_arm( momentum, South ) && muon_in_muon_arm( momentum, North ) ) ||
       ( muon_in_muon_arm( momentum, North ) && muon_in_muon_arm( momentum, South ) ) ) ) n_accept++;
  }
    
    // check if partices are muons
    //int kf = part->GetKF();
    //if ( kf==PY_MU || kf==-PY_MU )
  
  // store relevant ntuple variables
  /*
  _Px = momentum.Px();
  _Py = momentum.Py();
  _Pz = momentum.Pz();
  _E = momentum.E();
  _pT = momentum.Pt();
  _rapidity = momentum.Rapidity();
  */

  if ( n_accept >= _npart_min ) return true;
  else  return false;
    
}

//_________________________________________________________________________
bool PHPyDiParticleMuonTrigger::muon_in_muon_arm( const TLorentzVector& momentum, PHPyDiParticleMuonTrigger::Arm arm )
{
  
  const double DEG_TO_RAD = M_PI/180.0;
  const double south_theta_range[2] = { DEG_TO_RAD*143, DEG_TO_RAD*171 };
  const double north_theta_range[2] = { DEG_TO_RAD*9, DEG_TO_RAD*37 };
  
  return (
    (arm == South && std::abs( momentum.Theta() ) >= south_theta_range[0] && std::abs( momentum.Theta() ) <= south_theta_range[1] ) ||
    (arm == North && std::abs( momentum.Theta() ) >= north_theta_range[0] && std::abs( momentum.Theta() ) <= north_theta_range[1] ) );
  
};

//_________________________________________________________________________
void PHPyDiParticleMuonTrigger::AddParticleChoice( int particle_id )
{
  _particle_choice.push_back( particle_id );
}

//_________________________________________________________________________
void PHPyDiParticleMuonTrigger::AddParticleChoiceAbs( int particle_id )
{
  _particle_choice.push_back( particle_id );
  _particle_choice.push_back( -1*particle_id );
}

//_________________________________________________________________________
void PHPyDiParticleMuonTrigger::book_normalization_tree( void )
{
  assert( !( _normalization_file.empty() || _normalization_tree ) );
  
  cout << "PHPyDiParticleMuonTrigger::book_normalization_tree - writing to " << _normalization_file << endl;

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
  _normalization_tree->SetAutoSave( AUTO_SAVE );
  cout << "PHPyDiParticleMuonTrigger::book_normalization_tree - h10 ntuple created" << endl;
  
}

//_________________________________________________________________________
void PHPyDiParticleMuonTrigger::fill_normalization_tree( void )
{ 
  
  if( _normalization_file.empty() ) return;
  if( !( _normalization_tree ) ) book_normalization_tree();
  _normalization_tree->Fill();
  
}
