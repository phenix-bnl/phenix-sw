#include <PHPyDimuonTrigger.h>
#include <PHPyTrigger.h>
#include <PHPythiaHeader.h>
#include <PHPythiaContainer.h>
#include <PHPyCommon.h>
#include <getClass.h>
#include <Fun4AllReturnCodes.h>
#include <TLorentzVector.h>

#include <TMCParticle.h>
#include <cassert>
#include <cmath>
#include <cstdlib>

#include <algorithm>
#include <iterator>

using namespace std;

//___________________________________________________________________________
PHPyDimuonTrigger::PHPyDimuonTrigger(const string &name): 
    PHPyTrigger(name),
    _use_vtx( false ),
    _vertex_hist(NULL),
    _mode( All ),
    _muon_pt_min( 0. ),
    _min_pz( 0.0 ),
    _cut_on_parent( false ),
    _parent_mode( DirectParent ),
    _accepted_parents(),
    _unacceptable_parents(),
    _accepted_particles(),
    _trigger_filename( "dimu_trig.root" ),
    _trigger_tfile(NULL),
    _muon_tree(NULL),
    _eventnumber( 0 )
{ 
}

//___________________________________________________________________________
PHPyDimuonTrigger::~PHPyDimuonTrigger( void )
{}
  
//___________________________________________________________________________
int PHPyDimuonTrigger::Init(PHCompositeNode *top_node)
{

  cout << "PHPyDimuonTrigger::Init" << endl;
  cout << "_mode: " << _mode << endl;
  cout << " 4*PI acceptance: " << (_mode&All ? "true":"false") << endl;

  cout << "Cut on parent: " << _cut_on_parent << endl;
  cout << "Accepted parents: ";
  std::copy(_accepted_parents.begin(),
            _accepted_parents.end(),
            std::ostream_iterator<int>( cout, " "));
  cout << endl;

  cout << "Accepted particles: ";
  std::copy(_accepted_particles.begin(),
            _accepted_particles.end(),
            std::ostream_iterator<int>( cout, " "));
  cout << endl;
  
  cout << "_trigger_file: " << _trigger_filename << endl;
  return EVENT_OK;
  
} 
  
//___________________________________________________________________________
int PHPyDimuonTrigger::End(PHCompositeNode *top_node)
{

  // close TFile
  if(  _trigger_tfile )
  {
    _trigger_tfile->Write();
    _trigger_tfile->Close();
    delete _trigger_tfile;
  }
  
  // dump out trigger statistics
  cout << "PHPyDimuonTrigger::End - number of triggered events: " << _eventnumber << "/" << nconsidered << endl;
  if ( nconsidered>0 ) cout << "PHPyDimuonTrigger::End - ratio: " << float(_eventnumber)/nconsidered << endl;
  return EVENT_OK;
  
}

//___________________________________________________________________________
int PHPyDimuonTrigger::process_event(PHCompositeNode *top_node)
{
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(top_node,"PHPythiaHeader");
  if (!phpythiaheader)
  {
    cout << "PHPyOpenMuonTrigger::process_event - unable to get PHPythiaHeader, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(top_node,"PHPythia");
  if (!phpythia)
  {
    cout << "PHPyOpenMuonTrigger::process_event - unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }
  
  // increment counter
  ++nconsidered;
  if(nconsidered%10000==0) cout << "Event : " << nconsidered << endl;

  //move vertex if _vertex_hist is set
  if( _use_vtx )
  {
    double tmp_vtxz = _vertex_hist->GetRandom();
    phpythiaheader->moveVertex( 0., 0., tmp_vtxz );
    phpythia->moveVertex( 0., 0., tmp_vtxz );
  }
 
  // check if particles fullfil trigger conditions
  return ( dimuon_trigger(phpythia) ) ? EVENT_OK : ABORTEVENT;
  
}

//_________________________________________________________________________
bool PHPyDimuonTrigger::dimuon_trigger(PHPythiaContainer *phpylist)
{

  //check for >1 muons
  _num_muons = 0;
  //clear the container
  muon_container.clear();
  
  for( unsigned int ipart=0; ipart < phpylist->size(); ++ipart)
  {
    TMCParticle *part = phpylist->getParticle(ipart);

    if( accepted_particle(part) )
    {

      // Make sure it has not already decayed
      if(part->GetKS() != 1) 
	  continue;

      // Momentum must be enough to trigger dimuon
      if( abs( part->GetPz() ) < _min_pz )
        continue;

      // Discarding particles from unacceptable parents
      for( std::set<int>::iterator it = _unacceptable_parents.begin(); it != _unacceptable_parents.end(); ++it )
      	{
      	  if( phpylist->hasAncestor(part, *it))
      	    continue;
      	}

      TLorentzVector mom(part->GetPx(),
                         part->GetPy(),
                         part->GetPz(),
                         part->GetEnergy());      
      
      // Check if the muon is from an accepted parent particle
      if( _cut_on_parent && !accepted_parent(phpylist, part) )
        continue;

      // Only count the muon if it in the requested acceptance
      if( ( _mode & All )
          || (( _mode & South ) && muon_in_muon_arm( mom, South ))
          || (( _mode & North ) && muon_in_muon_arm( mom, North )) )
      {
        muon_container.push_back( *part );        
        _num_muons++;
	cout << "Number of muons for event " << nconsidered << " so far: " << _num_muons << endl;
      }
    }
  }

  if( _num_muons < 2)
    return false;
  
  ++_eventnumber;
  _Evt_bbcZ = 0.;

  //make dimuons and fill the tree
  for( vector<TMCParticle>::iterator trk1=muon_container.begin(); trk1!=muon_container.end();
       ++trk1)
  {
    for( vector<TMCParticle>::iterator trk2=trk1+1; trk2!=muon_container.end(); ++trk2)
    {
      if(_verbosity)
      {
        cout << "Event: " << _eventnumber<< endl;
        cout << "muon1: "<< trk1->GetKF() << " " << trk1->GetPx() << endl;
        cout << "muon2: "<< trk2->GetKF() << " " << trk2->GetPx() << endl;
      }
        
      reset_variables(); 
      //first fill dimuons
      mass = std::sqrt( square( trk1->GetEnergy() + trk2->GetEnergy() ) - (
          square( trk1->GetPx() + trk2->GetPx() ) +
          square( trk1->GetPy() + trk2->GetPy() ) +
          square( trk1->GetPz() + trk2->GetPz() ) ) );
      Px = trk1->GetPx() + trk2->GetPx();
      Py = trk1->GetPy() + trk2->GetPy();
      Pz = trk1->GetPz() + trk2->GetPz();
      E = trk1->GetEnergy() + trk2->GetEnergy();
      Pt = std::sqrt( square(Px) + square(Py) );
      rapidity = 0.5*std::log( (E+Pz)/(E-Pz) );  

      if( trk1->GetKF() * trk2->GetKF() < 0 )
      {
        charge = 0;
      } else if(trk1->GetKF()>0 && trk2->GetKF()>0)
      {
        charge = 1;
      } else if(trk1->GetKF()<0 && trk2->GetKF()<0)
      {
        charge = -1;
      }
  
      // Fill single muons 
      TMCParticle *anc1 = phpylist->hasAncestorAbsRange( &*trk1, 1, 10 );
      TMCParticle *parent1 = phpylist->getParent( &*trk1 );  
      /* if( anc1 == parent1 )
         {
         cout << "anc and parent are same" << phpylist->getHistoryString( &*trk1 )  << endl;
         } else if(anc1){
         cout << "anc KF=" << anc1->GetKF() << " parent KF= " << parent1->GetKF() << " " << phpylist->getHistoryString( &*trk1 ) << endl;
         } */

      Tr0_kF = trk1->GetKF();
      Tr0_Px = trk1->GetPx();
      Tr0_Py = trk1->GetPy();
      Tr0_Pz = trk1->GetPz();
      Tr0_E = trk1->GetEnergy();
      Tr0_pT = std::sqrt( square(Tr0_Px) + square(Tr0_Py) );
      Tr0_rapidity = 0.5*std::log( (Tr0_E + Tr0_Pz)/(Tr0_E - Tr0_Pz) );

      int nchar = phpylist->getHistoryString( &*trk1 ).Length();
      if ( nchar > 128 ) nchar = 128;
      for (int ichar = 0; ichar < nchar; ichar++) Tr0_history[ichar] = (phpylist->getHistoryString( &*trk1 ))[ichar];
      Tr0_Vx = trk1->GetVx()/10.; //vertex in mm
      Tr0_Vy = trk1->GetVy()/10.; 
      Tr0_Vz = trk1->GetVz()/10.; 
      Tr0_parentKF = parent1->GetKF();
      Tr0_parentKS = parent1->GetKS();
      Tr0_parentPx = parent1->GetPx();
      Tr0_parentPy = parent1->GetPy();
      Tr0_parentPz = parent1->GetPz();
      Tr0_parentE = parent1->GetEnergy();
      Tr0_parentPt = std::sqrt( square(Tr0_parentPx)+square(Tr0_parentPy) );
      Tr0_parentRap = 0.5*std::log( (Tr0_parentE + Tr0_parentPz)/(Tr0_parentE - Tr0_parentPz) );
      //Tr0_KF = parent1->GetKF();
     
      if(anc1)
      {
        Tr0_ancKF = anc1->GetKF();
        Tr0_ancKS = anc1->GetKS();
        Tr0_ancPx = anc1->GetPx();
        Tr0_ancPy = anc1->GetPy();
        Tr0_ancPz = anc1->GetPz();
        Tr0_ancE =  anc1->GetEnergy();
        Tr0_ancPt = std::sqrt( square(Tr0_ancPx)+square(Tr0_ancPy) );
        Tr0_ancRap = 0.5*std::log( (Tr0_ancE + Tr0_ancPz)/(Tr0_ancE - Tr0_ancPz));
      }
       


      TMCParticle *anc2 = phpylist->hasAncestorAbsRange( &*trk2, 1, 10 );
      TMCParticle *parent2 = phpylist->getParent( &*trk2 );
      Tr1_kF = trk2->GetKF();
      Tr1_Px = trk2->GetPx();
      Tr1_Py = trk2->GetPy();
      Tr1_Pz = trk2->GetPz();
      Tr1_E = trk2->GetEnergy();
      Tr1_pT = std::sqrt( square(Tr1_Px) + square(Tr1_Py) );
      Tr1_rapidity = 0.5*std::log( (Tr1_E + Tr1_Pz)/(Tr1_E - Tr1_Pz) );

      nchar = phpylist->getHistoryString( &*trk2 ).Length();
      if ( nchar > 128 ) nchar = 128;
      for (int ichar = 0; ichar < nchar; ichar++) Tr1_history[ichar] = (phpylist->getHistoryString( &*trk2 ))[ichar];
      Tr1_Vx = trk2->GetVx()/10.; //vertex in mm
      Tr1_Vy = trk2->GetVy()/10.;
      Tr1_Vz = trk2->GetVz()/10.;
      Tr1_parentKF = parent2->GetKF();
      Tr1_parentKS = parent2->GetKS();
      Tr1_parentPx = parent2->GetPx();
      Tr1_parentPy = parent2->GetPy();
      Tr1_parentPz = parent2->GetPz();
      Tr1_parentE = parent2->GetEnergy();
      Tr1_parentPt = std::sqrt( square(Tr1_parentPx)+square(Tr1_parentPy) );
      Tr1_parentRap = 0.5*std::log( (Tr1_parentE + Tr1_parentPz)/(Tr1_parentE - Tr1_parentPz) );

      if(anc2)
      {
        Tr1_ancKF = anc2->GetKF();
        Tr1_ancKS = anc2->GetKS();
        Tr1_ancPx = anc2->GetPx();
        Tr1_ancPy = anc2->GetPy();
        Tr1_ancPz = anc2->GetPz();
        Tr1_ancE =  anc2->GetEnergy();
        Tr1_ancPt = std::sqrt( square(Tr1_ancPx)+square(Tr1_ancPy) );
        Tr1_ancRap = 0.5*std::log( (Tr1_ancE + Tr1_ancPz)/(Tr1_ancE - Tr1_ancPz));
      }

      fill_muon_tree();      
    }
  }

  return true;    

}

//_________________________________________________________________________
bool PHPyDimuonTrigger::accepted_parent(PHPythiaContainer *phpylist, TMCParticle *part)
{
  if( _parent_mode == DirectParent )
    {
      TMCParticle *parent =  phpylist->getParticle( part->GetParent() -1 );
      int kfParent =  parent->GetKF();
      //
      // Sometimes direct parent is the same particle, listed with
      // KS = 21, so you have to go back one more step.  Stupid PYTHIA.
      //
      if (kfParent == part->GetKF())
	{
	  TMCParticle *pparent = phpylist->getParticle(parent->GetParent() -1 );
	  kfParent = pparent->GetKF();
	}
      return _accepted_parents.find(kfParent) != _accepted_parents.end();
    }
  else if( _parent_mode == Ancestor )
    {
      for( std::set<int>::iterator it = _accepted_parents.begin();
	   it != _accepted_parents.end();
	   ++it )
	{
	  if( phpylist->hasAncestor(part, *it))
	    return true;
	}
    }
  else if( _parent_mode == NotRequired )
    return true;

  return false;
}

bool PHPyDimuonTrigger::accepted_particle(TMCParticle *part)
{
  int kfPart =  part->GetKF();

  bool test = _accepted_particles.find(kfPart) != _accepted_particles.end();

  return test;
}

//_________________________________________________________________________
void PHPyDimuonTrigger::set_accepted_parents(const std::vector<int>& parIn)
{
  std::copy(parIn.begin(),
            parIn.end(),
            std::inserter(_accepted_parents,_accepted_parents.end()));

  if( !_accepted_parents.empty() )
    _cut_on_parent = true;    
}

//_________________________________________________________________________
void PHPyDimuonTrigger::set_accepted_particles(const std::vector<int>& parIn)
{
  std::copy(parIn.begin(),
            parIn.end(),
            std::inserter(_accepted_particles,_accepted_particles.end()));

}

//_________________________________________________________________________
void PHPyDimuonTrigger::set_unacceptable_parents(int* parIn, int npar)
{
  std::copy(parIn,
            parIn + npar,
            std::inserter(_unacceptable_parents,_unacceptable_parents.end()));
}

//_________________________________________________________________________
void PHPyDimuonTrigger::set_accepted_particles(int* parIn, int npar)
{
  std::copy(parIn,
            parIn + npar,
            std::inserter(_accepted_particles,_accepted_particles.end()));
}

//_________________________________________________________________________
void PHPyDimuonTrigger::set_accepted_parents(int* parIn, int npar)
{
  std::copy(parIn,
            parIn + npar,
            std::inserter(_accepted_parents,_accepted_parents.end()));

  if( !_accepted_parents.empty() )
    _cut_on_parent = true;  
}

//_________________________________________________________________________
bool PHPyDimuonTrigger::muon_in_muon_arm( const TLorentzVector& momentum, PHPyDimuonTrigger::Mode arm)
 {  
  const double DEG_TO_RAD = M_PI/180.0;
  const double south_theta_range[2] = { DEG_TO_RAD*143, DEG_TO_RAD*171 };
  const double north_theta_range[2] = { DEG_TO_RAD*9, DEG_TO_RAD*37 };
  
  return (
  (arm == South && std::abs( momentum.Theta() ) >= south_theta_range[0] && std::abs( momentum.Theta() ) <= south_theta_range[1] ) ||
  (arm == North && std::abs( momentum.Theta() ) >= north_theta_range[0] && std::abs( momentum.Theta() ) <= north_theta_range[1] ) );
  
};

//_________________________________________________________________________
void PHPyDimuonTrigger::book_muon_tree( void )
{
  assert( !( _trigger_filename.empty() || _muon_tree ) );
  
  //cout << "PHPyDimuonTrigger::book_muon_tree - writing to " << _trigger_filename << endl;

  _trigger_tfile = new TFile( _trigger_filename.c_str(), "RECREATE" ); 

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };
  //event variables
  _muon_tree = new TTree(  "muons", "J/Psi before selection" );	
  _muon_tree->Branch( "eventnumber", &_eventnumber, "eventnumber/I", BUFFER_SIZE );
  _muon_tree->Branch( "Evt_bbcZ", &_Evt_bbcZ, "Evt_bbcZ/D", BUFFER_SIZE );
  _muon_tree->Branch( "num_muons", &_num_muons, "num_muons/I", BUFFER_SIZE );

  //dimuon variables
  _muon_tree->Branch( "mass", &mass, "mass/D", BUFFER_SIZE );
  _muon_tree->Branch( "charge", &charge, "charge/I", BUFFER_SIZE );
  _muon_tree->Branch( "rapidity", &rapidity, "rapidity/D", BUFFER_SIZE );
  _muon_tree->Branch( "Px", &Px, "Px/D", BUFFER_SIZE );
  _muon_tree->Branch( "Py", &Py, "Py/D", BUFFER_SIZE );
  _muon_tree->Branch( "Pz", &Pz, "Pz/D", BUFFER_SIZE );
  _muon_tree->Branch( "Pt", &Pt, "Pt/D", BUFFER_SIZE );
  _muon_tree->Branch( "E", &E, "E/D", BUFFER_SIZE );

  //single muon variables
  _muon_tree->Branch( "Tr0_kF", &Tr0_kF, "Tr0_kF/I", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_Px", &Tr0_Px, "Tr0_Px/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_Py", &Tr0_Py, "Tr0_Py/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_Pz", &Tr0_Pz, "Tr0_Pz/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_E", &Tr0_E, "Tr0_E/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_pT", &Tr0_pT, "Tr0_pT/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_rapidity", &Tr0_rapidity, "Tr0_rapidity/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_ancKF", &Tr0_ancKF, "Tr0_ancKF/I", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_history", Tr0_history, "Tr0_history/C", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_Vx", &Tr0_Vx, "Tr0_Vx/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_Vy", &Tr0_Vy, "Tr0_Vy/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_Vz", &Tr0_Vz, "Tr0_Vz/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_parentKF", &Tr0_parentKF, "Tr0_parentKF/I", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_parentKS", &Tr0_parentKS, "Tr0_parentKS/I", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_parentPx", &Tr0_parentPx, "Tr0_parentPx/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_parentPy", &Tr0_parentPy, "Tr0_parentPy/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_parentPz", &Tr0_parentPz, "Tr0_parentPz/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_parentE", &Tr0_parentE, "Tr0_parentE/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_parentPt", &Tr0_parentPt, "Tr0_parentPt/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_parentRap", &Tr0_parentRap, "Tr0_parentRap/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_ancKS", &Tr0_ancKS, "Tr0_ancKS/I", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_ancPx", &Tr0_ancPx, "Tr0_ancPx/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_ancPy", &Tr0_ancPy, "Tr0_ancPy/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_ancPz", &Tr0_ancPz, "Tr0_ancPz/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_ancE", &Tr0_ancE, "Tr0_ancE/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_ancPt", &Tr0_ancPt, "Tr0_ancPt/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr0_ancRap", &Tr0_ancRap, "Tr0_ancRap/D", BUFFER_SIZE );



  _muon_tree->Branch( "Tr1_kF", &Tr1_kF, "Tr1_kF/I", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_Px", &Tr1_Px, "Tr1_Px/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_Py", &Tr1_Py, "Tr1_Py/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_Pz", &Tr1_Pz, "Tr1_Pz/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_E", &Tr1_E, "Tr1_E/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_pT", &Tr1_pT, "Tr1_pT/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_rapidity", &Tr1_rapidity, "Tr1_rapidity/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_ancKF", &Tr1_ancKF, "Tr1_ancKF/I", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_history", Tr1_history, "Tr1_history/C", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_Vx", &Tr1_Vx, "Tr1_Vx/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_Vy", &Tr1_Vy, "Tr1_Vy/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_Vz", &Tr1_Vz, "Tr1_Vz/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_parentKF", &Tr1_parentKF, "Tr1_parentKF/I", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_parentKS", &Tr1_parentKS, "Tr1_parentKS/I", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_parentPx", &Tr1_parentPx, "Tr1_parentPx/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_parentPy", &Tr1_parentPy, "Tr1_parentPy/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_parentPz", &Tr1_parentPz, "Tr1_parentPz/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_parentE", &Tr1_parentE, "Tr1_parentE/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_parentPt", &Tr1_parentPt, "Tr1_parentPt/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_parentRap", &Tr1_parentRap, "Tr1_parentRap/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_ancKS", &Tr1_ancKS, "Tr1_ancKS/I", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_ancPx", &Tr1_ancPx, "Tr1_ancPx/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_ancPy", &Tr1_ancPy, "Tr1_ancPy/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_ancPz", &Tr1_ancPz, "Tr1_ancPz/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_ancE", &Tr1_ancE, "Tr1_ancE/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_ancPt", &Tr1_ancPt, "Tr1_ancPt/D", BUFFER_SIZE );
  _muon_tree->Branch( "Tr1_ancRap", &Tr1_ancRap, "Tr1_ancRap/D", BUFFER_SIZE );


  _muon_tree->SetAutoSave( AUTO_SAVE );

  cout << "PHPyDimuonTrigger::book_muon_tree - muons ntuple created" << endl;
  
}

//_________________________________________________________________________
void PHPyDimuonTrigger::fill_muon_tree( void )
{ 
  
  if( _trigger_filename.empty() ) return;
  if( !( _muon_tree ) ) book_muon_tree();
  _muon_tree->Fill();
  
}


//________________________________________________________________________
void PHPyDimuonTrigger::reset_variables( void )
{
  //Reset values
  mass = -9999;
  charge = -9999;
  rapidity = -9999;
  Px = -9999;
  Py = -9999;
  Pz = -9999;
  Pt = -9999;
  E = -9999;
  
  Tr0_kF = -999;
  Tr0_Px = -9999;
  Tr0_Py = -9999;
  Tr0_Pz = -9999;
  Tr0_E  = -9999;
  Tr0_pT = -9999;
  Tr0_rapidity = -9999;
  Tr0_ancKF = -9999;
  for (int ichar = 0; ichar < 128; ichar++) Tr0_history[ichar]  = ' ';
  Tr0_Vx = -9999;
  Tr0_Vy = -9999;
  Tr0_Vz = -9999;
  Tr0_parentKF = -9999;
  Tr0_parentKS = -9999;
  Tr0_parentPx = -9999;
  Tr0_parentPy = -9999;
  Tr0_parentPz = -9999;
  Tr0_parentE = -9999;
  Tr0_parentPt = -9999;
  Tr0_parentRap = -9999;
  Tr0_ancKF = -9999;
  Tr0_ancKS = -9999;
  Tr0_ancPx = -9999;
  Tr0_ancPy = -9999;
  Tr0_ancPz = -9999;
  Tr0_ancE = -9999;
  Tr0_ancPt = -9999;
  Tr0_ancRap = -9999;
  
  
  Tr1_kF = -999;
  Tr1_Px = -9999;
  Tr1_Py = -9999;
  Tr1_Pz = -9999;
  Tr1_E  = -9999;
  Tr1_pT = -9999;
  Tr1_rapidity = -9999;
  Tr1_ancKF = -9999;
  for (int ichar = 0; ichar < 128; ichar++) Tr1_history[ichar]  = ' ';
  Tr1_Vx = -9999;
  Tr1_Vy = -9999;
  Tr1_Vz = -9999;
  Tr1_parentKF = -9999;
  Tr1_parentKS = -9999;
  Tr1_parentPx = -9999;
  Tr1_parentPy = -9999;
  Tr1_parentPz = -9999;
  Tr1_parentE = -9999;
  Tr1_parentPt = -9999;
  Tr1_parentRap = -9999;
  Tr1_ancKF = -9999;
  Tr1_ancKS = -9999;
  Tr1_ancPx = -9999;
  Tr1_ancPy = -9999;
  Tr1_ancPz = -9999;
  Tr1_ancE = -9999;
  Tr1_ancPt = -9999;
  Tr1_ancRap = -9999;

}
