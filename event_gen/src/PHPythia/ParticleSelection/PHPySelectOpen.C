#include <iostream>
#include <iomanip>
#include <vector>

#include <getClass.h>
#include <Fun4AllReturnCodes.h>

#include <TLorentzVector.h>

#if ROOT_VERSION_CODE >= ROOT_VERSION(5,15,8) 
#include <TMCParticle.h>
#else
#include <TMCParticle6.h>
#endif

#include <PHPySelectOpen.h>
#include <PHPythiaContainer.h>
#include <PHPyCommon.h>

using namespace std;

//! Constructor
PHPySelectOpen::PHPySelectOpen(const std::string &name):
  PHPyParticleSelect(name),
  _parent_mode(PHPySelectOpen::NotRequired), 
  _pt_min(0.0),
  _p_min(0.0),
  _min_pz(0.0),
  _verbosity(0)
{
}

//! Destructor
PHPySelectOpen::~PHPySelectOpen()
{
  ;
}

//! Basic method to select particles
/// return 1 if we want to keep the particle, 0 if not
/// This example is for Quarkonia and any descendants 
int PHPySelectOpen::ParticleCut( TMCParticle *part )
{
  if ( part->GetKS() != 1 ) return false;

  bool is_kept = false;

  for (unsigned int i=0; i<_particle_choice.size(); i++)
  {
    if (is_kept) break;

    if ( part->GetKF() == _particle_choice[i] || _particle_choice[i] == 0 )
    {

      TLorentzVector momentum( part->GetPx(), part->GetPy(), part->GetPz(), part->GetEnergy() );
      if ( momentum.Pt() < _pt_min ) continue;
      if ( momentum.P() < _p_min ) continue;
      if ( fabs(momentum.Pz()) < _min_pz ) continue;

      if ( _parent_mode == NotRequired ) {
        is_kept = true;
        break;
      }
      else 
      {
	//rejecting particles for unacceptable heritage (sounds snobbish)
	for(unsigned int l=0; l>_bad_parent.size(); l++)
	  {
	    if ( phpythia->hasAncestor(part, _bad_parent[l]) != 0 )
	      {
		is_kept = false;
		break;
	      }
	  }

	//or keeping selective ones
        for (unsigned int j=0; j<_parent_choice.size(); j++)
        {
          if ( _parent_mode == DirectParent ) {
            TMCParticle *parent_part =  phpythia->getParent( part );
            Int_t kfparent = 0;
            if ( parent_part ) kfparent = parent_part->GetKF();

            if ( kfparent == _parent_choice[j] ) 
            {
              is_kept = true;
              break;
            }
          }
          else if ( _parent_mode == Ancestor ) {
            if ( phpythia->hasAncestor(part, _parent_choice[j]) != 0 )
            {
              is_kept = true;
              break;
            }
          }
        }
      }
    }
  }

  if (is_kept)  {
    if ( _verbosity ) cout << "Keep particle " << phpythia->getHistoryString(part) << endl;
    return 1;
  }

  return 0;
}

void PHPySelectOpen::AddParticleChoice( int particle_id )
{
  _particle_choice.push_back( particle_id );
}

void PHPySelectOpen::AddParticleChoiceAbs( int particle_id )
{
  _particle_choice.push_back( particle_id );
  _particle_choice.push_back( -1*particle_id );
}

void PHPySelectOpen::AddParentChoice( int parent_id )
{
  _parent_choice.push_back( parent_id );
}

void PHPySelectOpen::AddBadParent( int parent_id )
{
  _bad_parent.push_back( parent_id );
}

void PHPySelectOpen::AddParentChoiceAbs( int parent_id )
{
  _parent_choice.push_back( parent_id );
  _parent_choice.push_back( -1*parent_id );
}

void PHPySelectOpen::AddBadParentAbs( int parent_id )
{
  _bad_parent.push_back( parent_id );
  _bad_parent.push_back( -1*parent_id );
}
