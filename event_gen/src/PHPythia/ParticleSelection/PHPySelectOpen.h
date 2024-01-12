#ifndef __PHPYSELECTOPEN_H__
#define __PHPYSELECTOPEN_H__

#include "PHPyParticleSelect.h"
#include "SubsysReco.h"

#include <vector>

/**

Reco Module for Selecting only Quarkopen and their descendants 
to write out in PYTHIA events.

*/

class PHCompositeNode;
class PHPythiaHeader;
class PHPythiaContainer;
class TMCParticle;

class PHPySelectOpen: public PHPyParticleSelect
{
public:
  //! constructor
  PHPySelectOpen(const std::string &name = "PHPySelectOpen");
  //! destructor
  virtual ~PHPySelectOpen();

  // Override this method to make your particle selection
  virtual int ParticleCut( TMCParticle *part );

  enum ParentMode
  {
    NotRequired = 0,
    DirectParent = 1,
    Ancestor = 2
  };

  virtual void SetParentMode( unsigned int mode ) { _parent_mode = mode; }

  virtual unsigned int GetParentMode(  ) { return _parent_mode; }

  virtual void SetPtMin( float value ) { _pt_min = value; }

  virtual float GetPtMin( ) { return _pt_min; }

  virtual void SetPMin( float value ) { _p_min = value; }

  virtual void SetMinPzAbs(float value) { _min_pz = value; }

  virtual float GetPMin( ) { return _p_min; }

  // Add the particle to be selected
  virtual void AddParticleChoice( int particle_id );

  virtual void AddParticleChoiceAbs( int particle_id );

  // Add the particle's ancestor to be selected
  virtual void AddParentChoice( int parent_id );

  virtual void AddBadParent( int parent_id );

  virtual void AddParentChoiceAbs( int parent_id );

  virtual void AddBadParentAbs( int parent_id );

  virtual void SetVerbosity( int verbosity ) { _verbosity = verbosity; }

  virtual int GetVerbosity( )  { return _verbosity; }

protected:

  unsigned int _parent_mode;
  
  float _pt_min;

  float _p_min;

  float _min_pz;

  int _verbosity;

  std::vector<int> _particle_choice;
  std::vector<int> _parent_choice;
  std::vector<int> _bad_parent;
};

#endif	/* __PHPYSELECTOPEN_H__ */

