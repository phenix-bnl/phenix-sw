#ifndef __MCHEPMCPARTICLECONTAINER_H__
#define __MCHEPMCPARTICLECONTAINER_H__

#include <iostream>

#include "PHObject.h"

class MCHepMCParticle;
class TClonesArray;

class MCHepMCParticleContainer: public PHObject
{
 public:
  MCHepMCParticleContainer();
  MCHepMCParticleContainer(const MCHepMCParticleContainer&);
  MCHepMCParticleContainer& operator=(const MCHepMCParticleContainer&);
  virtual ~MCHepMCParticleContainer();

  virtual unsigned int get_nMCHepMCParticles() const {return nMCHepMCParticles;}
  virtual MCHepMCParticle* const get_MCHepMCParticle(const unsigned int ihep) const;


  void copyfrom(const MCHepMCParticleContainer*);
  // virtual copy constructor
  virtual MCHepMCParticleContainer* clone() const;

  // Standard functions of all virtual classes...
  virtual void Reset();
  virtual int  isValid() const;
  virtual void identify(std::ostream &os = std::cout) const;

  MCHepMCParticle*  AddMCHepMCParticle(const MCHepMCParticle &src_mcsinglemuon);

  TClonesArray*   GetMCHepMCParticleContainer() {return MCHepMCParticles;}

  unsigned int nMCHepMCParticles;
  TClonesArray *MCHepMCParticles;

  void copyto(MCHepMCParticleContainer& dest) const;

  ClassDef(MCHepMCParticleContainer,1)
};

#endif /* __MCHEPMCPARTICLECONTAINER_H__ */
