#include <iostream>
#include "TClonesArray.h"
#include "phool.h"

#include "MCHepMCParticleContainer.h"
#include "MCHepMCParticle.h"

ClassImp(MCHepMCParticleContainer)

using namespace std;

static const unsigned int HEPMCPARTICLES = 10;

MCHepMCParticleContainer::MCHepMCParticleContainer()
{
  nMCHepMCParticles = 0;
  MCHepMCParticles  = new TClonesArray("MCHepMCParticle",HEPMCPARTICLES);
}

MCHepMCParticle* const
MCHepMCParticleContainer::get_MCHepMCParticle(const unsigned int ihep) const
{
  MCHepMCParticleContainer *THIS = (MCHepMCParticleContainer*)this;  //remove const-ness                                                                                                                                                                                                                       

  if(!(THIS->GetMCHepMCParticleContainer())) {
    cout <<" No MCHepMCParticleContainer TClonesArray."<<endl;
    return NULL;
  } else {
    MCHepMCParticle *heppart = (MCHepMCParticle*) THIS->GetMCHepMCParticleContainer()->UncheckedAt(ihep);
    return heppart;
  }
}



void
MCHepMCParticleContainer::copyfrom(const MCHepMCParticleContainer* src)
{
  Reset();
  
  for( unsigned int i = 0; i < src->get_nMCHepMCParticles(); ++i)
    {
      MCHepMCParticle* tmp = (MCHepMCParticle*)src->get_MCHepMCParticle(i);
      if(tmp) AddMCHepMCParticle(*tmp);
      else
	{
	  cerr << PHWHERE << "src single muon is null?" <<endl;
	}
    }
}

MCHepMCParticleContainer::MCHepMCParticleContainer(const MCHepMCParticleContainer& src)
{
  MCHepMCParticles=0;
  src.copyto(*this);
}

MCHepMCParticleContainer&
MCHepMCParticleContainer::operator=(const MCHepMCParticleContainer& src)
{
  if( this != &src) {
    src.copyto(*this);
  }
  return *this;
}

MCHepMCParticleContainer::~MCHepMCParticleContainer()
{
  MCHepMCParticles->Clear();
  delete MCHepMCParticles;
  return;
}

void
MCHepMCParticleContainer::Reset()
{
  MCHepMCParticles->Clear();
  if( nMCHepMCParticles > HEPMCPARTICLES) {
    MCHepMCParticles->Expand(HEPMCPARTICLES);
  }
  nMCHepMCParticles = 0;

  return;
}

int
MCHepMCParticleContainer::isValid() const
{
  return ((nMCHepMCParticles > 0) ? 1 : 0);
}

void
MCHepMCParticleContainer::identify(std::ostream &os) const
{
  os << "MCHepMCParticleContainer Object" <<endl;
  os << "Number of single muons: "<< nMCHepMCParticles <<endl;
  return;
}

MCHepMCParticle*
MCHepMCParticleContainer::AddMCHepMCParticle(const MCHepMCParticle &src_snglmu)
{
  //
  // The code below needs a bit of explanation.
  // Before this code is called, TClonesArray pointed to by
  // HepMCParticles has some size. But they may nor may not be filled by
  // the object of MCHepMCParticle.
  // When ExpandCreate() is called, a new MCHepMCParticle is 
  // created on the TClonesArray at the last positon. If there
  // is not sufficient space in the TClonesArray, the TClonesArray
  // is expaneded to store the new MCHepMCParticle object. Since
  // the creation is done by default ctor of MCHepMCParticle, the
  // data of "src_snglmu" have to be copied to the newly created
  // object.
  //
  //

  if( !MCHepMCParticles) return NULL;

  // this is a bit ugly but GetLast returns the index-1.
  //
  int nnew = MCHepMCParticles->GetLast()+2;
  //  cout << "nnew="<<nnew<<endl;
  MCHepMCParticles->ExpandCreate(nnew);
  //
  // ExpandCreate calls default ctor. So, the contents of src_snglmu
  // should be copied to newsnglmu

  MCHepMCParticle *newsnglmu = (MCHepMCParticle*)(MCHepMCParticles->UncheckedAt(MCHepMCParticles->GetLast()));
  *newsnglmu = src_snglmu;
  nMCHepMCParticles++;

  return newsnglmu;
}

void
MCHepMCParticleContainer::copyto(MCHepMCParticleContainer& dest) const
{
  delete dest.MCHepMCParticles;
  dest.MCHepMCParticles  = new TClonesArray("MCHepMCParticle",nMCHepMCParticles);
  dest.nMCHepMCParticles = nMCHepMCParticles;  

  for( unsigned int i = 0; i < nMCHepMCParticles; ++i) {
    MCHepMCParticle* src = static_cast<MCHepMCParticle*>(get_MCHepMCParticle(i));
    if(src) {
      dest.AddMCHepMCParticle(*src);
    } else {
      cerr << PHWHERE << "src single muon is null?" <<endl;
    }
  }
}

MCHepMCParticleContainer *
MCHepMCParticleContainer::clone() const
{
  return new MCHepMCParticleContainer(*this);
}
