#ifndef PHHEPMCCOMBINER_h
#define PHHEPMCCOMBINER_h

#include <string>

namespace HepMC
{
  class GenEvent;
  class GenParticle;
  class GenVertex;
};

//! shifts particles vertex based on values read from a file
class PHHepMCCombiner
{
  
 public:
  
  //! constructor
  PHHepMCCombiner() {}
  
  //! destructor
  virtual ~PHHepMCCombiner() {}

  void combineEvents(const HepMC::GenEvent *inputEvent, HepMC::GenEvent *outputEvent);

 private:

  int addParticle(const HepMC::GenParticle* p, HepMC::GenEvent* ev);
  int addVertex(const HepMC::GenVertex* v, HepMC::GenEvent* ev);


};

#endif
