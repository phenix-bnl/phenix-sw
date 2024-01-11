#ifndef __PHGENEVENTV1__
#define __PHGENEVENTV1__

#include <phool.h>
#include <PHObject.h>

#include <TString.h>

#include <HepMC/GenEvent.h>

#include <PHGenEvent.h>

class PHGenEventV1 : public PHGenEvent {
  
 public:

  PHGenEventV1();
  PHGenEventV1(unsigned int id, HepMC::GenEvent &event);
  PHGenEventV1(const PHGenEventV1& phevent);
  PHGenEventV1(const PHGenEventV1* phevent);  
  virtual ~PHGenEventV1();

  const HepMC::GenEvent* get_event() const;
  HepMC::GenEvent* get_event();
  TString get_event_record() const {return _event_record;}
  void set_event(HepMC::GenEvent &event);
  void set_event(HepMC::GenEvent* event);

  unsigned int get_id() const {return _id;}
  void set_id(const unsigned int id) {_id = id;}

  // the number of entries in the array of particles
  size_t particles_size() const;
  size_t vertices_size() const;

  // PHObject interface
  void identify(std::ostream& out = std::cout) const {
    out << "PHGenEventV1" << std::endl;
  }
  void print(std::ostream& out = std::cout) const;
  void Reset();
  
private:

  bool stale() const {return _stale;}
  void refresh() const;

  // a unique id to aid in tracing merged events (multiple container entries) 
  unsigned int _id;

  // the ASCII event record of the HepMC event (ROOT responsible for compression)
  // this reduces the central event storage requirement by 41% (Bzip2 achieves 32%)
  // a 2nd version of this class could possibly do better by storing the
  // vertices and particles in classes of their own and rebuilding the HepMC
  // from there. This might make better use of space and ROOT compression.
  TString _event_record;

#ifndef __CINT____ // hide from dictionary generation
  mutable bool _stale;                         //! exclude from ROOT I/O
  mutable HepMC::GenEvent* _event;             //! exclude from ROOT I/O
#endif // __CINT__
  
  ClassDef(PHGenEventV1,1)    
};

#endif	// __PHHEPMCGENEVENTV1__
