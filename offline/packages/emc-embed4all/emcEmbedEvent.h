#ifndef __EMCEMBEDEVENT_H__
#define __EMCEMBEDEVENT_H__

#include "PHObject.h"

/** A small wrapper class to hold information about the merged event.
    Contains the characteristics of the primary particle(s), and
    the vertices (real and simulated).
*/

class emcEmbedEvent : public PHObject
{
 public:

  emcEmbedEvent();
  virtual ~emcEmbedEvent();

  emcEmbedEvent* clone() const { warning("clone"); return 0; }

  /// Energy of i-th primary particle (i>=0 && <size())
  virtual double energy(unsigned int i) const { warning("energy"); return 0; }

  /// Mass of i-th primary particle.
  virtual double mass(unsigned int i) const;

  /// Total momentum of i-th primary particle.
  virtual double momentum(unsigned int i) const;

  /// PDG particle code.
  virtual int pid(unsigned int i) const { warning("pid"); return 0; }

  /// Pt of i-th primary particle.
  virtual double pt(unsigned int i) const;

  /// Px of i-th primary particle.
  virtual double px(unsigned int i) const { warning("px"); return 0; }

  /// Py of i-th primary particle.
  virtual double py(unsigned int i) const { warning("py"); return 0; }

  /// Pz of i-th primary particle.
  virtual double pz(unsigned int i) const { warning("pz"); return 0; }
 
  ///
  void identify(std::ostream& os = std::cout) const { warning("identify"); }

  ///
  virtual int isValid() const { warning("isValid"); return 0; }

  ///
  virtual void print(std::ostream& os = std::cout) const { warning("print"); }

  ///
  void Reset() { warning("Reset"); }

  /// Set the information for i-th particle.
  virtual void set_primary(unsigned int i,
			   int pid, double energy, 
			   double px, double py, double pz,
			   double zreal, double zsimu)
  { warning("set_primary"); }

  /// Number of primary particles.
  virtual size_t size() const { warning("size"); return 0; }

  /// Real vertex.
  virtual double zreal(unsigned int i) const { warning("zreal"); return 0; }

  /// Simulated vertex.
  virtual double zsimu(unsigned int i) const { warning("zsimu"); return 0; }

 private:

  void warning(const char* method) const;

  ClassDef(emcEmbedEvent,0)
};

#endif
