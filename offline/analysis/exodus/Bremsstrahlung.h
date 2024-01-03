#ifndef Bremsstrahlung_h
#define Bremsstrahlung_h

#include <list>
#include <utility>

class Particle;
class ParticleList;

namespace Bremsstrahlung {
  bool InitializeHistograms();

  /* EXTERNAL BREMSSTRAHLUNG
   *
   * Sampling of the particle through a medium and bremsstrahlung.
   *
   * The mean free path and the energy loss of the particle are taken from
   * GEANT4 (ComputeMeanFreepath). As material Berillium was assumed.
   *
   * This class has to be intialized with a radiation length; otherwise a
   * thickness of X=X_0 is assumed.
   *
   */
  class ExternalBremsstrahlung {
    private:
      double thickness; // length of material to travel through for particle; in radiation lengths [g/cm^2]
      double pathLength; // the length we can still travel; this gets changed as particle is propagated; in radiation lengths [g/cm^2]
      bool isHappening(const double energy);
      std::pair<Particle,Particle> sampleSecondaries(const Particle& electron);
      bool sample(const Particle& electron);

    public:
      explicit ExternalBremsstrahlung(const double thickness_=1);
      bool run(const Particle& particle);
      void loop(ParticleList& event);
      double getFreePath(const double energy) const;
      void setThickness(const double thickness); // set the material thickness in radiation lengths [g/cm^2]

      std::list<Particle> secondaries;
  };

  /* INTERNAL BREMSSTRAHLUNG
   *
   * A very simple implementation of interaction in the final state
   * electron-positron of a J/Psi decay (internal bremsstrahlung).
   *
   * The cross section is calculated with the same technique as used in the
   * Run5 J/Psi analysis (PPG069), which in turn references [1]. The energy
   * loss of the single electrons assumed to have the same form as for external
   * bremsstrahlung.
   *
   * At maximum one electron/positron will be allowed to radiate.
   *
   * This function expects an electron-positron pair as arguments (in any
   * order); the particles are modified in place.
   *
   * A bool flag indicating whether this pair was affected or not is returned.
   *
   * ----
   * [1] "Bremsstrahlung in Leptonic Onia Decays", A.Spiridonov, 2005,
   *      hep-ex/510076v1
   *
   */
  bool InternalBremsstrahlung(Particle& p1, Particle& p2);
}

#endif
