//-----------------------------------------------------------------------------
//
//  Declaration of the classes Particle
//
//-----------------------------------------------------------------------------

#ifndef PARTICLE_H
#define PARTICLE_H

#include "Momentum.h"

class Particle
{
  public:
    Particle();
    Particle(const int inittype, const Mom4& initmom4, const double initdecaysum,
        const int initGeneration);
    Particle(const int inittype, const double initE,
        const double initpx, const double initpy, const double initpz,
        const double initdecaysum, const int initGeneration);
    int GetID() const {return itsType;}
    const Mom4& Get4mom() const {return itsMom4;}
    Mom4& Get4mom() {return itsMom4;}
    void SetID(const int type){itsType = type;}
    void Set4mom(const Mom4& mom4) {itsMom4 = mom4;}
    void Set4mom(const double E, const double px, const double py, const double pz)
    {
      itsMom4.Set(E, px,py,pz);
    }
    void Set4mom(const double E, const Mom3& p)
    {
      itsMom4.Set(E, p);
    }
    void SetVertex(const double xVTX, const double yVTX, const double zVTX)
    {
      itsVertex[0] = xVTX;
      itsVertex[1] = yVTX;
      itsVertex[2] = zVTX;
    }
    double GetxVertex() const {return itsVertex[0];}
    double GetyVertex() const {return itsVertex[1];}
    double GetzVertex() const {return itsVertex[2];}
    void Show() const;
    double GetDecaysum() const {return itsDecaysum;}
    void SetDecaysum(const double decaysum) {itsDecaysum = decaysum;}
    int GetGeneration() const {return itsGeneration;}
    void SetGeneration(const int Generation) {itsGeneration = Generation;}
    double GetMass() const;
    void SetWeight(const double weight) {itsWeight = weight;}
    double GetWeight() const {return itsWeight;}
    void SetAccept(const int accept) {itsAccept = accept;}
    void SetValid(const int valid){itsValid = valid;}
    int GetValid() const {return itsValid;}
  private:
    int    itsType;
    Mom4   itsMom4;
    double itsVertex[3];
    double itsDecaysum;
    int    itsGeneration;
    double itsWeight;
    int    itsAccept;
    int    itsValid;
};

#endif /* PARTICLE_H */
