#ifndef PHENIXFiducial_h
#define PHENIXFiducial_h

class Particle;
class ParticlePropertyList;

int PHENIXFiducial(const int fieldSetting,
    const double pt_cut, const double vtx_cut,
    const Particle& PParticle, const ParticlePropertyList& PPList);

#endif
