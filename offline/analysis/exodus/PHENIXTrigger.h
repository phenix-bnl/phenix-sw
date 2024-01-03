#ifndef PHENIXTrigger_h
#define PHENIXTrigger_h

class Particle;
class ParticlePropertyList;

double PHENIXTrigger(const int fieldSetting,
    const double pt_cut, const double vtx_cut,
    const Particle& PParticle, const ParticlePropertyList& PPList);

#endif
