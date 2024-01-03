#ifndef DalitzDecay_h
#define DalitzDecay_h

class Particle;
class ParticlePropertyList;
class Decay;

void DalitzDecay(Particle& PParent,
    Particle& PLepton1,
    Particle& PLepton2,
    Particle& POther,
    const ParticlePropertyList& PPList,
    const Decay& PDecay);

#endif
