#ifndef ThreeBodyDecay_h
#define ThreeBodyDecay_h

class Particle;
class ParticlePropertyList;
class Decay;

void ThreeBodyDecay(Particle& PParent,
    Particle& PChild1,
    Particle& PChild2,
    Particle& PChild3,
    const ParticlePropertyList& PPList,
    const Decay& PDecay);

#endif
