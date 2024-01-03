#ifndef TwoBodyDecay_h
#define TwoBodyDecay_h

class Particle;
class ParticlePropertyList;
class Decay;

void TwoBodyDecay(Particle& PParent,
    Particle& PChild1,
    Particle& PChild2,
    const ParticlePropertyList& PPList,
    const Decay& PDecay);

#endif
