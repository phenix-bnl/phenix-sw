#ifndef DoubleDalitzDecay_h
#define DoubleDalitzDecay_h

class Decay;
class Particle;
class ParticlePropertyList;

void DoubleDalitzDecay(Particle& PParent,
    Particle& PLepton1,
    Particle& PLepton2,
    Particle& PLepton3,
    Particle& PLepton4,
    const ParticlePropertyList& PPList,
    const Decay& PDecay);

#endif
