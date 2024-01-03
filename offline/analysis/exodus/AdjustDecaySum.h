#ifndef AdjustDecaySum_h
#define AdjustDecaySum_h

class ParticleList;
class DecayList;
class Particle;

void AdjustDecaySum(ParticleList& PList, const DecayList& DList);
void AdjustDecaySum(Particle& PParticle, const DecayList& DList);

#endif
