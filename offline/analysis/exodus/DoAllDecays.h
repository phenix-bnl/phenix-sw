#ifndef DoAllDecays_h
#define DoAllDecays_h

class ParticleList;
class ParticlePropertyList;
class DecayList;

void DoAllDecays(ParticleList& PList,
    const ParticlePropertyList& PPList,
    const DecayList& DList);

#endif
