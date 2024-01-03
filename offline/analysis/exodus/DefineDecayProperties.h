#ifndef DefineDecayProperties_h
#define DefineDecayProperties_h

#include <string>
#include "Tools.h"

class DecayList;
class ParticlePropertyList;

DecayList* DefineDecayProperties(const ParticlePropertyList& PPList,
    const std::string& decayfile = getDataFileName("defined_decays.txt"));

#endif
