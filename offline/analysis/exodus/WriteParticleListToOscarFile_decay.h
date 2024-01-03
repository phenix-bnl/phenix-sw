#ifndef WriteParticleListToOscarFile_decay_h
#define WriteParticleListToOscarFile_decay_h

class ParticleList;
class ParticlePropertyList;

void WriteParticleListToOscarFile_decay(const char* file, const ParticleList& PList,
    const ParticlePropertyList& PPList);

#endif
