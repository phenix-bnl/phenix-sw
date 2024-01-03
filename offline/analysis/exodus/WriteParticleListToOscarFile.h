#ifndef WriteParticleListToOscarFile_h
#define WriteParticleListToOscarFile_h

class ParticleList;
class ParticlePropertyList;

void WriteParticleListToOscarFile(const char* file, const ParticleList& PList,
    const ParticlePropertyList& PPList);

#endif
