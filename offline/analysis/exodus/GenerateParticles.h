#ifndef GenerateParticles_h
#define GenerateParticles_h

class ParticleList;
class ParticleGeneratorList;
class ParticlePropertyList;

void GenerateParticles(ParticleList *PList, const int events,
    const ParticleGeneratorList *PGList,
    const ParticlePropertyList *PPList);

#endif
