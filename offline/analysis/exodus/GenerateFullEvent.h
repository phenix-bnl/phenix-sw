#ifndef GenerateFullEvent_h
#define GenerateFullEvent_h

class ParticleList;
class ParticleGeneratorList;
class ParticlePropertyList;

void GenerateFullEvent(ParticleList *PList, const int dnch_dy, const double zVTXmax,
    ParticleGeneratorList *PGList,
    ParticlePropertyList *PPList);

#endif
