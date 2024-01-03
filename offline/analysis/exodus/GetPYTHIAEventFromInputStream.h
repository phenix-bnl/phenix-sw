#ifndef GetPYTHIAEventFromInputStream_h
#define GetPYTHIAEventFromInputStream_h

#include <fstream>

class ParticleList;

ParticleList* GetPYTHIAEventFromInputStream(std::ifstream*, const int);

#endif
