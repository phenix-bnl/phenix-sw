#ifndef GetEventFromInputStream_h
#define GetEventFromInputStream_h

#include <fstream>

class ParticleList;

ParticleList* GetEventFromInputStream(std::ifstream* input_file);

#endif
