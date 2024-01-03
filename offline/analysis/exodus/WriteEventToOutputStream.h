#ifndef WriteEventToOutputStream_h
#define WriteEventToOutputStream_h

#include <fstream>

class ParticleList;

void WriteEventToOutputStream(std::ofstream * output_file, const ParticleList& PList);

#endif
