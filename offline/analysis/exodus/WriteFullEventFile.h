#ifndef WriteFullEventFile_h
#define WriteFullEventFile_h

#include <fstream>

class ParticleList;
class ParticlePropertyList;

void WriteFullEventFile(std::ofstream& fout, const int eventID,
    const ParticleList& PList,
    const ParticlePropertyList& PPList);

#endif
