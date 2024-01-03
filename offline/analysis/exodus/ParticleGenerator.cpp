#include "ParticleGenerator.h"

ParticleGenerator::ParticleGenerator(const int id, const double weight) :
  itsID(id), itsWeight(weight),
  itsPtYFunction(0), itsPtHistogram(0), itsYHistogram(0), itsMHistogram(0) {}
