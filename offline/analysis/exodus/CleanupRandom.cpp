#include <TRandom.h>
#include <iostream>
#include <string>
#include "CleanupRandom.h"

void CleanupRandom()
{
  const unsigned int seed = gRandom->GetSeed();
  std::cout << std::endl;
  std::cout << "Seed used in this run: " << seed << std::endl;

  return;
}
