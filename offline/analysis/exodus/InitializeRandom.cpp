#include <TRandom.h>
#include <TRandom3.h>
#include "InitializeRandom.h"

void InitializeRandom()
{
  gRandom = new TRandom3(0);
}
