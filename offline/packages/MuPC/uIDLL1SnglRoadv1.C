
#include "uIDLL1SnglRoadv1.h"

ClassImp(uIDLL1SnglRoadv1)

uIDLL1SnglRoadv1::uIDLL1SnglRoadv1()
{

  arm = -9999;
  orient = -9999;
  symset = -9999;

  return;
}

uIDLL1SnglRoadv1::uIDLL1SnglRoadv1(uIDLL1SnglRoadv1 *clus)
{

  if (!clus) return;

  arm = clus->get_arm();
  orient = clus->get_orient();
  symset = clus->get_symset();

  return;
}
