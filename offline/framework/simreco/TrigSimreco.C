#include "TrigSimreco.h"
#include <Fun4AllReturnCodes.h>

#include <TrigLvl1.h>

#include <getClass.h>
#include <PHCompositeNode.h>

#include <iostream>

using namespace std;

TrigSimreco::TrigSimreco(const char *name): TrigReco(name)
{
  return ;
}

int
TrigSimreco::process_event(PHCompositeNode *topNode)
{
  TrigLvl1* triglvl1 = findNode::getClass<TrigLvl1>(topNode, "TrigLvl1");
  for (int i = 0;i < 2;i++)
    {
      triglvl1->set_lvl1_beam_clk(0, i);
    }
  triglvl1->set_lvl1_trigraw(0x8FFFFFFF); // all bits except ppg trigger
  triglvl1->set_lvl1_triglive(0x8FFFFFFF); // all bits except ppg trigger
  triglvl1->set_lvl1_trigscaled(0x8FFFFFFF); // all bits except ppg trigger
  triglvl1->set_lvl1_clock_cross(0);
  if (verbosity > 0)
    {
      triglvl1->identify();
    }
  return 0;
}

