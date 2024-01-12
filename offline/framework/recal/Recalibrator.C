#include "Recalibrator.h"
#include "MasterRecalibrator.h"

#include <iostream>

using namespace std;

Recalibrator::Recalibrator(const string &name): 
  SubsysReco(name),
  myclassname(name),
  order(0),
  fillhistos(0)
{
  return;
}

void
Recalibrator::Print(const string &what) const
{
  cout << "Recalibrator " << ThisName << ", order "
       << order << ": working on base class: " 
       << mybaseclass << " from Node: " << inputnodename 
       << endl;
  return;
}

void
Recalibrator::SetMasterRecalibrator(MasterRecalibrator* theMasterRecal)
{
  masterRecal = theMasterRecal;
}
