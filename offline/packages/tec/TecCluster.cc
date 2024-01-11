#include "TecCluster.hh"
#include <iostream>

ClassImp(TecCluster)

using namespace std;

void TecCluster::identify(ostream& out) const {
  out << "I am a TecCluster object." << endl;
}

