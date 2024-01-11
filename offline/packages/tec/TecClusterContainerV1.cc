#include <TecClusterContainerV1.hh>
#include <phool.h>
#include <TClonesArray.h>
#include <iostream>
#include <TecClusterV1.hh>

ClassImp(TecClusterContainerV1)

using namespace std;

TecClusterContainerV1::TecClusterContainerV1()
{
  NumberOfClusters = 0;
  TecClusters = new TClonesArray("TecClusterV1", TECMAXNUMCLUSTERS);
}

TecClusterContainerV1::~TecClusterContainerV1()
{
  Clear();
  delete TecClusters;
}

void
TecClusterContainerV1::Clear(Option_t *option)
{
  TecClusters->Clear();
  if (TecClusters->GetSize() > TECMAXNUMCLUSTERS)
    {
      TecClusters->Expand(TECMAXNUMCLUSTERS);
    }
  NumberOfClusters = 0;
}

int
TecClusterContainerV1::isValid() const
{
  if (getNClusters() > 0)
    {
      return 1;
    }
  return 0;
}

void
TecClusterContainerV1::Reset()
{
  Clear();
}

int
TecClusterContainerV1::AddTecCluster(TecClusterV1 &source)
{
  if (NumberOfClusters < TecClusters->GetSize())
    {
      TClonesArray &tecclusters = *TecClusters;
      new(tecclusters[NumberOfClusters]) TecClusterV1(source);
      NumberOfClusters++;
    }
  else
    {
      int MaxNumberOfClusters = TecClusters->GetSize() + TECMAXNUMCLUSTERS;
      TecClusters->Expand(MaxNumberOfClusters);
      TClonesArray &tecclusters = *TecClusters;
      new(tecclusters[NumberOfClusters]) TecClusterV1(source);
      NumberOfClusters++;
    }
  
  return NumberOfClusters;
}

void
TecClusterContainerV1::identify(ostream& out) const
{
  out << "I am a TecClusterContainerV1 object." << endl;
}
