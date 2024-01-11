#include <Lvl2DecisionOutv1.h>
#include <string>
#include <cassert>

using namespace std;

ClassImp(Lvl2DecisionOutv1)

Lvl2DecisionOutv1::Lvl2DecisionOutv1()
{
  Clear();
}

Lvl2DecisionOutv1::~Lvl2DecisionOutv1()
{
}

void Lvl2DecisionOutv1::setLevel1TriggerDecision(UINT ilevel1, UINT decision)
{
  assert( ilevel1 < MaxNumLvl1Triggers );
  _lvl1Decision[ilevel1] = decision;
}

void Lvl2DecisionOutv1::setAlgorithmDecision(UINT ialg, UINT decision)
{
  assert( ialg < MaxNumAlgorithms );
  _algorithmDecision[ialg] = decision;
}

void Lvl2DecisionOutv1::setLvl1AlgorithmDecision(UINT ilevel1, UINT ialg, UINT decision)
{
  assert( (ialg<MaxNumAlgorithms) && (ilevel1<MaxNumLvl1Triggers) );
  _lvl1AlgorithmDecision[ilevel1][ialg] = decision;
}

void Lvl2DecisionOutv1::Reset()
{
  Clear();
}

void Lvl2DecisionOutv1::Clear(const Option_t* /*opt*/)
{
  // set everything to 0
  _finalDecision = 0;
  _numLvl1Triggers = 0;
  for (int ialg=0; ialg<MaxNumAlgorithms; ialg++)
    {
      _algorithmDecision[ialg] = 0;
    }
  for (int ilevel1=0; ilevel1<MaxNumLvl1Triggers; ilevel1++)
    {
      _lvl1Decision[ilevel1] = 0;
      for (int ialg=0; ialg<MaxNumAlgorithms; ialg++)
        {
          _lvl1AlgorithmDecision[ilevel1][ialg] = 0;
        }
    }
}

void Lvl2DecisionOutv1::dump(ostream& os) const
{
  //
  //  Start with the full decision
  //
  os << "Final Level2 decision: " << _finalDecision
     << ", Number of Level-1 triggers: " << _numLvl1Triggers
     << endl;

  for (UINT itrig = 0; itrig < MaxNumLvl1Triggers; itrig++)
  {
    if (_lvl1Decision[itrig] != 0) {
      os << "Level1 Trigger bit: " << itrig
         << ", Decision: " << hex << _lvl1Decision[itrig] << dec << endl;

      //  Now get the algorithms for this level-1 trigger
      //
      for (UINT ialg = 0; ialg < MaxNumAlgorithms; ialg++)
      {
        if (_lvl1AlgorithmDecision[itrig][ialg] != 0) {
          os << "Algorithm index: " << ialg
             << ", decision: " << hex << _lvl1AlgorithmDecision[itrig][ialg] << dec << endl;
        }
      }
    }
  }

  os << endl;
}
