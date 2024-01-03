#include <TF1.h>
#include <limits>
#include <stdexcept>
#include "AdjustDecaySum.h"
#include "Bremsstrahlung.h"
#include "CleanupRandom.h"
#include "DecayList.h"
#include "DecayMachine.h"
#include "DefineDecayProperties.h"
#include "DefineParticleProperties.h"
#include "DoAllDecays.h"
#include "GenerateFullEvent.h"
#include "GenerateFullEventFlow.h"
#include "GenerateParticles.h"
#include "InitializeRandom.h"
#include "InitializeSetup.h"
#include "ParticleGeneratorList.h"
#include "ParticlePropertyList.h"
#include "ParticleList.h"
// initialize everything for v2 like azimuthal distribution
static TF1* f_dNdphi = new TF1("f_dNdphi","1.+2*[0]*cos(2*x)",0.,6.2832);
static TF1* f_v2 = new TF1("f_v2","0.0875*x-0.015*x*x",0.,3.);

DecayMachine::DecayMachine():
  setup(0),
  PGList(0),
  event(0),
  useFlow(false),
  dNch_dy(1),
  vertexRange(std::numeric_limits<double>::max())
{
  InitializeRandom();
  PPList.reset(DefineParticleProperties());
  Decays.reset(DefineDecayProperties(*PPList));
}

DecayMachine::~DecayMachine() {
  CleanupRandom();
}

DecayMachine& DecayMachine::getInstance() {
  static DecayMachine Instance;
  return Instance;
}

ParticlePropertyList& DecayMachine::getParticlePropertyList() {
  return *PPList;
}

void DecayMachine::setGenerator(int setup_) {
  PGList.reset(InitializeSetup(setup_, *PPList));
  setup = setup_;
}

void DecayMachine::setFlow(const bool useFlow_) {
  useFlow = useFlow_;
}

DecayList& DecayMachine::getDecayProperties() {
  return *Decays;
}

ParticleList& DecayMachine::run() {
  event.reset(new ParticleList);

  if ( setup==0 ) { throw std::runtime_error("no setup selected"); }

  if ( Decays==0 ) { throw std::runtime_error("no decays defined"); }

  if ( setup!=7 ) {
    GenerateParticles(event.get(), 1, PGList.get(), PPList.get());
  }
  else
  {
    if (useFlow) {
      GenerateFullEventFlow(*event, dNch_dy, vertexRange, *PGList, *PPList, *f_dNdphi, *f_v2);
    } else {
      GenerateFullEvent(event.get(), dNch_dy, vertexRange, PGList.get(), PPList.get());
    }
  }

  AdjustDecaySum(*event, *Decays);
  DoAllDecays(*event, *PPList, *Decays);
  static Bremsstrahlung::ExternalBremsstrahlung brems(.006); // use 0.6% thickness of beam pipe
  brems.loop(*event);
  return *event;
}

void DecayMachine::setdNch_dy(const int dNch_dy_) {
  dNch_dy = dNch_dy_;
}

void DecayMachine::setVertexRange(const double range) {
  vertexRange = range;
}
