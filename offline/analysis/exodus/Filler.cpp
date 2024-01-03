#include <cassert>
#include <limits>
#include <stdexcept>
#include "BookROOTObjects.h"
#include "CloseFullEventFile.h"
#include "CloseROOTFile.h"
#include "DecayMachine.h"
#include "FillROOTObjects.h"
#include "Filler.h"
#include "OpenFullEventFile.h"
#include "OpenROOTFile.h"
#include "WriteFullEventFile.h"

class ParticlePropertyList;

Filler& Filler::getInstance() {
  static Filler Instance;
  return Instance;
}

Filler::Filler():
  fill_primaries      (false),
  fill_singles(false),
  fill_single_photon  (false),
  fill_electron_pair  (false),
  setup(-100),
  Nevents(0),
  dNdy_pi0(1),
  N_coll(1),
  PPList(0),
  root_file(0),
  oscar_file(0),
  outputFormat(NONE)
{ ; }

void Filler::Close() {
  switch(outputFormat) {
    case root:
      CloseROOTFile(*root_file);
      break;
    case oscar:
      CloseFullEventFile(*oscar_file);
      break;
    default:
      break;
  }
}

void Filler::setFillPrimaries(const bool fill_primaries_) {
  fill_primaries = fill_primaries_;
}

void Filler::setFillSingles(const bool fill_singles_) {
  fill_singles = fill_singles_;
}

void Filler::setFillSinglePhoton(const bool fill_single_photon_) {
  fill_single_photon = fill_single_photon_;
}

void Filler::setFillElectronPair(const bool fill_electron_pair_) {
  fill_electron_pair = fill_electron_pair_;
}

void Filler::setSetup(const int setup_) {
  setup = setup_;
}

void Filler::setNevents(const unsigned int Nevents_) {
  Nevents = Nevents_;
}

void Filler::setdNdy_pi0(const double dNdy_pi0_) {
  dNdy_pi0 = dNdy_pi0_;
}

void Filler::setNcoll(const int N_coll_) {
  N_coll = N_coll_;
}

void Filler::setParticlePropertyList(ParticlePropertyList* PPList_) {
  PPList = PPList_;
}

void Filler::setOutputFile(const std::string& output_file_) {
  if (output_file_.rfind(".root") < std::numeric_limits<unsigned int>::max()) {
    setOutputFormat(root);
    root_file = OpenROOTFile(output_file_.c_str());
    initDefaultROOTObjects();
  }
  else if (output_file_.rfind(".oscar") < std::numeric_limits<unsigned int>::max()) {
    setOutputFormat(oscar);
    oscar_file = OpenFullEventFile(output_file_.c_str());
  } else {
    throw std::runtime_error("Filler: output format not understood");
  }

  output_file = output_file_;
}

void Filler::initDefaultROOTObjects() {
  if (not root_file) {
    throw std::runtime_error("Filler: no ROOT file opened yet");
  }
  BookROOTObjects();
}

void Filler::Fill(const ParticleList& PList, const unsigned int evtnum) {
  if (not PPList) {
    PPList = &DecayMachine::getInstance().getParticlePropertyList();
  }

  switch(outputFormat) {
    case oscar:
      WriteFullEventFile(*oscar_file,static_cast<int>(evtnum),PList,*PPList);
      break;
    case root:
      if (setup == -100) {
        throw std::runtime_error("Filler: setup not set");
      }
      if (dNdy_pi0+100 < std::numeric_limits<double>::epsilon()) {
        throw std::runtime_error("Filler: dNdy_pi0 not set");
      }
      if (N_coll+100 < std::numeric_limits<double>::epsilon()) {
        throw std::runtime_error("Filler: N_coll not set");
      }
      if (output_file.empty()) {
        throw std::runtime_error("Filler: output_file not set");
      }
      FillROOTObjects(setup, Nevents, dNdy_pi0, N_coll, PList, *PPList, fill_primaries, fill_singles, fill_electron_pair, fill_single_photon);
      break;
    default:
      assert(false); // Should never reach this
  }
}

void Filler::setOutputFormat(const FileFormat format) {
  outputFormat = format;
}
