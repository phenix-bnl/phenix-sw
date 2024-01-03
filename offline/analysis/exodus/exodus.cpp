//-----------------------------------------------------------------------------
//
//  Decay machine of the EXODUS package
//
//  Main program
//
//-----------------------------------------------------------------------------

#include <Rtypes.h>
#include <TROOT.h>
#include <boost/lexical_cast.hpp>
#include <iostream>
#include <string>
#include "DecayMachine.h"
#include "Filler.h"

class ParticleList;

TROOT exodus("exodus","Initialize ROOT for exodus");

int main() try
{
  gROOT->SetBatch(kTRUE);

  std::cout << std::endl << std::endl;
  std::cout << "**********************************" << std::endl;
  std::cout << "*                                *" << std::endl;
  std::cout << "*  W E L C O M E to E X O D U S  *" << std::endl;
  std::cout << "*                                *" << std::endl;
  std::cout << "*        GENESIS version         *" << std::endl;
  std::cout << "*                                *" << std::endl;
  std::cout << "*                                *" << std::endl;
  std::cout << "*    Wir kriegen alles klein!    *" << std::endl;
  std::cout << "*                                *" << std::endl;
  std::cout << "**********************************" << std::endl;
  std::cout << std::endl << std::endl;

  Filler& filler = Filler::getInstance();
  filler.setFillPrimaries(true);
  filler.setFillSingles(true);
  filler.setFillElectronPair(true);
  filler.setFillSinglePhoton(true);
  DecayMachine& decayMachine = DecayMachine::getInstance();

  std::string line;
  int setup;
  do
  {
    std::cout << "Choose one of the predefined setups:" << std::endl;
    std::cout << "------------------------------------" << std::endl;
    std::cout << std::endl;
    std::cout << "1) CERES" << std::endl;
    std::cout << "2) ISR" << std::endl;
    std::cout << "3) PHENIX electron cocktail" << std::endl;
    std::cout << "4) Phi->KK" << std::endl;
    std::cout << "5) PHENIX single particles: available in exodus_generate only" << std::endl;
    std::cout << "6) PHENIX single particles (EMC): available in exodus_generate only" << std::endl;
    std::cout << "7) PHENIX: complete events" << std::endl;
    std::cout << std::endl;
    std::cout << "Your choice (1-7): ";
    std::getline(std::cin, line);
    setup = boost::lexical_cast<int>(line);
    std::cout << std::endl;
  } while ( setup<1 || setup>7 );

  filler.setSetup(setup);

  std::cout << "How many events? ";
  std::getline(std::cin, line);
  unsigned events = boost::lexical_cast<unsigned>(line);
  std::cout << std::endl;

  while (true) {
    try {
      std::cout << "Output file: (use .root/.oscar extension for desired filetype) ";
      std::string output_file;
      std::getline(std::cin, output_file);
      std::cout << std::endl;
      filler.setOutputFile(output_file);
      break;
    } catch (...) {;}
  }

  if ( setup==3 )
  {
    std::cout << "dN/dy of pizero: ";
    std::getline(std::cin, line);
    const double dNdy_pi0 = boost::lexical_cast<double>(line);
    std::cout << std::endl;
    std::cout << "N_coll: ";
    std::getline(std::cin, line);
    const double N_coll = boost::lexical_cast<double>(line);
    std::cout << std::endl;

    filler.setNevents(events);
    filler.setdNdy_pi0(dNdy_pi0);
    filler.setNcoll(static_cast<int>(N_coll));
  }

  if ( setup==7 ) {
    std::cout << "Select dN_ch/dy at y=0 (integer value): ";
    std::getline(std::cin, line);
    const int dnch_dy = boost::lexical_cast<int>(line);
    std::cout << std::endl;
    decayMachine.setdNch_dy(dnch_dy);
    std::cout << "Select vertex range (+-cm): ";
    std::getline(std::cin, line);
    decayMachine.setVertexRange(boost::lexical_cast<double>(line));
    std::cout << std::endl;
    std::cout << "Elliptic flow (1=yes; 0=no): ";
    std::getline(std::cin, line);
    const bool v2flag = boost::lexical_cast<bool>(line);
    std::cout << std::endl;
    decayMachine.setFlow(v2flag);
  }

  decayMachine.setGenerator(setup);

  for (unsigned int ievent=1; ievent<=events; ievent++ )
  {
    const ParticleList& event = decayMachine.run();
    if ( ievent % 10000 == 0 )
      std::cout << ievent << " events done" << std::endl;

    filler.Fill(event,ievent);
  }
  filler.Close();
} catch (const std::exception& e) {
  std::cerr << e.what() << std::endl;
  return EXIT_FAILURE;
}
