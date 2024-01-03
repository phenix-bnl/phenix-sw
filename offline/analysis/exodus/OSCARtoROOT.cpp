//-----------------------------------------------------------------------------
//
//  Translate OSCAR compliant ASCII file into a ROOT Ntuple
//
//-----------------------------------------------------------------------------

#include <TFile.h>
#include <TNtuple.h>
#include <TROOT.h>
#include <boost/lexical_cast.hpp>
#include <cmath>
#include <fstream>
#include <iostream>
#include <limits>
#include <string>
#include <gsl/gsl_math.h>

TROOT OSCARtoROOT("OSCARtoROOT","Converter OSCAR to ROOT");

//____________________________________________________________________________________
int main() try
{
  std::cout << std::endl << std::endl;
  std::cout << "**********************************" << std::endl;
  std::cout << "*                                *" << std::endl;
  std::cout << "*  W E L C O M E to OSCARtoROOT  *" << std::endl;
  std::cout << "*                                *" << std::endl;
  std::cout << "*   Translate OSCAR compliant    *" << std::endl;
  std::cout << "* ASCII file into a ROOT Ntuple  *" << std::endl;
  std::cout << "*                                *" << std::endl;
  std::cout << "**********************************" << std::endl;
  std::cout << std::endl << std::endl;

  std::string input_file;
  std::cout << "OSCAR compliant input file: ";
  std::getline(std::cin, input_file);
  std::cout << std::endl;

  std::string output_file;
  std::cout << "Output file containing ROOT Ntuple: ";
  std::getline(std::cin, output_file);
  std::cout << std::endl;

  std::string line;
  std::cout << "Number of events to convert (0=all): ";
  std::getline(std::cin, line);
  const int max_events = boost::lexical_cast<int>(line);
  std::cout << std::endl;

  std::cout << "Opening ROOT output file: "  << output_file << std::endl;
  TFile* hfile = new TFile(output_file.c_str(),"RECREATE","ROOT file");

  TNtuple *particle = new TNtuple("particle","primary particle ntuple",
      "event:pnum:pid:px:py:pz:E:mass:xvtx:yvtx:zvtx:theta:phi:rap:eta");

  std::cout << "Opening input file: " << input_file << std::endl;
  std::cout << std::endl;
  std::ifstream input_stream(input_file.c_str());
  if( !input_stream )
  {
    std::cout << "OSCARtoROOT::main - unable to open file " << input_file << std::endl;
    return 0;
  }

  // read header
  for (unsigned int i=0; i<4; ++i) {
    std::getline(input_stream, line);
    std::cout << line << std::endl;
  }

  // read events
  int event_id = 0;
  bool file_end = false;
  do
  {
    int particles_per_event;
    int zero;
    input_stream >> zero;
    input_stream >> particles_per_event;
    if ( particles_per_event!=0 )
    {
      event_id++;
      for (int iparticle=0; iparticle<particles_per_event; iparticle++)
      {
        float px, py, pz, E, mass, xvtx, yvtx, zvtx, opt;
        float theta, phi, rap, eta, mom;
        int pnum, pid;
        input_stream >> pnum;
        input_stream >> pid;
        input_stream >> zero;
        input_stream >> px;
        input_stream >> py;
        input_stream >> pz;
        input_stream >> E;
        input_stream >> mass;
        input_stream >> xvtx;
        input_stream >> yvtx;
        input_stream >> zvtx;
        input_stream >> opt;
 //       std::cout<<pnum<<" "<<pid<<" "<<opt<<std::endl;

        phi = std::atan2(py,px);
        if ( py<0.0 ) phi=phi+static_cast<float>(2*M_PI);
        mom = std::sqrt(px*px+py*py+pz*pz);
        if ( mom > std::numeric_limits<double>::epsilon())
        {
          theta = std::acos(pz/mom);
        }
        else theta = 0.0;
        eta = -std::log(std::tan(theta/2.0f));
        if ( std::abs(E-pz) > std::numeric_limits<double>::epsilon() )
          rap = std::log((E+pz)/(E-pz))/2.0f;
        else
          rap = 0.0;
        pnum = particles_per_event - 1;

        particle->Fill(
            static_cast<float>(event_id),
            static_cast<float>(pnum),
            static_cast<float>(pid),
            px,py,pz,E,mass,xvtx,yvtx,zvtx,
            theta,phi,rap,eta);
      }
      input_stream >> particles_per_event;
      input_stream >> zero;

      if ( particles_per_event!=0 || zero!=0 )
      {
        std::cout << "OSCARtoROOT::main - error in OSCAR file!"
          << " particles_per_event: " << particles_per_event
          << " zero: " << zero
          << std::endl;
        return 0;
      }

    } else {
      file_end=true;
    }
    if ( event_id==max_events && max_events!=0 ) file_end=true;
  }
  while ( !file_end );

  std::cout << "Saving ROOT Ntuple and closing file" << std::endl;
  hfile->Write();
  hfile->Close();
  hfile = 0;

  return 0;
} catch (const std::exception& e) {
  std::cerr << e.what() << std::endl;
  return EXIT_FAILURE;
}
