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
#include <iostream>
#include <fstream>
#include <iostream>
#include <limits>
#include <string>

TROOT OSCARtoROOT_fullevent("OSCARtoROOT_fullevent","Converter OSCAR to ROOT");

//____________________________________________________________________________________
int main() try
{
  std::cout << std::endl << std::endl;
  std::cout << "********************************************" << std::endl;
  std::cout << "*                                          *" << std::endl;
  std::cout << "*  W E L C O M E to OSCARtoROOT_fullevent  *" << std::endl;
  std::cout << "*                                          *" << std::endl;
  std::cout << "*        Translate OSCAR compliant         *" << std::endl;
  std::cout << "*      ASCII file into a ROOT Ntuple       *" << std::endl;
  std::cout << "*                                          *" << std::endl;
  std::cout << "********************************************" << std::endl;
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
      "event:pnum:pid:px:py:pz:E:mass:xvtx:yvtx:zvtx:theta:phi:rap:eta:pindex:weight:parentid");

  std::cout << "Opening input file: " << input_file << std::endl;
  std::cout << std::endl;
  std::ifstream* input_stream = new std::ifstream;;
  input_stream->open(input_file.c_str());

  char character;
  if ( input_stream->get(character) )
  {
    std::string line;
    *input_stream >> line;
    if ( line!="OSC1999A" ) std::cout << "error in OSCAR file!" << std::endl;
    for ( int idummy=0; idummy<11; idummy++) *input_stream >> line;
  }

  float weight = 0.;
  float parent = 0.;
  int event_id = 0;
  bool file_end = false;
  while ( !input_stream->eof() && file_end==false )
  {
    int particles_per_event, zero;
    *input_stream >> zero;
    *input_stream >> particles_per_event;
    if ( particles_per_event>=0 )
    {
      for (int iparticle=0; iparticle<particles_per_event; iparticle++)
      {
        int   pnum, pid, index;
        float px, py, pz, E, mass, xvtx, yvtx, zvtx;
        float theta, phi, rap, eta, mom;
        *input_stream >> pnum;
        *input_stream >> pid;
        *input_stream >> event_id;
        *input_stream >> px;
        *input_stream >> py;
        *input_stream >> pz;
        *input_stream >> E;
        *input_stream >> mass;
        *input_stream >> xvtx;
        *input_stream >> yvtx;
        *input_stream >> zvtx;
        *input_stream >> index;

        phi = std::atan2(py,px);
        if ( py<0.0 ) phi = phi + static_cast<float>(2.0*M_PI);
        mom = std::sqrt(px*px+py*py+pz*pz);
        if ( mom > std::numeric_limits<float>::epsilon() )
        {
          theta = std::acos(pz/mom);
        }
        else theta = 0.0;
        eta = -std::log(std::tan(theta/2.0f));
        if ( std::abs(E-pz) > std::numeric_limits<float>::epsilon() ) {
          rap = std::log((E+pz)/(E-pz))/2.0f;
        } else {
          rap = 0.0;
        }
        float array[18];
        array[ 0] = static_cast<float>(event_id);
        array[ 1] = static_cast<float>(pnum);
        array[ 2] = static_cast<float>(pid);
        array[ 3] = static_cast<float>(px);
        array[ 4] = static_cast<float>(py);
        array[ 5] = static_cast<float>(pz);
        array[ 6] = static_cast<float>(E);
        array[ 7] = static_cast<float>(mass);
        array[ 8] = static_cast<float>(xvtx);
        array[ 9] = static_cast<float>(yvtx);
        array[10] = static_cast<float>(zvtx);
        array[11] = static_cast<float>(theta);
        array[12] = static_cast<float>(phi);
        array[13] = static_cast<float>(rap);
        array[14] = static_cast<float>(eta);
        array[15] = static_cast<float>(index);
        array[16] = static_cast<float>(weight);
        array[17] = static_cast<float>(parent);
        particle->Fill(array);
      }
      *input_stream >> particles_per_event;
      *input_stream >> zero;
      if ( particles_per_event!=0 || zero!=0 )
        std::cout << "error in OSCAR file!" << std::endl;
    }
    if ( event_id==max_events && max_events!=0 ) file_end=true;
  }

  std::cout << "Saving ROOT Ntuple and closing file" << std::endl;
  hfile->Write();
  hfile->Close();
  hfile = 0;

  return 0;
} catch (const std::exception& e) {
  std::cerr << e.what() << std::endl;
  return EXIT_FAILURE;
}
