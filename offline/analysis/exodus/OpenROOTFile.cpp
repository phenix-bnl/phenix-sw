//-----------------------------------------------------------------------------
//
//  Open ROOT output file
//
//-----------------------------------------------------------------------------

#include <TFile.h>
#include <iostream>
#include <string>
#include "OpenROOTFile.h"

TFile * OpenROOTFile(const char *output_file)
{
  std::cout << "Opening ROOT output file: "  << output_file << std::endl;

  TFile * hfile = new TFile(output_file,"RECREATE","Demo ROOT file");

  return hfile;
}
