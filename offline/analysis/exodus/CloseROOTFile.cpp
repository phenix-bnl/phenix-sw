//-----------------------------------------------------------------------------
//
//  Save ROOT objects in ROOT output file and close it
//
//-----------------------------------------------------------------------------

#include <TFile.h>
#include <iostream>
#include <string>
#include "CloseROOTFile.h"

void CloseROOTFile(TFile& root_file)
{
  std::cout << "Saving ROOT objects and closing output file" << std::endl;

  root_file.Write();
  root_file.Close();
}
