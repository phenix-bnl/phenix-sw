//-----------------------------------------------------------------------------
//
//  Open an ASCII output file and return stream
//
//-----------------------------------------------------------------------------

#include "OpenFullEventFile.h"
#include <fstream>
#include <iostream>
#include <string>

std::ofstream * OpenFullEventFile(const char * file)
{
  std::cout << "Opening output file: " << file << std::endl;
  std::cout << std::endl;

  std::ofstream * output_file = new std::ofstream;
  output_file->open(file);

  *output_file << "# OSC1999A" << std::endl;
  *output_file << "# final_id_p_x" << std::endl;
  *output_file << "# EXODUS event generator in full event mode" << std::endl;
  *output_file << "#" << std::endl;

  output_file->precision(5);

  return (output_file);
}
