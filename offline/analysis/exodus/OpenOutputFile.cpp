//-----------------------------------------------------------------------------
//
//  Open an ASCII output file and return stream
//
//-----------------------------------------------------------------------------

#include <fstream>
#include <iostream>
#include <string>
#include "OpenOutputFile.h"

std::ofstream * OpenOutputFile(const char * file)
{
  std::cout << "Opening output file: " << file << std::endl;
  std::cout << std::endl;

  std::ofstream * output_file = new std::ofstream;;
  output_file->open(file);

  return (output_file);
}
