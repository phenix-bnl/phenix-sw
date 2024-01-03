//-----------------------------------------------------------------------------
//
//  Open an ASCII input file and return stream
//
//-----------------------------------------------------------------------------

#include <fstream>
#include <iostream>
#include <string>
#include "OpenInputFile.h"

std::ifstream * OpenInputFile(const char * file)
{
  std::cout << "Opening input file: " << file << std::endl;
  std::cout << std::endl;

  std::ifstream * input_file = new std::ifstream;;
  input_file->open(file);

  return (input_file);
}
