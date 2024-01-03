//-----------------------------------------------------------------------------
//
//  Close the input stream
//
//-----------------------------------------------------------------------------

#include <fstream>
#include "CloseInputFile.h"

void CloseInputFile(std::ifstream& input_file)
{
  input_file.close();
}
