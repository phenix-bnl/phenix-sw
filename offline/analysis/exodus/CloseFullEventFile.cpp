//-----------------------------------------------------------------------------
//
//  Close the output stream
//
//-----------------------------------------------------------------------------

#include <fstream>
#include "CloseFullEventFile.h"

void CloseFullEventFile(std::ofstream& output_file)
{
  output_file.close();
}
