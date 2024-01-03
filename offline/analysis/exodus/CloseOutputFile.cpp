//-----------------------------------------------------------------------------
//
//  Close the output stream
//
//-----------------------------------------------------------------------------

#include <fstream>
#include "CloseOutputFile.h"

void CloseOutputFile(std::ofstream& output_file)
{
  output_file.close();
}
