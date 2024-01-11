// $Id: MUTOO.cxx,v 1.2 2019/04/16 02:04:08 slash Exp $

/*!
	\file MUTOO.cxx
	\brief widely used utility functions and enumerations
	\author H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2019/04/16 02:04:08 $
*/

#include "MUTOO.h"
#include <cmath>
#include <algorithm>

//_______________________________________________________
void MUTOO::PRINT(std::ostream& os, const std::string& message)
{
  const int max_col=80;
  if(!message.size()) {
    os << std::string(max_col,'-') << std::endl;
    return;
  }
  int fill = max_col - message.size() - 2;
  int pre = static_cast<int>(std::floor(fill/2.0));
  int post = fill - pre;
  os << std::string(pre,'-') << " ";
  os << message << " ";
  os << std::string(post,'-') << std::endl;  
}

long int MUTOO::global_index(int arm, int station, int gap, int plane, int octant, int half_octant)
{
  return plane + NumberOfPlanes*(half_octant + MAX_HALF_OCTANT *(octant + MAX_OCTANT *(gap + NumberOfGaps*( station + NumberOfStations* arm ))));
}
