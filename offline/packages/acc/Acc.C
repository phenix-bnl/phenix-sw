
#include <iostream>

#include "phool.h"
#include "Acc.h"

namespace ACC {

//_______________________________________________________________
int getBoxID(const int ich)
{
  // get BOX id from PMT id

  if(ich<0 || ich>=ACC_NCH){
    std::cout << PHWHERE << " Invalid channel number, ich= " << ich << std::endl;
    return -1;
  }
  
  int column = ich/40;     // column number (0 - 8)
  int sector = ich/8;      // 1 sector consists of 4 boxes (0 - 40)
  int side   = (ich%4)/2;  // WEST or EAST (0=WEST 1=EAST)
  int ud     = ich%2;      // Box position in each sector (0=up 1=down)

  return (column*10 + sector*2 + side*10 + ud);
}

int getPmtID(int ns, const int ibox)
{
  // get PMT id from Box id
  // ns=0  North side PMT in each box.
  // ns=1  South side PMT in each box.

  if(ibox<0 || ibox>=ACC_NBOX){
    std::cout << PHWHERE << " Invalid box number, ibox= " << ibox << std::endl;
    return -1;
  }

  if(ns<0 || ns>1){
    std::cout << PHWHERE << " ns = 0 or 1, ns= " << ns << std::endl;
    std::cout << " forced to be 0 " << std::endl;
    ns = 0;
  }

  int column = ibox/20;     // column number (0 - 8)
  int row    = (ibox%10)/2; // row number (sector id) (0 - 5)
  int side   = (ibox/10)%2; // WEST or EAST (0=WEST 1=EAST)
  int ud     = ibox%2;      // Box position in each sector (0=up 1=down)

  return (column*40 + row*8 + side*2 + ud) + ns*4;
}

int convertBoxID(const int ibox)
{
  int box_index = ibox / 10; // 0 - 7

  int offset = 10;
  int new_index = -1;
  if( box_index%2 == 0 ) new_index = ibox + offset;
  else                   new_index = ibox - offset;

  return new_index;
}

const char* Name()
{
  return "ACC";
}

}

