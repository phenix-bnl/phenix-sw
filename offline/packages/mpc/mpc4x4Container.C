#include <mpc4x4Container.h>
#include <mpc4x4Content.h>
#include <iostream>

ClassImp(mpc4x4Container)

using namespace std;

mpc4x4Container::mpc4x4Container()
{
}

int mpc4x4Container::find4x4(const int i4x4id)
{
  int n4x4 = size();
  if ( n4x4==0 ) return -1;

  for (int i4x4=0; i4x4<n4x4; i4x4++)
    {
      mpc4x4Content *m4x4 = get4x4( i4x4 );
      if ( m4x4==0 )
        {
          cout << PHWHERE << " can't find 4x4 " << i4x4 << endl;
          continue;
        }

      int m4x4id = m4x4->get_4x4id();
      if ( m4x4id == i4x4id )
        {
          return i4x4;
        }
    }

  return -1;
}

