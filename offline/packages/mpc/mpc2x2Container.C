#include <mpc2x2Container.h>
#include <mpc2x2Content.h>
#include <iostream>

ClassImp(mpc2x2Container)

using namespace std;

mpc2x2Container::mpc2x2Container()
{
}

int mpc2x2Container::find2x2(const int i2x2id)
{
  int n2x2 = size();
  if ( n2x2==0 ) return -1;

  for (int i2x2=0; i2x2<n2x2; i2x2++)
    {
      mpc2x2Content *m2x2 = get2x2( i2x2 );
      if ( m2x2==0 )
        {
          cout << PHWHERE << " can't find 2x2 " << i2x2 << endl;
          continue;
        }

      int m2x2id = m2x2->get_2x2id();
      if ( m2x2id == i2x2id )
        {
          return i2x2;
        }
    }

  return -1;
}

