#include "pseudoRunningMean.h"
#include <iostream.h>
#include <iomanip.h>



//int main(int argc, char *argv[])
a()
{
  int i;
  pseudoRunningMean *p = new pseudoRunningMean(5,50);
  int array[5];

  for (i=0; i<5; i++) array[i] = 200;

  for (i=0; i<500; i++) 
    {
      if ( i>60 ) array[0] = 300;
      if ( i>200 ) array[0] = 200;

      p->Add(array);

      cout << setw(5) << i << "  " << setw(14) << p->getMean(0) << endl;
      array[0]++;
    }

  return 0;
}

