{
#include <iostream.h>
  
  // load the shared library
  gSystem->Load("$EVT_LIB/libEvent.so");
  
  // make a event iterator which reads from a testEventiterator
  Eventiterator *xx = 
    new fileEventiterator("bbzdcerledlaser1.evt");
  
  // e is  pointer to an object of type "Event" 
  Event *e;


  int i = 3;
  int j;
  int nw;

  while (i-- && (e = xx->getNextEvent()) )
    {
      e->identify();
     Packet *p = e->getPacket(13001);
      if (p)
        {
          p->dump();
          cout << "Event nr:  " << p->iValue(0,"EVTNR") << endl;
          cout << "Module nr: " << p->iValue(0,"MODULE") << endl;
          cout << "Beam Clock " << p->iValue(0,"BCLK")   << endl;
          cout << "Parity     " << p->iValue(i,"PARITY") << endl;
          cout << "nWord      " << p->iValue(0,"NWORD")  << endl; 
          
          for (j=0; j<8; j++) 
            {
              cout << j << "  " 
                   << p->iValue(j,"T1") << "  "
                   << p->iValue(j,"T2") << "  "
                   << p->iValue(j) << "  "
                   << endl;
            }
          
          delete p;
        }
      delete e;
    }
  // finally, we delete the iterator, thereby closing the file..
  delete xx;
}

