////////////////////////
//microL2DArrayWrapperv1:
//    
//    
//    
//    structure to hold UINT array, so it can inherit 
//    from TObject and therefore be put in a TClonesArray
//    
//    
//    may replace with a TArrayI
//
//
//////////////////////////

#include <microL2DArrayWrapperv1.h>

ClassImp(microL2DArrayWrapperv1)

microL2DArrayWrapperv1::microL2DArrayWrapperv1()
{
  short int i;
  for (i=0;i<MICROL2MAXNUMALGORITHMSV1+1;i++)
    {
      theArray[i]= 0;
    }
}

microL2DArrayWrapperv1::microL2DArrayWrapperv1(microL2DArrayWrapperv1 *wrapper)
{
  short int i;
  for (i=0;i<MICROL2MAXNUMALGORITHMSV1+1;i++)
    {
      theArray[i]= wrapper->get_theArrayVal(i);
    }
}


