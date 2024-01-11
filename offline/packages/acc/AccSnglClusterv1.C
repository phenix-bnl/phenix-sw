#include "AccSnglClusterv1.h"

ClassImp(AccSnglClusterv1)

AccSnglClusterv1::AccSnglClusterv1()
{

  //  In the constructor init to -9999
  for (unsigned int i=0; i<ACCBOX; i++)
    {
      aerph1[i] = -9999;       
      aerph2[i] = -9999;
      aert1 [i] = -9999;      
      aert2 [i] = -9999;
    }      
  aerhitid     = -9999 ;
  aerhitconfig = -9999 ;

  return;
}

AccSnglClusterv1::AccSnglClusterv1(AccSnglClusterv1 *track)
{

  //  Copies all values from another cluster into this one.
  if (!track) return;

  for (unsigned int i=0; i<ACCBOX; i++)
    {
      aerph1[i] = track->get_aerph1(i);       
      aerph2[i] = track->get_aerph2(i);
      aert1 [i] = track->get_aert1 (i);      
      aert2 [i] = track->get_aert2 (i);
    }      

  aerhitid     = track->get_aerhitid     ();
  aerhitconfig = track->get_aerhitconfig ();
  
  return;
}










