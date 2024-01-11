#include <mpcGeaTowerContainer.h>
#include <mpcGeaTowerContent.h>

ClassImp(mpcGeaTowerContainer)

mpcGeaTowerContainer::mpcGeaTowerContainer()
{
}

mpcGeaTowerContent *mpcGeaTowerContainer::findPrimary(const int feech)
{
  if ( !GetArray() ) return 0;

  float max_edep = 0.;
  mpcGeaTowerContent *max_gtow = 0;

  for (unsigned int ig=0; ig<size(); ig++)
   {
     mpcGeaTowerContent *geatow = getTower(ig);
     if ( geatow->get_ch() == feech )
       {
         if ( geatow->get_edep() > max_edep )
           {
             max_edep = geatow->get_edep();
             max_gtow = geatow; 
           }
       }
   }

  return max_gtow;
}

