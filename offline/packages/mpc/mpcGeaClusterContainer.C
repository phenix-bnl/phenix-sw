#include <mpcGeaClusterContainer.h>
#include <mpcGeaClusterContent.h>

ClassImp(mpcGeaClusterContainer)

mpcGeaClusterContainer::mpcGeaClusterContainer()
{
}

mpcGeaClusterContent *mpcGeaClusterContainer::findPrimary(const int feech)
{
  if ( !GetArray() ) return 0;

  float max_edep = 0.;
  mpcGeaClusterContent *max_gclus = 0;

  for (unsigned int ig=0; ig<size(); ig++)
    {
      mpcGeaClusterContent *geaclus = getCluster(ig);
      if ( geaclus->get_ch() == feech )
	{
	  if ( geaclus->get_edep() > max_edep )
	    {
	      max_edep = geaclus->get_edep();
	      max_gclus = geaclus; 
	    }
	}
    }

  return max_gclus;
}

int mpcGeaClusterContainer::findPrimaryIndex(const int feech) 
{ 
  if ( !GetArray() ) return 0; 
 
  float max_edep = 0.; 
  int max_gindex = -9999; 
 
  for (unsigned int ig=0; ig<size(); ig++) 
    { 
      mpcGeaClusterContent *geaclus = getCluster(ig); 
      if ( geaclus->get_ch() == feech ) 
        { 
          if ( geaclus->get_edep() > max_edep ) 
            { 
              max_edep = geaclus->get_edep(); 
              max_gindex = ig;  
            } 
        } 
    } 
 
  return max_gindex; 
} 

int mpcGeaClusterContainer::findGeaClusIndex(const int it, int opt)
{ 
  //finds clus location where track it deposited the most energy
  //opt == 0, use itorigin
  //opt == 1, use itincoming

  if ( !GetArray() ) return 0;  
  
  float max_edep = 0.;  
  int max_gindex = -9999;
  //  std::cout << "size is: " << size() << std::endl;
  for (unsigned int ig=0; ig<size(); ig++)  
    {  
      mpcGeaClusterContent *geaclus = getCluster(ig);  
      int match_it = geaclus->get_itorigin();
      if(opt == 1) match_it = geaclus->get_itincoming();
      
      //  std::cout << "it, matchit: " << it << ", " << match_it << "\n";
      if ( match_it == it )  
        {  
          if ( geaclus->get_edep() > max_edep )  
            {  
	      //      std::cout << "we have a match\n";
              max_edep = geaclus->get_edep();  
	      max_gindex = ig;
            }  
        }  
    }  

  return max_gindex;  
}  
