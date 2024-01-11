#include <MpcTriggerMapping.h>
#include <MpcMap.h>
#include <map>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h> 
#include <PHIODataNode.h> 	
#include <getClass.h>

#include <cstdlib>

typedef PHIODataNode<PHObject> PHObjectNode_t;
 	
using namespace std;

MpcTriggerMapping *MpcTriggerMapping::__instance = NULL;

MpcTriggerMapping *MpcTriggerMapping::instance(PHCompositeNode *topNode)
{
  if (__instance)
    {
      return __instance;
    }
  
  // instantiate new MpcTriggerMapping on first call
  __instance = new MpcTriggerMapping();
  
  if ( topNode!=0 )
    {
      __instance->AddToNodeTree(topNode);
    }
  
  return __instance;
}

MpcTriggerMapping::MpcTriggerMapping(PHCompositeNode *topNode)
{
  Reset();
  Initialize2x2List();
  InitializeMap();
  
  if ( topNode!=0 )
    {
      this->AddToNodeTree(topNode);
    }
  
  __instance = this;
  
}


MpcTriggerMapping::~MpcTriggerMapping()
{

}


void MpcTriggerMapping::Reset()
{
  for(int ich=0;ich<MAXCH;++ich)
    {
      clear(ich);
      valid2x2[ich] = 0;
    }
  
}


void MpcTriggerMapping::AddToNodeTree(PHCompositeNode *topNode)
{
  PHNodeIterator mapnode_iter(topNode);
  PHCompositeNode *mapNode = dynamic_cast<PHCompositeNode*>(mapnode_iter.findFirst("PHCompositeNode", "MpcTriggerMapping"));
  if ( mapNode!= 0 ) // node already exists
    {
      cout << PHWHERE << " MpcTriggerMapping Node already exists" << endl;
      return;
    }
  
  PHCompositeNode *mpcNode;
  PHNodeIterator iter(topNode);
  mpcNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "MPC"));
  if (!mpcNode) // create mpcNode
    {
      mpcNode = new PHCompositeNode("MPC");
      topNode->addNode(mpcNode);
    }
  
  //PHObjectNode_t *MpcTriggerMappingNode = new PHObjectNode_t(__instance, "MpcTriggerMapping", "PHObject");
  PHObjectNode_t *MpcTriggerMappingNode = new PHObjectNode_t(this, "MpcTriggerMapping", "PHObject");
  mpcNode->addNode(MpcTriggerMappingNode);
}




int MpcTriggerMapping::GetAbove(int id2x2)
{
  
  // 36 42  ... 0  6   we worry about this boundary with this calculation
  // ----------------
  // 5  11 .... 41 47
  // 4  10 .... 40 46
  
  if(id2x2 < 0 || id2x2 > MAXCH -1) return -9999;
    
  if(id2x2%6 != 5)
    {
      return id2x2+1;
    }
  else if(id2x2%72 < 36)
    { 
      return id2x2+36 - 5;
    }
  else
    {
      return id2x2 -(36 + 5);
    }
  return -9999;
}


int MpcTriggerMapping::GetRight(int id2x2)
{
 
  // 1  7  ... 31 | 37 43 ... 67 | 1
  // 4  10 ... 30 | 36 42 ... 66 | 0
  
  if(id2x2 < 0 || id2x2 > MAXCH -1) return -9999;
    
  if(id2x2%72 < 66)
    {
      return id2x2+6;
    }
  else
    { 
      return id2x2 - 72 + 6;
    }

  return -9999;
}




int MpcTriggerMapping::GetDiagonal(int id2x2)
{
  
  //only worry about the upper right corners

  if(id2x2 < 0 || id2x2 > MAXCH -1) return -9999;
  
  
  if(id2x2%36 != 35) //the upper right corner
    {
      return GetRight(GetAbove(id2x2));
    }

  return -9999;
}

void MpcTriggerMapping::Initialize2x2List()
{
  MpcMap* mpcmap = MpcMap::instance();
  
  for(int ich=0;ich<576;++ich)
    {
      
      int gridx = mpcmap->getGridX( ich );
      //      int arm = (ich>=288)?1:0;
         
      if ( gridx < 0 )
        {
	  
	  //          cout << "ERROR, Non-existent channel " << ich << endl;
          continue;
        }
      
      int mondo = 6*mpcmap->getAsic(ich)+mpcmap->getMondo(ich);
      if(mondo < 0 || mondo >= MAXCH) 
	{
	  cout << PHWHERE << "Problem with initializing list of 2x2s....exiting\n";
	  // we do want to exit - rather than writing out of bounds in the next line
	  exit(1);
	}
      valid2x2[mondo] = 1;
    }
  
  return;
  
}

void MpcTriggerMapping::InitializeMap()
{
  
  //map 4 2x2's to a 4x4
  // exclude any 2x2 that does not actually exist
  //exclude the diagonal 2x2 for the corners
   

  vector<int> vid;
  for(int iarm=0;iarm<2;++iarm)
    for(int ix=0;ix<12;++ix)
      for(int iy=0;iy<6;++iy)
	{
	  vid.clear();
	  int id = iy+6*ix+iarm*72;
	  vid.push_back(id);
	  vid.push_back(GetRight(id));
	  vid.push_back(GetAbove(id));
	  vid.push_back(GetDiagonal(id));
	  

	  for(vector<int>::iterator itr = vid.begin(); itr!= vid.end(); ++itr)
	    {
	      if( *itr < 0 || *itr > MAXCH-1) continue;
	      if( valid2x2[*itr] == 0) continue;
	      
	      set_next_2x2id(id,*itr);
	    }
	  
	  
	}

}

void MpcTriggerMapping::Print(Option_t*) const {
  for(int i=0;i<MAXCH;++i)
    {
      if( get_size(i) > 0)
	{
	  cout << "4x4: " << i << "\t";
	  for(int j=0;j<(int) get_size(i);++j)
	    {
	      cout << get_2x2id(i,j) << "\t";
	    }
	  cout << endl;
	}
    }

  for(int i=0;i<MAXCH;++i)
    {
      cout << "valid2x2: " << i << ": " << valid2x2[i] << endl;
    }

  
}



/*


void MpcTriggerMapping::InitializeMap()
{
  


  //mapping goes

  
  MpcMap* mpcmap = MpcMap::instance();
  
  //create vector of MondoMappings
  //drawplot with different values based on the mondo number
  
    
    
  for(int iarm=0;iarm<2;++iarm)
    for(int ix=0;ix<12;++ix)
      for(int iy=0;iy<12;++iy)
	{
	  inner_fem[iarm][ix][iy] = -2;
	  outer_fem[iarm][ix][iy] = -2;
	}
  
  for(int iarm=0;iarm<2;++iarm)
    for(int ix=0;ix<6;++ix)
      for(int iy=0;iy<6;++iy)
	{
	  inner_2x2[iarm][ix][iy] = -2;
	  outer_2x2[iarm][ix][iy] = -2;
	}
  

  for(int ich=0;ich<576;++ich)
    {
      
      int gridx = mpcmap->getGridX( ich );
      int gridy = mpcmap->getGridY( ich );
      int arm = (ich>=288)?1:0;
         
      if ( gridx == -1 )
        {
          cout << "ERROR, Non-existent channel " << ich << endl;
          continue;
        }
      
      int gridx1 = -2;
      int gridy1 = -2;

      int gridx2 = -2;
      int gridy2 = -2;
      
      int infem1 = map_tower_inner(gridx,gridy,gridx1,gridy1);
      int infem2 = map_tower_outer(gridx,gridy,gridx2,gridy2);
      
      
      
      

      int mondo = 6*mpcmap->getAsic(ich)+mpcmap->getMondo(ich);
      
      
      if(infem1 == 1)
	{
	  inner_fem[arm][gridx1][gridy1] = mondo;
	}
      
      if(infem2 == 1)
	{
	  outer_fem[arm][gridx2][gridy2] = mondo;
	}
      

      
     
      
      
    }
  

  
  vector<int> vchannels; // collection of channels within a given geometrical 2x2
  
  //map 12x12 collection of channels to a 6x6 collection of 2x2's
  for(int iarm=0;iarm<2;++iarm)
    for(int ix=0;ix<6;++ix)
      for(int iy=0;iy<6;++iy)
	{
	  vchannels.clear();
	  //  1 1 2 2
	  //  1 1 2 2     //iy = 1, chy = 2
	  //  3 3 4 4
	  //  3 3 4 4     //iy = 0, chy = 0
	  
	  int chx = ix*2;
	  int chy = iy*2;
	  
	  //inner fem
	  //push the geometrical 2x2 channels into a vector
	  for(int ix2=chx;ix2<2+chx;++ix2)
	    for(int iy2=0;iy2<2+chy;++iy2)
	      {
		vchannels.push_back(inner_fem[iarm][ix2][iy2]);
	      }
	  inner_2x2[iarm][ix][iy] = GetMode(vchannels);
	  vchannels.clear();
	  
	  //outer fem
	  //push the geometrical 2x2 channels into a vector
	  for(int ix2=chx;ix2<2+chx;++ix2)
	    for(int iy2=0;iy2<2+chy;++iy2)
	      {
		vchannels.push_back(outer_fem[iarm][ix2][iy2]); 
	      }
	  outer_2x2[iarm][ix][iy] = GetMode(vchannels);
	  
	}
  
  //now we have the inner and outer sequence of 2x2's
  //we just need to make the overlapping 4x4 sums
  //start with lower left corner of inner fem and connect it above and to the right with the outer fem
  //also have to connect the outer fem to the right with the inner fem
  //ignore any overlapping corner sums
  


}







//transforms ix,iy positions for the outer part of the mpc fem into a 12x12 grid used for the overlapping 4x4 tower sums

int MpcTriggerMapping::map_tower_outer(int ix, int iy, int& rx, int&ry)
{
  if(ix < 0 || iy < 0) return 0; 
  
  if(ix <= 2)
    {
      ry = iy - 3;
      rx = ix+18+6 - 15;
    }
  else if(iy <= 2)
    {
      ry = iy+12 - 3;
      rx = ix+12 - 15;
    }
  else if(iy >= 15)
    {
      rx = ix+12 - 15;
      ry = iy - 12 - 3;
    }
  else if(ix >= 15)
    { 
      rx = ix - 15;
      ry = iy - 3;
    }
  else{
    return 0;
  }

  return 1; 
  

    
}


//transforms ix,iy positions for the inner part of the mpc fem into a 12x12 grid used for the overlapping 4x4 tower sums

int MpcTriggerMapping::map_tower_inner(int ix, int iy, int& rx, int&ry)
{
  

  if(ix >= 3  && ix <= 14 && iy >= 3 && iy <= 14)
    {
      ry = iy - 3;
      rx = ix - 3;
      
    }
  else
    {
      return 0;
    }

  return 1;
     
} 

int MpcTriggerMapping::GetMode(const vector<int>& v)
{
  
  std::map<int,int> vmap;
  for( vector<int>::iterator itr=v.begin();itr!= v.end(); ++itr)
    {
      if(vmap.find(*itr) == vmap.end())
	{
	  map[*itr] = 1;
	}
      else
	{ 
	  ++vmap[*itr]; 
	}
    }
  //now go through the map and find out which element has the most entries
  
  int max_element = -9999;  
  int max_count = 0;   
 
  for( map<int,int>::iterator itr = vmap.begin(); itr!= vmap.end; ++itr)    
    {
      if(itr->first < 0) continue;  
      if(itr->second > max_count) 
	{
	  max_element = itr->first;
	  max_count = itr->second; 
	} 
      
    } 
  
  return max_element;  
   
}


*/

  
  

