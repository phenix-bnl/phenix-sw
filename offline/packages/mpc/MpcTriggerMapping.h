#ifndef __MPCTRIGGERMAPPING_H__
#define __MPCTRIGGERMAPPING_H__

#include <vector>
#include <string>
#include <PHObject.h>

class PHCompositeNode;


struct Mpc4x4Node
{
  Mpc4x4Node(): vch()
  {}
  std::vector<int> vch;
};


class MpcTriggerMapping : public PHObject
{
 public:
  static MpcTriggerMapping *instance(PHCompositeNode *topNode = 0);
  MpcTriggerMapping(PHCompositeNode *topNode = 0);
  virtual ~MpcTriggerMapping();
  void Print(Option_t* =0) const;
  void Reset();
  void Initialize2x2List();
  void InitializeMap();
  void AddToNodeTree(PHCompositeNode *topNode);
  unsigned int get_size(int id) const
  {
    if(id < 0 || id > MAXCH-1) return -9999;
    return chmap[id].vch.size();
  }
  
  
  int get_2x2id(int id, int index) const
  {
    if( index > (int) get_size(id)-1 ) return -9999;
    return chmap[id].vch.at(index);
  }


  

  
 private:
  
 
  
  int GetAbove(int id2x2);
  int GetRight(int id2x2);
  int GetDiagonal(int id2x2);

  void clear(int id)
  {
    if(id < 0 || id > MAXCH-1) return;
    chmap[id].vch.clear();
    return;
  }

  int set_next_2x2id(int id, int id2x2)
  {
    if(id < 0 || id > MAXCH-1) return 0;
    chmap[id].vch.push_back(id2x2);
    return 1;
  }
  
  static MpcTriggerMapping *__instance;
  
  
  static const int MAXCH = 144;
  Mpc4x4Node chmap[MAXCH];
  int valid2x2[MAXCH];
  

  //functions used to construct  this from MpcMap
  //for now we assume the sequential 2x2 scheme for mapping to a 4x4 is correct

  /*
 
  int map_tower_inner(int ix, int iy, int& rx, int&ry);
  int map_tower_outer(int ix, int iy, int& rx, int&ry);
  int GetMode(const vector<int>& v);


  int inner_fem[2][12][12];
  int outer_fem[2][12][12];
  
  int inner_2x2[2][6][6];
  int outer_2x2[2][6][6];
  */

  ClassDef(MpcTriggerMapping,1);
};




#endif

