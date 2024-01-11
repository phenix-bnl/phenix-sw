#ifndef Ass_HH
#define Ass_HH

#include <Rtypes.h>
#include <TObject.h>
#include <vector>

#define RESERVESIZE 5

class Ass : public TObject {
public:
  vector<int> _id;
  vector<float> _dist;
  vector<float> _distxyz[3];

public:
  Ass(int reserve_size = RESERVESIZE);
  ~Ass();
  int Reset();
  int GetSize();
  int GetID(int key);
  float GetDist(int key);
  int Insert(int id,float dist,float distx = 0,float disty = 0,float distz = 0);

  ClassDef(Ass,1)
};
//
#endif
//
