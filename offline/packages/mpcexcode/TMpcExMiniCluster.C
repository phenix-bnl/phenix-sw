#include "TMpcExMiniCluster.h"

using namespace std;


void Square::Print(){
  cout<<"square: "
      <<" x: "<<_x
      <<" y: "<<_y
      <<" ix: "<<_ix
      <<" iy: "<<_iy
      <<" E: "<<GetE()
      <<endl;
}

Square* Square::Clone(){
  Square* sq = new Square();
  sq->SetX(_x);
  sq->SetY(_y);
  sq->SetGridX(_ix);
  sq->SetGridY(_iy);
  for(int i=0;i<8;i++){
    sq->SetLayerE(i,_layer_e[i]);
  }

  return sq;
}

