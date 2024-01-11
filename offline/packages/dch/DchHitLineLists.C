#include <DchHitLineLists.hh>

#include <iostream>

using namespace std;

//____________________________________________________________
DchHitLineLists::DchHitLineLists(size_t totalHits)
{
  all = new PHPointerList<DchHitLine>(totalHits);
  for (short a=0; a<2; a++) 
  {
    x1all[a] = 0;
    x2all[a] = 0;
    uv1all[a] = 0;
    uv2all[a] = 0;
    for (short s=0; s<2; s++) 
    {
      x1[a][s]  = new PHPointerList<DchHitLine>(totalHits/20);
      uv1[a][s] = new PHPointerList<DchHitLine>(totalHits/20);
      x2[a][s]  = new PHPointerList<DchHitLine>(totalHits/20);
      uv2[a][s] = new PHPointerList<DchHitLine>(totalHits/20);
      for(short  p = 0; p <40; p++) {
        for (short c=0; c<80;c++) {
          //initialize the list to an upper value
          list[a][s][p][c] =  new PHPointerList<DchHitLine>((size_t)10); 
        }
      }
    }
  }
}

//____________________________________________________________
DchHitLineLists::~DchHitLineLists()
{
    
  // delete everything that is allocated using "new" in the constructor
  for (short a=0; a<2; a++) 
  {
    
    if(x1all[a]) delete x1all[a];
    if(x2all[a]) delete x2all[a];
    if(uv1all[a]) delete uv1all[a];
    if(uv2all[a]) delete uv2all[a];

    for (short s=0; s<2; s++) 
    {
      if(x1[a][s]) delete x1[a][s];
      if(x2[a][s]) delete x2[a][s];
      if(uv1[a][s]) delete uv1[a][s];
      if(uv2[a][s]) delete uv2[a][s];
      for(short  p = 0; p <40; p++) {
        for (short c=0; c<80;c++) {
          if( list[a][s][p][c] ) delete list[a][s][p][c]; 
        }
      }
    }
  }
  
  all->clearAndDestroy();    
  delete all;
  
}

//_________________________________________________
PHBoolean DchHitLineLists::clearAndDestroy()
{
  all->clearAndDestroy();
  for (short a=0; a<2; a++) {
    delete x1all[a]; x1all[a] = 0;
    delete x2all[a]; x2all[a] = 0;
    delete uv1all[a]; uv1all[a] = 0;
    delete uv2all[a]; uv2all[a] = 0;
    for (short s=0; s<2; s++) 
    {
      x1[a][s]->clear();
      uv1[a][s]->clear();
      x2[a][s]->clear();
      uv2[a][s]->clear();
      for(short p = 0; p <40; p++) {
        for (short c=0; c<80;c++) {
          list[a][s][p][c]->clear();
        }
      }
    }
  }
          
  return True;
}
 
void DchHitLineLists::sortInCellPartialLists()
{
  size_t i;
  short a,s,c,p;
  size_t oldlength,length;
  DchHitLine *hitLine;
  for (a = 0; a<2; a++) {
    for (s=0; s<2; s++) {
      oldlength = x1[a][s]->length();
      delete x1[a][s];
      x1[a][s] = new PHPointerList<DchHitLine>(oldlength);
      oldlength = x2[a][s]->length();
      delete x2[a][s];
      x2[a][s] = new PHPointerList<DchHitLine>(oldlength);
      oldlength = uv1[a][s]->length();
      delete uv1[a][s];
      uv1[a][s] = new PHPointerList<DchHitLine>(oldlength);
      oldlength = uv2[a][s]->length();
      delete uv2[a][s];
      uv2[a][s] = new PHPointerList<DchHitLine>(oldlength);
      for (c=0;c <80; c++) { // put first the first cell for all the planes
	for (p =0; p<12; p++) {  // fill X1
	  length =  list[a][s][p][c]->length();
	  for ( i = 0; i <length; i++) {
	    hitLine = (*list[a][s][p][c])[i];
	    x1[a][s]->append(hitLine);
	  }
	} // end fill X1
	for (p =12; p<20; p++) {  // fill UV1
	  length =  list[a][s][p][c]->length();
	  for ( i = 0; i <length; i++) {
	    hitLine = (*list[a][s][p][c])[i];
	    uv1[a][s]->append(hitLine);
	  }
	} // end fill UV1
	for (p =20; p<32; p++) {  // fill X2
	  length =  list[a][s][p][c]->length();
	  for ( i = 0; i <length; i++) {
	    hitLine = (*list[a][s][p][c])[i];
	    x2[a][s]->append(hitLine);
	  }
	} // end fill X2
	for (p =32 ;p<40; p++) {  // fill UV2
	  length =  list[a][s][p][c]->length();
	  for ( i = 0; i <length; i++) {
	    hitLine = (*list[a][s][p][c])[i];
	    uv2[a][s]->append(hitLine);
	  }
	} // end fill UV2	
      }
    }
  }
  
}
void DchHitLineLists::sortInPlanePartialLists()
{
  size_t i;
  short a,s,p,c;
  DchHitLine *hitLine;  
  size_t oldlength,length;
  for (a = 0; a<2; a++) {
    for (s=0; s<2; s++) {
      oldlength = x1[a][s]->length();
      delete x1[a][s];
      x1[a][s] = new PHPointerList<DchHitLine>(oldlength);
      oldlength = x2[a][s]->length();
      delete x2[a][s];
      x2[a][s] = new PHPointerList<DchHitLine>(oldlength);
      oldlength = uv1[a][s]->length();
      delete uv1[a][s];
      uv1[a][s] = new PHPointerList<DchHitLine>(oldlength);
      oldlength = uv2[a][s]->length();
      delete uv2[a][s];
      uv2[a][s] = new PHPointerList<DchHitLine>(oldlength);
      for (p=0; p<12; p++) {
	for (c=0;c <80;c++) {
	  length =  list[a][s][p][c]->length();
	  for ( i = 0; i <length; i++) {
	    hitLine = (*list[a][s][p][c])[i];
	    x1[a][s]->append(hitLine);
	  }
	}
      }
      for (p=12; p<20; p++) {
	for (c=0;c <80;c++) {
	  length =  list[a][s][p][c]->length();
	  for ( i = 0; i <length; i++) {
	    hitLine = (*list[a][s][p][c])[i];
	    uv1[a][s]->append(hitLine);
	  }
	  
	}
      }
      for (p=20; p<32; p++) {
	for (c=0;c <80;c++) {
	  length =  list[a][s][p][c]->length();
	  for ( i = 0; i <length; i++) {
	    hitLine = (*list[a][s][p][c])[i];
	    x2[a][s]->append(hitLine);
	  }

	}
      }
      for (p=32; p<40; p++) {
	for (c=0;c <80;c++) {
	  length =  list[a][s][p][c]->length();
	  for ( i = 0; i <length; i++) {
	    hitLine = (*list[a][s][p][c])[i];
	    uv2[a][s]->append(hitLine);
	  }
	}
      }
      
    }
  }
}

PHBoolean DchHitLineLists::append(DchHitLine* hit)
{
  short plane  = hit->getPlane();
  short arm    = hit->getArm();
  short side   = hit->getSide();
  short cell   = hit->getCell();
  short wire   = wireType[plane];

  all->append(hit);
  list[arm][side][plane][cell]->append(hit);
  return append(wire, arm, side, hit);
}

PHBoolean DchHitLineLists::merge()
{
  size_t i,length;
  short a,s;
  for (a=0; a<2; a++) {
    delete x1all[a];
    delete x2all[a];
    delete uv1all[a];
    delete uv2all[a];
    x1all[a] =  new PHPointerList<DchHitLine>(x1[a][0]->length() + x1[a][1]->length());                    
    x2all[a] =  new PHPointerList<DchHitLine>(x2[a][0]->length() + x2[a][1]->length());
    uv1all[a] =  new PHPointerList<DchHitLine>(uv1[a][0]->length() + uv1[a][1]->length());
    uv2all[a] =  new PHPointerList<DchHitLine>(uv2[a][0]->length() + uv2[a][1]->length());
  }

  for (a=0; a<2;a++) {
    for (s = 0; s<2; s++) {
      length = x1[a][s]->length();
      for(i=0; i < length; i++) {
	x1all[a]->append((*x1[a][s])[i]);
      }
      length = x2[a][s]->length();
      for(i=0; i < length; i++) {
	x2all[a]->append((*x2[a][s])[i]);
      }
      length = uv1[a][s]->length();
      for(i=0; i < length; i++) {
	uv1all[a]->append((*uv1[a][s])[i]);
      }
      length = uv2[a][s]->length();
      for(i=0; i < length; i++) {
	uv2all[a]->append((*uv2[a][s])[i]);
      }
    }
  }
 
  return True;
}

PHPointerList<DchHitLine>* DchHitLineLists::getList(short arm ,short side, short plane, short cell)
{
  return list[arm][side][plane][cell];
}

PHPointerList<DchHitLine>* DchHitLineLists::getList(short type, short arm ,short side)
{
  if (side < 0) {
    if (x1all && type == X1Wire) return x1all[arm];
    if (x2all && type == X2Wire) return x2all[arm];
    if (uv1all && type == UV1Wire) return uv1all[arm];
    if (uv2all && type == UV2Wire) return uv2all[arm];
    cout << "No List ready now: return zero !!!!  "<< endl;
    return 0;
    
  }else {
    if (type == X1Wire)  return x1[arm][side];
    if (type == X2Wire)  return x2[arm][side];
    if (type == UV1Wire) return uv1[arm][side];
    if (type == UV2Wire) return uv2[arm][side];
  }
  return 0;

}

PHBoolean DchHitLineLists::append(short wire, short arm, short side, DchHitLine* hit)
{
  if (wire == X1Wire) {
    x1[arm][side]->append(hit);
  }else if (wire == X2Wire) {
    x2[arm][side]->append(hit);
  }else if (wire == UV1Wire) {
    uv1[arm][side]->append(hit);      
  }else if (wire == UV2Wire) {
    uv2[arm][side]->append(hit);
  }else {
    return False;
  }

  return True;
}
size_t DchHitLineLists::lengthOfList(short arm, short side,short plane,short cell)
{
    return list[arm][side][plane][cell]->length();
}

size_t DchHitLineLists::lengthOfList(short wire, short arm, short side)
{
  if (wire == X1Wire) {
    if (side < 0) return x1all[arm]->length();
    return  x1[arm][side]->length();
  }else if (wire == X2Wire) {
     if (side < 0) return  x2all[arm]->length();
     return  x2[arm][side]->length();    
  }else if (wire == UV1Wire) {
     if (side < 0) return  uv1all[arm]->length();    
     return uv1[arm][side]->length();
  }else if (wire == UV2Wire) {
    if (side < 0) return  uv2all[arm]->length();
    return uv2[arm][side]->length();
  }
 
  cout << " ERROR : from DchHitLineLists " << endl;
  cout << " NO LIST found which satisfy the request 2 " << endl;
  return 0;
 
}

size_t DchHitLineLists::lengthOfList(short wire, short arm)
{
  if (wire == X1Wire) {
    return x1all[arm]->length();
  }else if (wire == X2Wire) {
    return x2all[arm]->length();    
  }else if (wire == UV1Wire) {
    return uv1all[arm]->length();
  }else if (wire == UV2Wire) {
    return uv2all[arm]->length();
  }

  cout << "ERROR from DchHitLineLists " << endl;
  cout << " NO list found that satisfy the request " << endl;
  return 0 ;

}


