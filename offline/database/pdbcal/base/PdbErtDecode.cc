#include <iostream>
#include <string.h>

#include <PdbErtDecode.hh>

//_______________________________
PdbErtDecode::PdbErtDecode()
{

  memset(word4x4ACoord, -1, 2*2*4*sizeof(int));
  memset(word4x4BCoord, -1, 2*2*4*sizeof(int));
  memset(word4x4CCoord, -1, 2*2*4*sizeof(int));
  memset(word2x2Coord,  -1, 2*2*4*sizeof(int));
  memset(word4x5CoordRICH,  -1, 2*2*4*sizeof(int));

  memset(SMCoordModeAPBGL, -1, 16*sizeof(int));
  memset(SMCoordModeBPBGL, -1, 16*sizeof(int));
  memset(SMCoordModeAPBSC, -1, 16*sizeof(int));
  memset(SMCoordModeBPBSC, -1, 16*sizeof(int));
  memset(SMCoordModeARICH, -1, 16*sizeof(int));
  memset(SMCoordModeBRICH, -1, 16*sizeof(int));
}


//_______________________________
/*
PdbErtDecode::PdbErtDecode( const PdbErtDecode &c)
{
  for (int sector=0; sector <4; sector++) {
    for (int side=0; side<2; side++) {
       set_word4x4ACoord_ROC_WORD(sector, side, 
		                  c.get_word4x4ACoord_ROC(sector, side), 
				  c.get_word4x4ACoord_WORD(sector, side));
       set_word4x4BCoord_ROC_WORD(sector, side, 
		                  c.get_word4x4BCoord_ROC(sector, side), 
				  c.get_word4x4BCoord_WORD(sector, side));
       set_word4x4CCoord_ROC_WORD(sector, side, 
		                  c.get_word4x4CCoord_ROC(sector, side),
				  c.get_word4x4CCoord_WORD(sector, side));
       set_word2x2Coord_ROC_WORD(sector, side, 
		                  c.get_word2x2Coord_ROC(sector, side),
				  c.get_word2x2Coord_WORD(sector, side));
       set_word4x5Coord_ROC_WORD(sector, side, 
		                  c.get_word4x5Coord_ROC(sector, side), 
				  c.get_word4x5Coord_WORD(sector, side));
    }
  }

  for(int bit = 0; bit<16; bit++) {
    set_SMCoordModeAPBGL(bit, c.get_SMCoordModeAPBGL(bit));
    set_SMCoordModeBPBGL(bit, c.get_SMCoordModeBPBGL(bit));
    set_SMCoordModeAPBSC(bit, c.get_SMCoordModeAPBSC(bit));
    set_SMCoordModeBPBSC(bit, c.get_SMCoordModeBPBSC(bit));
    set_SMCoordModeARICH(bit, c.get_SMCoordModeARICH(bit));
    set_SMCoordModeBRICH(bit, c.get_SMCoordModeBRICH(bit));
  }
}
*/
//_______________________________
PdbErtDecode& PdbErtDecode::operator = (const PdbErtDecode &c)
{
  for (int sector=0; sector <4; sector++) {
    for (int side=0; side<2; side++) {
       set_word4x4ACoord_ROC_WORD(sector, side, 
		                  c.get_word4x4ACoord_ROC(sector, side), 
				  c.get_word4x4ACoord_WORD(sector, side));
       set_word4x4BCoord_ROC_WORD(sector, side, 
		                  c.get_word4x4BCoord_ROC(sector, side), 
				  c.get_word4x4BCoord_WORD(sector, side));
       set_word4x4CCoord_ROC_WORD(sector, side, 
		                  c.get_word4x4CCoord_ROC(sector, side),
				  c.get_word4x4CCoord_WORD(sector, side));
       set_word2x2Coord_ROC_WORD(sector, side, 
		                  c.get_word2x2Coord_ROC(sector, side),
				  c.get_word2x2Coord_WORD(sector, side));
       set_word4x5Coord_ROC_WORD(sector, side, 
		                  c.get_word4x5Coord_ROC(sector, side), 
				  c.get_word4x5Coord_WORD(sector, side));
    }
  }

  for(int bit = 0; bit<16; bit++) {
    set_SMCoordModeAPBGL(bit, c.get_SMCoordModeAPBGL(bit));
    set_SMCoordModeBPBGL(bit, c.get_SMCoordModeBPBGL(bit));
    set_SMCoordModeAPBSC(bit, c.get_SMCoordModeAPBSC(bit));
    set_SMCoordModeBPBSC(bit, c.get_SMCoordModeBPBSC(bit));
    set_SMCoordModeARICH(bit, c.get_SMCoordModeARICH(bit));
    set_SMCoordModeBRICH(bit, c.get_SMCoordModeBRICH(bit));
  }
  return *this;
}

//____________________________________
int PdbErtDecode::set_word4x4ACoord_ROC_WORD(const int sector, const int side,
				  const int v_roc, const int v_word)
{
  if ( side < 0    || side>=2 || sector < 0 || sector>=4 ) 
	  	return -1;

  word4x4ACoord[sector][side][0] = v_roc;
  word4x4ACoord[sector][side][1] = v_word;

  return 0;
}
//_______________________________
int PdbErtDecode::set_word4x4BCoord_ROC_WORD(const int sector, const int side,
				  const int v_roc, const int v_word)
{
  if ( side < 0    || side>=2 || sector < 0 || sector>=4 ) 
	  	return -1;

  word4x4BCoord[sector][side][0] = v_roc;
  word4x4BCoord[sector][side][1] = v_word;

  return 0;
}
//_______________________________
int PdbErtDecode::set_word4x4CCoord_ROC_WORD(const int sector, const int side,
				  const int v_roc, const int v_word)
{
  if ( side < 0    || side>=2 || sector < 0 || sector>=4 ) 
	  	return -1;

  word4x4CCoord[sector][side][0] = v_roc;
  word4x4CCoord[sector][side][1] = v_word;

  return 0;
}
//_______________________________
int PdbErtDecode::set_word2x2Coord_ROC_WORD(const int sector, const int side,
				  const int v_roc, const int v_word)
{
  if ( side < 0    || side>=2 || sector < 0 || sector>=4 ) 
	  	return -1;

  word2x2Coord[sector][side][0] = v_roc;
  word2x2Coord[sector][side][1] = v_word;

  return 0;
}
//_______________________________
int PdbErtDecode::set_word4x5Coord_ROC_WORD(const int sector, const int side,
				  const int v_roc, const int v_word)
{
  if ( side < 0    || side>=2 || sector < 0 || sector>=4 ) 
	  	return -1;

  word4x5CoordRICH[sector][side][0] = v_roc;
  word4x5CoordRICH[sector][side][1] = v_word;

  return 0;
}

//
//____________________________________
int PdbErtDecode::get_word4x4ACoord_ROC(const int sector, const int side) const
{
  if ( side < 0    || side>=2 || sector < 0 || sector>=4 ) 
	  	return -1;

   return word4x4ACoord[sector][side][0];
}
int PdbErtDecode::get_word4x4ACoord_WORD(const int sector, const int side) const
{
  if ( side < 0    || side>=2 || sector < 0 || sector>=4 )
		return -1;

  return word4x4ACoord[sector][side][1];
}
//_______________________________
int PdbErtDecode::get_word4x4BCoord_ROC(const int sector, const int side) const
{
  if ( side < 0    || side>=2 || sector < 0 || sector>=4 ) 
	  	return -1;

  return word4x4BCoord[sector][side][0];
}
int PdbErtDecode::get_word4x4BCoord_WORD(const int sector, const int side)  const
{
  if ( side < 0    || side>=2 || sector < 0 || sector>=4 )
		                  return -1;

    return word4x4BCoord[sector][side][1];
}

//_______________________________
int PdbErtDecode::get_word4x4CCoord_ROC(const int sector, const int side) const
{
  if ( side < 0    || side>=2 || sector < 0 || sector>=4 ) 
	  	return -1;

  return word4x4CCoord[sector][side][0];
}
int PdbErtDecode::get_word4x4CCoord_WORD(const int sector, const int side) const
{
  if ( side < 0    || side>=2 || sector < 0 || sector>=4 )
                 return -1;

    return word4x4CCoord[sector][side][1];
}

//_______________________________
int PdbErtDecode::get_word2x2Coord_ROC(const int sector, const int side) const
{
  if ( side < 0    || side>=2 || sector < 0 || sector>=4 ) 
	  	return -1;

  return word2x2Coord[sector][side][0];
}
int PdbErtDecode::get_word2x2Coord_WORD(const int sector, const int side) const
{
  if ( side < 0    || side>=2 || sector < 0 || sector>=4 )
                return -1;

  return word2x2Coord[sector][side][1];
}
//__________________________________
int PdbErtDecode::get_word4x5Coord_ROC(const int sector, const int side) const
{
  if ( side < 0    || side>=2 || sector < 0 || sector>=4 ) 
	  	return -1;

  return word4x5CoordRICH[sector][side][0];
}
int PdbErtDecode::get_word4x5Coord_WORD(const int sector, const int side) const
{
  if ( side < 0    || side>=2 || sector < 0 || sector>=4 )
                return -1;

  return word4x5CoordRICH[sector][side][1];
}

//____________________________________
int PdbErtDecode::set_SMCoordModeAPBGL(const int bit, const int value)
{
  if(bit>=16) return -1;
  SMCoordModeAPBGL[bit] = value;
  return 0;
}
//____________________________________
int PdbErtDecode::set_SMCoordModeBPBGL(const int bit, const int value)
{
  if(bit>=16) return -1;
  SMCoordModeBPBGL[bit] = value;
  return 0;
}
//____________________________________
int PdbErtDecode::set_SMCoordModeAPBSC(const int bit, const int value)
{
  if(bit>=16) return -1;
  SMCoordModeAPBSC[bit] = value;
  return 0;
}
//____________________________________
int PdbErtDecode::set_SMCoordModeBPBSC(const int bit, const int value)
{
  if(bit>=16) return -1;
  SMCoordModeBPBSC[bit] = value;
  return 0;
}
//____________________________________
int PdbErtDecode::set_SMCoordModeARICH(const int bit, const int value)
{
  if(bit>=16) return -1;
  SMCoordModeARICH[bit] = value;
  return 0;
}
//____________________________________
int PdbErtDecode::set_SMCoordModeBRICH(const int bit, const int value)
{
  if(bit>=16) return -1;
  SMCoordModeBRICH[bit] = value;
  return 0;
}

//____________________________________
int PdbErtDecode::get_SMCoordModeAPBGL(const int bit) const
{
  if(bit>=16) return -1;
  return SMCoordModeAPBGL[bit];
}
//____________________________________
int PdbErtDecode::get_SMCoordModeBPBGL(const int bit) const
{
  if(bit>=16) return -1;
  return SMCoordModeBPBGL[bit];
}
//____________________________________
int PdbErtDecode::get_SMCoordModeAPBSC(const int bit) const
{
  if(bit>=16) return -1;
  return SMCoordModeAPBSC[bit];
}
//____________________________________
int PdbErtDecode::get_SMCoordModeBPBSC(const int bit) const
{
  if(bit>=16) return -1;
  return SMCoordModeBPBSC[bit];
}
//____________________________________
int PdbErtDecode::get_SMCoordModeARICH(const int bit) const
{
  if(bit>=16) return -1;
  return SMCoordModeARICH[bit];
}
//____________________________________
int PdbErtDecode::get_SMCoordModeBRICH(const int bit) const
{
  if(bit>=16) return -1;
  return SMCoordModeBRICH[bit];
}

//____________________________________
void PdbErtDecode::print() const
{
  std::cout << "PdbErtDecode" << std::endl;
  std::cout << "Arm\tSector\tSector T0\tError\tStatus" << std::endl;

  for(int sector=0; sector<4; sector++) {
    for(int side = 0; side<2; side++) {
       std::cout<<word4x4ACoord[sector][side][0]<<" "<<word4x4ACoord[sector][side][1]<<" "
	   <<word4x4BCoord[sector][side][0]<<" "<<word4x4BCoord[sector][side][1]<<" "
	   <<word4x4CCoord[sector][side][0]<<" "<<word4x4CCoord[sector][side][1]<<" "
	   <<word2x2Coord[sector][side][0]<<" "<<word2x2Coord[sector][side][1]<<" "
	   <<word4x5CoordRICH[sector][side][0]<<" "<<word4x5CoordRICH[sector][side][1]<<std::endl;
    }
  }

  for(int bit = 0; bit<16; bit++) 
    std::cout<<SMCoordModeAPBGL[bit]<<" ";

  std::cout<<std::endl;
  for(int bit = 0; bit<16; bit++) 
    std::cout<<SMCoordModeBPBGL[bit]<<" ";

  std::cout<<std::endl;
  for(int bit = 0; bit<16; bit++) 
    std::cout<<SMCoordModeAPBSC[bit]<<" ";

  std::cout<<std::endl;
  for(int bit = 0; bit<16; bit++) 
    std::cout<<SMCoordModeBPBSC[bit]<<" ";

  std::cout<<std::endl;
  for(int bit = 0; bit<16; bit++) 
    std::cout<<SMCoordModeARICH[bit]<<" ";

  std::cout<<std::endl;
  for(int bit = 0; bit<16; bit++) 
    std::cout<<SMCoordModeBRICH[bit]<<" ";

  std::cout<<std::endl;
}
