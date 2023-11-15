#ifndef __PdbErtDecode_HH__
#define __PdbErtDecode_HH__

#include "PdbCalChan.hh"

class PdbErtDecode : public PdbCalChan 
{
public:
  PdbErtDecode();
  //  PdbErtDecode( const PdbErtDecode &c);
  PdbErtDecode& operator = (const PdbErtDecode &c);

  virtual ~PdbErtDecode(){};

  //  word and roc position in ERT packets
  //
  virtual int  set_word4x4ACoord_ROC_WORD( const int sector, const int side, 
				           const int v_roc, const int v_word);
  virtual int  set_word4x4BCoord_ROC_WORD( const int sector, const int side, 
				           const int v_roc, const int v_word);
  virtual int  set_word4x4CCoord_ROC_WORD( const int sector, const int side, 
				           const int v_roc, const int v_word);
  virtual int  set_word2x2Coord_ROC_WORD( const int sector, const int side, 
				          const int v_roc, const int v_word);
  virtual int  set_word4x5Coord_ROC_WORD( const int sector, const int side, 
				          const int v_roc, const int v_word);

  virtual int  get_word4x4ACoord_ROC( const int sector, const int side) const; 
  virtual int  get_word4x4ACoord_WORD(const int sector, const int side) const; 
  virtual int  get_word4x4BCoord_ROC( const int sector, const int side) const; 
  virtual int  get_word4x4BCoord_WORD(const int sector, const int side) const; 
  virtual int  get_word4x4CCoord_ROC( const int sector, const int side) const; 
  virtual int  get_word4x4CCoord_WORD(const int sector, const int side) const; 
  virtual int  get_word2x2Coord_ROC( const int sector, const int side) const; 
  virtual int  get_word2x2Coord_WORD(const int sector, const int side) const; 
  virtual int  get_word4x5Coord_ROC( const int sector, const int side) const; 
  virtual int  get_word4x5Coord_WORD(const int sector, const int side) const; 

  // correlation between bit position and SM numbering convention 
  //
  virtual int set_SMCoordModeAPBGL(const int bit, const int value);
  virtual int set_SMCoordModeBPBGL(const int bit, const int value);
  virtual int set_SMCoordModeAPBSC(const int bit, const int value);
  virtual int set_SMCoordModeBPBSC(const int bit, const int value);
  virtual int set_SMCoordModeARICH(const int bit, const int value);
  virtual int set_SMCoordModeBRICH(const int bit, const int value);

  virtual int get_SMCoordModeAPBGL(const int bit) const;
  virtual int get_SMCoordModeBPBGL(const int bit) const;
  virtual int get_SMCoordModeAPBSC(const int bit) const;
  virtual int get_SMCoordModeBPBSC(const int bit) const;
  virtual int get_SMCoordModeARICH(const int bit) const;
  virtual int get_SMCoordModeBRICH(const int bit) const;

  virtual void print() const;

private:
  int word4x4ACoord[4][2][2];      // for 4x4A, 0: ROC(0-19), 
                                   //           1: word(0-5)
  int word4x4BCoord[4][2][2];      // 4x4B
  int word4x4CCoord[4][2][2];      // 4x4C
  int word2x2Coord[4][2][2];       // 2x2
  int word4x5CoordRICH[4][2][2];   // 2x2

  //.. bit position of a SM in a word
  //
  int SMCoordModeAPBGL[16];  
  int SMCoordModeBPBGL[16];
  int SMCoordModeAPBSC[16];
  int SMCoordModeBPBSC[16];
  int SMCoordModeARICH[16];
  int SMCoordModeBRICH[16];

  ClassDef(PdbErtDecode,1);
};

#endif /* __PdbErtDecode_HH__ */
