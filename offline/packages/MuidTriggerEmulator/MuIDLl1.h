
#ifndef __MuIDLl1_h__
#define __MuIDLl1_h__

#include <string>
#include <vector>
#include <TMuiChannelId.hh>
#include <hash_vector.hh>

class Event;
class PHCompositeNode; 

class MuIDLl1{

  protected:

  struct revLUTentry
  {
    int frame;
    int bit;
  };


  enum { MAX_ARM=2};
  enum { MAX_ORIENTATION=2 };
  enum { MAX_ROC=20 };
  enum { WORD_PER_ROC=6 };
  enum { BIT_PER_WORD=16 };
  enum { MAX_FEM=MAX_ARM*MAX_ORIENTATION }; // 4
  enum { WORD_PER_FEM=WORD_PER_ROC*MAX_ROC }; // 120
  enum { HEADER_WORDS=5 };
  enum { USR_WORDS=8 };
  enum { MAX_WORD=WORD_PER_FEM*MAX_FEM }; // 240
  enum { CHANNEL_PER_FEM=BIT_PER_WORD*WORD_PER_FEM }; // 1920
  enum { MAX_CHANNEL=CHANNEL_PER_FEM*MAX_FEM }; // 3840
  enum { PANEL_PER_GAP=6};
  enum { GAP_PER_ARM=5};
  
  enum { MAX_HORIZ_SYMSETS=118};
  enum { MAX_VERT_SYMSETS=158};
  enum { MAX_SYMSETS=MAX_VERT_SYMSETS };
  enum { SOUTH=0, NORTH=1};
  enum { HORIZONTAL=0, VERTICAL=1};
  enum { COSMICS_SUM=7 };
  enum { FPGA_ROC_BITS_CUT=25 };
  
  int success;
  char filename[50];
  
  long SymsetVector[MAX_ARM][MAX_ORIENTATION][5];
  long ShallowSymsetVector[MAX_ARM][MAX_ORIENTATION][5];

  long PanelSymsetVector[MAX_ARM][MAX_ORIENTATION][PANEL_PER_GAP][5];
  long PanelShallowSymsetVector[MAX_ARM][MAX_ORIENTATION][PANEL_PER_GAP][5];

  long Cosmics[MAX_ARM][4];

  short ROCData[MAX_ARM][MAX_ORIENTATION][MAX_ROC][WORD_PER_ROC];
  unsigned char fiberBitMask[MAX_ARM][MAX_ORIENTATION][MAX_ROC][WORD_PER_ROC*BIT_PER_WORD];
  short fiberBitData[MAX_ARM][MAX_ORIENTATION][MAX_ROC][WORD_PER_ROC*BIT_PER_WORD];
  bool LT[MAX_ARM][MAX_ORIENTATION][GAP_PER_ARM][MAX_SYMSETS];
  int flagBit[MAX_ARM][MAX_ORIENTATION][MAX_ROC][2];

  revLUTentry RocToFiber[WORD_PER_ROC*BIT_PER_WORD];
  
  long numROCBitsSet[MAX_ARM][MAX_ORIENTATION][MAX_ROC];

  long numUsedROCBitsSet[MAX_ARM][MAX_ORIENTATION][5];
  long numUsedROCBitsSetPanel[MAX_ARM][MAX_ORIENTATION][PANEL_PER_GAP];

  short diagBitvector[MAX_ARM][MAX_ORIENTATION][5];
  
  bool ROCNoiseCut; 

  bool NewMuidPanelAlg; 
  bool TwoDSymsetAlg; 
  bool RequireFourGaps;
  bool OneHadronTrigger; 

  bool mutooInitialized;

  std::string tmuihitmap_name;
  
  public:

  //! constructor
  MuIDLl1(const char* = " " );
  
  //! destructor
  virtual ~MuIDLl1()
  {}

  //! get data from event structure
  int getDataFromMuidPacket(Event*); 
  
  //! get data from Muioo DST nodes
  bool getDataFromMutoo(PHCompositeNode* ); 
  
  //! initialize muid geometry
  void initializeMutoo(PHCompositeNode* );
  
  int getFiberBitMasks(const char *filename);

  void setROCNoiseCut(bool in){ROCNoiseCut = in;}; 
  void setNewPanelAlg(bool in){NewMuidPanelAlg = in;}; 

  void setRequireFourGaps(bool in){RequireFourGaps = in;}
  void setOneHadronTrigger(bool in){OneHadronTrigger = in;}
  
  void set_tmuihitmap_name(std::string name) {tmuihitmap_name = name;}

  int calculate(short SimSyncErr[2][2][5][20]);
  void mapMuidSouthHorizLI(int chip, short SimSyncErr[2][2][5][20], int Cflag);
  void mapMuidSouthVertLI(int chip, short SimSyncErr[2][2][5][20], int Cflag);
  void mapMuidNorthHorizLI(int chip, short SimSyncErr[2][2][5][20], int Cflag);
  void mapMuidNorthVertLI(int chip, short SimSyncErr[2][2][5][20], int Cflag);

  void mapMuidPanelLI(int arm, int orient, int panel,short SimSyncErr[2][2][5][20], int Cflag); 

  void processSymsets(int arm, int orient, int chip);
  void processSymsetsByPanel(int arm, int orient, int panel);
  void processSymsetsByPanel2D(int arm, int panel);
  bool Symset(bool m0, bool m1[], bool m2[], bool m3[], bool m4[]);
  bool ShallowSymset(bool m0, bool m1[], bool m2[], bool m3[], bool m4[]);

  int Success() {return success;};
  void getSymsetVector(int arm, int orient, long SymVec[]);
  void getShallowSymsetVector(int arm, int orient, long SymVec[]);

  void getPanelSymsetVector(int arm, int orient, int panel, long SymVec[]);
  void getPanelShallowSymsetVector(int arm, int orient, int panel, long SymVec[]);

  void getCosmics(int arm, long CosmicVec[]); 

  long getNumROCBitsSet(int arm, int orient, int iroc);
  long getNumROCBitsSet(int arm, int orient);
  long getNumUsedROCBitsSet(int arm, int orient, int chip);

  long getDiagLT(int arm, int orient, int chip);

  int getRoadSum(int arm, int orient, int flag);
  int EdgeCount(int arm, int orient, int start, int stop, int flag);
  int getRoadSumByPanel(int arm, int orient, int panel, int flag);
  int EdgeCountByPanel(int arm, int orient, int start, int stop, int panel, int flag);
 
  bool GL1_1Deep_S(); 
  bool GL1_1Deep_N(); 
  bool GL1_1Shal_S(); 
  bool GL1_1Shal_N(); 
  bool GL1_2Deep_S(); 
  bool GL1_2Deep_N(); 
  bool GL1_1Deep1Shallow_S(); 
  bool GL1_1Deep1Shallow_N(); 

  void getHitSymsetList(int arm, int orientation, int flag, std::vector<short>& list); 
  void getHitClusterList(int arm, int orientation, int flag, std::vector<short>& list); 

};

#endif
