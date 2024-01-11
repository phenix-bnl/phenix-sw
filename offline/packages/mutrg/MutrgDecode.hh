#ifndef __MUTRGDECODE__
#define __MUTRGDECODE__

#include <vector>
#include <string>

class MutrgHeader;
class MutrgHitArray;

class MutrgDecode{
public:
  MutrgDecode(void);
  virtual ~MutrgDecode(void){;}
  void Reset(void);

  int Decode(int num,std::vector<unsigned int> &data_array,
	     MutrgHeader *mutrg_header,MutrgHitArray *mutrg_hits);
  int Decode(MutrgHitArray *mutrg_hits);
  int DecodeF0(MutrgHitArray *mutrg_hits,int nheader,int nfooter);

  int FillHeader(MutrgHeader *mh);
  int FillHeaderF0(MutrgHeader *mh);
  int FillHeaderF1(MutrgHeader *mh);
  int FillHeaderF2(MutrgHeader *mh);
  int FillHeaderF3(MutrgHeader *mh);

  int MaskMutrgHit(MutrgHitArray *mutrg_hits,
		   int arm,int station,int octant,int halfoctant);

  void SetMutrgHeader(MutrgHeader *mh){mutrg_header=mh;}
  void SetData(std::vector<unsigned int> &data_array){data=data_array;}

protected:
  std::string class_name;

  MutrgHeader *mutrg_header;
  std::vector<unsigned int> data;
};

#endif /* __MUTRGDECODE__ */
