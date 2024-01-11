#ifndef __MUTRGKEY__
#define __MUTRGKEY__

class MutrgKey{
public:
  MutrgKey(void){;}
  virtual ~MutrgKey(){;}

  static unsigned int LocToKey(int arm,int station,int octant,int halfoctant,
			       int gap,int cathode,int strip);
  static unsigned int LocToKey(int arm,int station,int octant,int halfoctant,
			       int strip);
  static void KeyToLoc(unsigned int key,
		       int &arm,int &station,int &octant,int &halfoctant,
		       int &gap,int &cathode,int &strip);
  static void KeyToLoc(unsigned int key,
		       int &arm,int &station,int &octant,int &halfoctant,
		       int &strip);

  static int KeyToArm(unsigned int key);
  static int KeyToStation(unsigned int key);
  static int KeyToOctant(unsigned int key);
  static int KeyToHalfOctant(unsigned int key);
  static int KeyToGap(unsigned int key);
  static int KeyToCathode(unsigned int key);
  static int KeyToStrip(unsigned int key);

  static unsigned int Mask(bool arm,bool station,bool octant,
			   bool halfoctant,bool gap,bool cathode,bool strip);
  static unsigned int Mask(bool arm,bool station,bool octant,
			   bool halfoctant,bool strip);
  static unsigned int Mask(unsigned int key,
			   bool arm,bool station,bool octant,
			   bool halfoctant,bool gap,bool cathode,bool strip);
  static unsigned int Mask(unsigned int key,
			   bool arm,bool station,bool octant,
			   bool halfoctant,bool strip);

  static const unsigned short KEY_SHIFT_ARM;        // =17;
  static const unsigned short KEY_SHIFT_STATION;    // =15;
  static const unsigned short KEY_SHIFT_OCTANT;     // =12;
  static const unsigned short KEY_SHIFT_HALFOCTANT; // =11;
  static const unsigned short KEY_SHIFT_GAP;        // = 9;
  static const unsigned short KEY_SHIFT_CATHODE;    // = 8;
  static const unsigned short KEY_SHIFT_STRIP;      // = 0;
  static const unsigned short KEY_MASK_ARM;        // =0x01;
  static const unsigned short KEY_MASK_STATION;    // =0x03;
  static const unsigned short KEY_MASK_OCTANT;     // =0x07;
  static const unsigned short KEY_MASK_HALFOCTANT; // =0x01;
  static const unsigned short KEY_MASK_GAP;        // =0x03;
  static const unsigned short KEY_MASK_CATHODE;    // =0x01;
  static const unsigned short KEY_MASK_STRIP;      // =0xff;
};

#endif /* __MUTRGKEY__ */
