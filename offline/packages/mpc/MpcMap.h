#ifndef __MPCMAP_H__
#define __MPCMAP_H__

#include <PHTimeStamp.h>
#include <PdbMpcMap.hh>
#include <PdbMpcPinMap.hh>
#include <PHObject.h>
#include <vector>
#include <string>

class PHCompositeNode;

class MpcMap : public PHObject
{
public:
  static MpcMap *instance(PHCompositeNode *topNode = 0);
  MpcMap(PHCompositeNode *topNode = 0, const int do_download=1);
  virtual ~MpcMap();

  int getGridX(const int ifee576ch) const { return mapdata[ifee576ch].get_gridx(); }
  int getGridY(const int ifee576ch) const { return mapdata[ifee576ch].get_gridy(); }
  int getPinFeeCh(const int ifee576ch) const { return pindata[ifee576ch].get_pinfee576(); }
  int getPinFeeCh(const std::string pin_name);
  int getFeeCh(const int ix, const int iy, const int iarm) const {
    if (ix<0||ix>17||iy<0||iy>17) return -1;

    if ( iarm==0 ) return south_feech_from_gridxy[ix][iy];
    if ( iarm==1 ) return north_feech_from_gridxy[ix][iy];

    return -2;
  }

  /// Driver Board Number 1-10 in south, 11-20 in north, counter-clockwise from top
  int getDriver(const int ifee576ch) const { return mapdata[ifee576ch].get_driver(); }

  /// ASICs number from 0-23
  int getAsic(const int ifee576ch) const { return ifee576ch/24; }

  /// Mondo on the ASIC, 0-5 (24th channel on board is associated with mondo 0)
  int getMondo(const int ifee576ch) const { return 5 - (ifee576ch/4)%6; }

  /// FEM, 0-3, where 0,1 are the south inner,outer, and 2,3 are north inner,outer
  int getFem(const int ifee576ch) const { return ifee576ch/144; }

  /// 0 = South, 1 = North
  int getArm(const int ifee576ch) const { return (ifee576ch<288) ? 0 : 1; }

  /// Module Number (everything in a skin. TL is 0, going counter-clockwise, S to N)
  /// Careful, this only works for Run8+
  int getModule(const int ifee576ch) const { 
    int arm = getArm( ifee576ch );
    int ix = getGridX( ifee576ch );
    int iy = getGridY( ifee576ch );
    return module_number[arm][ix][iy];
  }
  int isModuleEdge(const int ifee576ch) const;
  void SetModules();

  /// Whether this is an active crystal channel
  int isCrystal(const int ifee576ch) { return (getGridX(ifee576ch)>=0) ? 1 : 0; }

  /// Whether this is a Pin Diode Channel
  int isPin(const int ifee576ch) { return (getGridX(ifee576ch)<-1&&getGridX(ifee576ch)>-18) ? 1 : 0; }

  /// Get physical positions
  double getX(const int ch) const { return mapdata[ch].get_x(); }
  double getY(const int ch) const { return mapdata[ch].get_y(); }
  double getZ(const int ch) const { return mapdata[ch].get_z(); }
  //const PHPoint &getPoint(int ch) const { return pos[ch]; }

  int Download_Maps(const std::string& mpcalmapfile);
  int Download_Maps(const PHTimeStamp& time);
  int Download_Maps(PHCompositeNode *topNode);

  int StoreInDatabase(PHTimeStamp& tStart,const char *username = "",const char *description = "",const PHTimeStamp& tStop = PHTimeStamp(2147483647) );
  void AddToNodeTree(PHCompositeNode *topNode);

  PHTimeStamp *getStartTime() { return &StartTime; }
  PHTimeStamp *getEndTime() { return &EndTime; }

  void Reset();
  void Print(Option_t* ="") const;
  void Dump_to_file(const std::string& outfname = "MpcCal.map");
  int IsValid(const int verbosity = 0) const;

  int setEdgeFraction(const float frac){ edge_fraction = frac; return 1;}
  void SetEdges();
  int isInAcceptance(const float x, const float y, const int gridx, const int gridy, const int arm) const;
  int isInAcceptance(const float x, const float y, const int fee576ch) const;
  int isEdge(const int fee576ch) const;

private:

  static MpcMap *__instance;

  void MakeInverseMap();

  static const int MAXCH = 576;

  int status;			// status of data, 0 = un-init, >0 is OK, <0 is bad

  PHTimeStamp StartTime;	// calibration start and end time
  PHTimeStamp EndTime;

  PdbMpcMap mapdata[MAXCH];
  PdbMpcPinMap pindata[MAXCH];

  int south_feech_from_gridxy[18][18];	// s. reverse map (grid to feech)
  int north_feech_from_gridxy[18][18];	// n. reverse map (grid to feech)

  float edge_fraction;
  int is_edge[MAXCH];  // identifies edge towers, 0 = not edge, 1 = inner edge, 2 = outer edge
  int is_dead[MAXCH];  // dead towers identified by warnmap
  int is_neardead[MAXCH];  // towers adjacent to dead towers
  
  int adjacent_towers[3][3][MAXCH];  // array containing adjacent towers of central tower

  int module_number[2][18][18];  // module number

  using TObject::Dump;  // unhide

  ClassDef(MpcMap,0)
};

#endif	// __MPCMAP_H__

