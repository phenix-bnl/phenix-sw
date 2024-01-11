#ifndef  __PBSCTIMINGFIXES_H__
#define  __PBSCTIMINGFIXES_H__

/** (OLD) Various timing corrections.
    @ingroup oldemccalib
 */

class pbscTimingFixes
{

private:
  float femTPattern[144];
  float globalT0[6];
  float smT0[108];
  float smTRes[108];
  float towerT0[108][144];

  void loadCommonFixes();
  void loadFixes2_1();
  void loadTowerT0_1();
  void loadFixes2_2();
  void loadTowerT0_2();

protected:
  pbscTimingFixes() {}

  virtual ~pbscTimingFixes() {}

  static pbscTimingFixes * single;
  static bool fixesLoaded;
public:
  static pbscTimingFixes * getInstance()
  {
    if (!single)
      single = new pbscTimingFixes();
    return single;
  }
  static void deleteInstance()
  {
    delete single;
  }
  void loadFixes(int runNumber)
  {
    loadCommonFixes();
    if (runNumber < 29987)
      loadFixes2_1();
    else
      loadFixes2_2();
    fixesLoaded = true;
  }
  inline float getSectorT0(int S)
  {
    return globalT0[S];
  }
  inline float getSMT0(int SM)
  {
    return smT0[SM];
  }
  inline float getTowerT0(int SM, int T)
  {
    return towerT0[SM][T];
  }
  inline float getSMTRes(int SM)
    {
      return smTRes[SM];
    }
    inline float getFEMTPattern(int iSMT)
    {
      return femTPattern[iSMT];
    }
    inline bool areFixesLoaded()
    {
      return fixesLoaded;
    }

  public:
    //  ClassDef(timingFixes,1)
  };
#endif
