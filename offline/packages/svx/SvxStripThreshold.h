#ifndef __SVXSTRIPTHRESHOLD_HH_
#define __SVXSTRIPTHRESHOLD_HH_

#include "SvxParameters.h"
#include <string>

class svxAddress;
class PHCompositeNode;
class PHTimeStamp;

class SvxStripThreshold {
  public:
    SvxStripThreshold(PHCompositeNode*);
    SvxStripThreshold(svxAddress*);
    virtual ~SvxStripThreshold() {}

    void Verbosity(const int level) { verbosity = level;}

    void set_UseDatabase(const bool yesno) {UseDatabase=yesno;}

    bool readFromFile(const std::string &filename = "DeadChannelMap_Stripixel_0000347129-0000.txt");
    bool readFromThresholdFile(const std::string &filename = "threshold.h");
    bool readFromDatabase(PHTimeStamp * T);
    bool writeToDatabase(PHTimeStamp * Tbeg, PHTimeStamp * Tend);

    int getThreshold (const int layer,
                      const int ladder,
                      const int sensor,
                      const int roc,
                      const int rocchannel) const;

    int getThreshold (const int layer,
                      const int ladder,
                      const int sensor,
                      const int sensorsection,
                      const int sensorreadout,
                      const int channel) const;

    void print();
    void print(const int layer, const int ladder, const int sensor) const;
    void printROC(const int layer, const int ladder, const int sensor) const;

  private:

    bool UseDatabase;
    svxAddress* SvxAddressObject;
// layer; ladder; sensor; ROC; ROC Channel
    int thresh[SVXLAYERNUMBER-2][SVXLADDERNUMBER][SVXSENSORNUMBER][12][128];
    int verbosity;

};

#endif

