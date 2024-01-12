#ifndef __RUN14AUAU200DEPRECAL_H__
#define __RUN14AUAU200DEPRECAL_H__

#include "Recalibrator.h"
#include <string>

class PHCompositeNode;
class PHCentralTrack;
class PHGlobal;
class TH1F;
class TH2F;

class Run14AuAu200DepRecal : public Recalibrator
{
public:
    Run14AuAu200DepRecal(const std::string &name = "Run14AuAu200DepRecal");
    virtual ~Run14AuAu200DepRecal() {}  

    static const int NSECT_PbSc = 6; // 0-5: W0, W1, W2, W3, E3, E2
    static const int NSECT_PbGl = 2; // 0-1: E1, E0

    //Recalibration parameters for functional forms
    float meanPbSc_params[NSECT_PbSc][3];
    float meanPbGl_params[NSECT_PbGl][5];
    float sigmaPbSc_params[NSECT_PbSc][4];
    float sigmaPbGl_params[NSECT_PbGl][5];
    
    int process_event(PHCompositeNode *topNode);
    int InitRun(PHCompositeNode *topNode);
    int Init(PHCompositeNode *topNode);
    int isValidRun(const int runno) const;
    void help();
    void initialize_parameters();

protected:

    int runNumber;
    PHCentralTrack *d_cnt;
    PHGlobal *d_global;
    int Calibrate_Run14AuAu200GeV();
    float calculate_dep(int sector, float pt, float eop);
    float sigmaFunc(float pt, int sect);
    float meanFunc(float pt, int sect);
};

#endif /* __RUN14AUAU200ELECTRONEMCMATCHING_H__ */
