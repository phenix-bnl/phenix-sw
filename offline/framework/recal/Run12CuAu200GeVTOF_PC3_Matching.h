#ifndef __Run12CuAu200GeVTOF_PC3_Matching_H__
#define __Run12CuAu200GeVTOF_PC3_Matching_H__

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbBankID.hh>
#include <RunToTime.hh>

#include <PdbFloatVector.hh>

#include <string>
#include <Recalibrator.h>

class PHCompositeNode;

class Run12CuAu200GeVTOF_PC3_Matching : public Recalibrator
{
public:

    static const int pc3_zed_bins  = 10;
    static const int pc3_pt_bins   = 18;
    static const int pc3_cent_bins =  9;
    static const int pc3_num_det   =  8;

    static const int tof_zed_bins  = 10;
    static const int tof_pt_bins   = 18;
    static const int tof_cent_bins =  9;
    static const int tof_num_det   =  8;



//  float point_lines_pc3[9][8][10][15][4];
//  float point_lines_pc3_II[9][8][10][15][4];


    double mean_tofe_offset[4*960];
    double mean_tofw_offset[4*512];





    double pc3_point_lines[pc3_cent_bins][pc3_num_det][pc3_zed_bins][pc3_pt_bins-1][4];
    double tof_point_lines[tof_cent_bins][tof_num_det][tof_zed_bins][tof_pt_bins-1][4];




    double point_lines_tof[9][8][10][15][4];
    double point_lines_tof_II[9][8][10][15][4];


    Run12CuAu200GeVTOF_PC3_Matching(const std::string &name="Run12CuAu200GeVTOF_PC3_Matching");
    virtual ~Run12CuAu200GeVTOF_PC3_Matching() {}
    int isValidRun(const int runno) const;
    int InitRun(PHCompositeNode *topNode);
    int process_event(PHCompositeNode *topNode);



    double pc3sdphi_func(float charge, short dcarm, int iCent, int iZed, float pt, float pc3dphi);
    double pc3sdz_func(  float charge, short dcarm, int iCent, int iZed, float pt, float pc3dz);
    void  init_pc3_fit_pars(int run_num);

//  float pc3sdphi_func_II(float charge, short dcarm, int iCent, int iZed, float pt, float pc3dphi);
//  float pc3sdz_func_II(  float charge, short dcarm, int iCent, int iZed, float pt, float pc3dz);
//  void  init_pc3_fit_pars_II();


    double tofsdphi_func(float charge, short dcarm, int iCent, int iZed, float pt, float tofdphi);
    double tofsdz_func(  float charge, short dcarm, int iCent, int iZed, float pt, float tofdz);
    void  init_tofe_fit_pars(int run_num);
    void  init_tofw_fit_pars(int run_num);

//  float tofsdphi_func_II(float charge, short dcarm, int iCent, int iZed, float pt, float tofdphi);
//  float tofsdz_func_II(  float charge, short dcarm, int iCent, int iZed, float pt, float tofdz);
//  void  init_tof_fit_pars_II();

private:

    int run_num;
    bool SlatCheck(int array[], int slat, int first, int last);


};

#endif /* __Run12CuAu200GeVTOF_PC3_Matching_H__ */
