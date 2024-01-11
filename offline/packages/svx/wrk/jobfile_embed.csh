#!/bin/tcsh
setenv LD_LIBRARY_PATH /phenix/u/workarea/lebedev/offline/packages/svx/install/lib:${LD_LIBRARY_PATH}
#setenv LD_LIBRARY_PATH /phenix/u/workarea/lebedev/tmp/install/lib:${LD_LIBRARY_PATH}
printenv OFFLINE_MAIN
echo "input parameters: " $1 $2
#@ new1 = $1 + 2;
#echo "new par1 = " $new1
#root -b -q svxembed_sim_RUN11.C\(10000,\"/phenix/subsys/vtx/singles/pisafiles/PISAEvent_10kevts_bbbar_1$1$2.root\",\"/phenix/zdata03/phnxreco/VTX/lebedev/svxhits/svxhits_347129_000$2_0-20cent_vtx1svx.dat\",\"/phenix/zdata03/phnxreco/VTX/lebedev/simdst/simDST_bbbar_1$1$2_merged${2}_0-20cent_pro90.root\"\)\;
#root -b -q svxembed_sim_RUN11.C\(10000,\"/phenix/subsys/vtx/singles/pisafiles/PISAEvent_electrons_1$1$2.root\",\"/phenix/zdata03/phnxreco/VTX/lebedev/svxhits/svxhits_347129_000$2_0-20cent_vtx1svx.dat\",\"/phenix/zdata03/phnxreco/VTX/lebedev/simdst/simDST_electrons_1$1$2_merged${2}_0-20cent_pro90.root\"\)\;
root -b -q svxembed_sim_RUN11.C\(10000,\"/phenix/subsys/vtx/singles/pisafiles/PISAEvent_ddbar_1$1$2_sh_347129_0${2}.root\",\"/phenix/zdata03/phnxreco/VTX/lebedev/svxhits/svxhits_347129_000$2_0-20cent_vtx1svx.dat\",\"/phenix/zdata03/phnxreco/VTX/lebedev/simdst/simDST_ddbar_1$1$2_merged${2}_0-20cent_testshift.root\"\)\;
#root -b -q svxembed_sim_RUN11.C\(10000,\"/phenix/subsys/vtx/singles/pisafiles/PISAEvent_bbbar_1$1$2_sh_347129_0${2}.root\",\"/phenix/zdata03/phnxreco/VTX/lebedev/svxhits/svxhits_347129_000$2_0-20cent_vtx1svx.dat\",\"/phenix/zdata03/phnxreco/VTX/lebedev/simdst/simDST_bbbar_1$1$2_merged${2}_0-20cent_testshift.root\"\)\;
#root -b -q svxembed_sim_RUN11.C\(10000,\"/phenix/subsys/vtx/singles/pisafiles/PISAEvent_electrons_1$1$2_sh${2}.root\",\"/phenix/zdata03/phnxreco/VTX/lebedev/svxhits/svxhits_347129_000$2_0-20cent_vtx1svx.dat\",\"/phenix/zdata03/phnxreco/VTX/lebedev/simdst/simDST_electrons_1$1$2_merged${2}_0-20cent_testshift.root\"\)\;
#root -b -q svxembed_sim_RUN11.C\(10000,\"/phenix/subsys/vtx/singles/pisafiles/PISAEvent_positrons_1$1$2_sh${2}.root\",\"/phenix/zdata03/phnxreco/VTX/lebedev/svxhits/svxhits_347129_000$2_0-20cent_vtx1svx.dat\",\"/phenix/zdata03/phnxreco/VTX/lebedev/simdst/simDST_positrons_1$1$2_merged${2}_0-20cent_testshift.root\"\)\;


