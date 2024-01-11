#include "dTofReconstructedWrapper.h"
#include "TofOut.h"

ClassImp(TofOut)

void TofOut::FillFromWrapper(dTofReconstructedWrapper *wrap)
{
  unsigned int ihit;
  if (wrap)
    {
      set_TofNHit(wrap->RowCount());
      set_TClonesArraySize(wrap->RowCount());
      for (ihit = 0; ihit < wrap->RowCount();ihit++)
        {
          AddTofHit(ihit);
          set_id(ihit, wrap->get_id(ihit));
          set_panel(ihit, wrap->get_panel(ihit));
          set_sector(ihit, wrap->get_sector(ihit));
          set_side(ihit, wrap->get_side(ihit));
          set_slat(ihit, wrap->get_slat(ihit));
          set_slatid(ihit, wrap->get_slatid(ihit));
          for (short j = 0; j < 2;j++)
            {
              set_qvc(ihit, j, wrap->get_qvc(j, ihit));
              set_tvc(ihit, j, wrap->get_tvc(j, ihit));
            }
          set_eloss(ihit, wrap->get_eloss(ihit));
          set_eloss_err(ihit, wrap->get_eloss_err(ihit));
          set_tof(ihit, wrap->get_tof(ihit));
          set_tof_err(ihit, wrap->get_tof_err(ihit));
          for (short j = 0; j < 3;j++)
            {
              set_xtof(ihit, j, wrap->get_xtof(j, ihit));
              set_xtof_err(ihit, j, wrap->get_xtof_err(j, ihit));
            }
          set_tdiff(ihit, wrap->get_tdiff(ihit));
        }
    }
  return ;
}
