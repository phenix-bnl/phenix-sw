#include<TMuiPseudoLL1.h>
#include<TDataType.h>

ClassImp(TMuiPseudoLL1)
ClassImp(TMuiPseudoLL1_v1)
  
TMuiPseudoLL1_v1::TMuiPseudoLL1_v1() :
  _arm(0),
  _index(0)
{
  zero();
}

TMuiPseudoLL1_v1::TMuiPseudoLL1_v1(const Key& key,
				     UShort_t arm,
				     UShort_t index) :
  TMuiPseudoLL1(key),
  _arm(arm),
  _index(index)
{
  zero();
}


TMuiPseudoLL1_v1::TMuiPseudoLL1_v1(const TMuiPseudoLL1* base_ptr) :
  TMuiPseudoLL1(*base_ptr),
  _arm(base_ptr->get_arm()),
  _index(base_ptr->get_index())
{
  _emulator_decision = base_ptr->get_emulator_decision();

  for (int iword = 0; iword < MUIOO::kLL1_NSymsetWords; iword++) 
    { 
      _symsetword_deep_horiz[iword] = base_ptr->get_symsetword_deep_horiz(iword);
      _symsetword_deep_vert[iword] = base_ptr->get_symsetword_deep_vert(iword);
      _symsetword_shallow_horiz[iword] = base_ptr->get_symsetword_shallow_horiz(iword);
      _symsetword_shallow_vert[iword] = base_ptr->get_symsetword_shallow_vert(iword);
    }
}

TMuiPseudoLL1_v1::TMuiPseudoLL1_v1(const TMuiPseudoLL1& base_ref) :
  TMuiPseudoLL1(base_ref),
  _arm(base_ref.get_arm()),
  _index(base_ref.get_index())
{
  _emulator_decision = base_ref.get_emulator_decision();

  for (int iword = 0; iword < MUIOO::kLL1_NSymsetWords; iword++) 
    { 
      _symsetword_deep_horiz[iword] = base_ref.get_symsetword_deep_horiz(iword);
      _symsetword_deep_vert[iword] = base_ref.get_symsetword_deep_vert(iword);
      _symsetword_shallow_horiz[iword] = base_ref.get_symsetword_shallow_horiz(iword);
      _symsetword_shallow_vert[iword] = base_ref.get_symsetword_shallow_vert(iword);
    }
}

void TMuiPseudoLL1_v1::zero() 
{
  _emulator_decision = 0;

  for (int iword = 0; iword < MUIOO::kLL1_NSymsetWords; iword++) 
    { 
      _symsetword_deep_horiz[iword] = 0;
      _symsetword_deep_vert[iword] = 0;
      _symsetword_shallow_horiz[iword] = 0;
      _symsetword_shallow_vert[iword] = 0;
    }
  return;
}









