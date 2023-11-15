*CMZ :  2.04/00 08/10/92  23.36.25  by  Charles F. Maguire
*-- Author :    Charles F. Maguire   08/10/92
      subroutine writdumm
      implicit none
c
c     dummy write entry points (not used for reading HIJET files)
c
      entry write_code_bank
      print *,'  call to write_code_bank'
      return
      entry write_run_bank
      print *,'  call to write_run_bank'
      return
      end
