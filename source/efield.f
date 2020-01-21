c
c
c     ###################################################
c     ##  COPYRIGHT (C)  2019                          ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ########################################################################
c     ##                                                                    ##
c     ##  module efield  --  related to electric field calculations         ##
c     ##                                                                    ##
c     ########################################################################
c
c
c     atomclass1       atom numbers of residues in class1 (E.g active site residues)
c     atomclass2       atom numbers of residues in class2 (Eg. scaffold)
c     class1count      Number of atoms in class1
c     class2count      Number of atoms in class2
c     ecprobe          atom numbers that define probes
c     numprobes        Number of atoms in ecprobe
c     ecmpole          Multipoles of each residue
c     ecfieldx         Direct Electric field at probe due to each residue postion(x-dir)
c     ecfieldy         Direct Electric field at probe due to each residue postion(y-dir)


      module efield
      implicit none
      real*8, allocatable :: fielde(:,:)
      save
      end