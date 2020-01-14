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
c


      module efield
      implicit none
      integer nprobes;
      integer, allocatable :: probes(:)
      integer, allocatable :: probe_mask(:)
      real*8, allocatable :: fielde(:,:)
      real*8, allocatable :: dfieldx(:,:)
      real*8, allocatable :: dfieldy(:,:)
      real*8, allocatable :: dfieldz(:,:)
      save
      end
