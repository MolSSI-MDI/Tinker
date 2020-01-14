c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1996  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     #################################################################
c     ##                                                             ##
c     ##  module mdiserv  --  MDI server interface                   ##
c     ##                                                             ##
c     #################################################################
c
c
c     "init_mdi" checks for the argument to the -mdi command line
c     option and if it is found, initializes the MDI Library
c
c
      module mdiserv

      contains

      subroutine init_mdi
      use argue
 1    use mdi,         only : MDI_Init
      implicit none
      logical found_mdi
      integer i
      integer mpi_comm, ierr
      character*240 mdi_options
      character*240 string
c
c     check for a keyfile specified on the command line
c
      found_mdi = .false.
      do i = 1, narg-1
         string = arg(i)
         call upcase (string)
         if (string(1:4) .eq. '-MDI') then
            mdi_options = arg(i+1)
            found_mdi = .true.
         end if
      end do
c
c     initialize the MDI Library
c
      if ( found_mdi ) then
        mpi_comm = 0
        call MDI_Init(mdi_options, mpi_comm, ierr)
      end if

      return
      end

      end module mdiserv
