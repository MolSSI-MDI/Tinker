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
 1    use mdi , only : MDI_NAME_LENGTH
      integer :: mdi_comm = 0
      logical :: mdi_exit = .false.
      logical :: use_mdi = .false.
      character(len=MDI_NAME_LENGTH) :: target_node = " "
      save
      contains
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine init_mdi  --  Initialize MDI Library            ##
c     ##                                                             ##
c     #################################################################
c
      subroutine init_mdi
      use argue
      use iounit
      use efield
      use mpole
 1    use iso_c_binding
 2    use mdi , only : MDI_Init, MDI_Accept_Communicator
      implicit none
      logical found_mdi
      integer i
      integer mpi_comm, ierr
      character*240 mdi_options
      character*240 string
      procedure(execute_command), pointer :: generic_command => null()
      type(c_ptr)                         :: class_obj
      generic_command => execute_command
      class_obj = c_null_ptr
c
c     check for the -mdi command line argument
c
      use_mdi = .false.
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
        use_mdi = .true.
        mpi_comm = 0
        call MDI_Init(mdi_options, mpi_comm, ierr)
        if ( ierr .ne. 0 ) then
           write(iout,*)'INIT_MDI -- Could not initalize MDI'
           call fatal
        end if
c
c     set the execute_command callback function
c
        CALL MDI_Set_Execute_Command_Func(generic_command, class_obj,
     &                                  ierr)
c
c     accept an MDI communicator
c
        call MDI_Accept_Communicator(mdi_comm, ierr)
        if ( ierr .ne. 0 ) then
           write(iout,*)'INIT_MDI -- Could not accept MDI communicator'
           call fatal
        end if
      end if
      mdi_exit = .false.

      return
      end subroutine init_mdi

c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine exit_mdi  --  Exit       MDI Library            ##
c     ##                                                             ##
c     #################################################################
c

      subroutine exit_mdi
      use efield
      mdi_exit = .true.
      if (allocated(fielde)) deallocate (fielde)
      if (allocated(dfieldx)) deallocate (dfieldx)
      if (allocated(dfieldy)) deallocate (dfieldy)
      if (allocated(dfieldz)) deallocate (dfieldz)
      return
      end subroutine exit_mdi


c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine mdi_listen  --  Listen for commands             ##
c     ##                                                             ##
c     #################################################################
c
      subroutine mdi_listen(node_name)
 1    use mdi , only : MDI_NAME_LENGTH, MDI_Recv_Command
      implicit none
      character(len=*), intent(in) :: node_name
      integer ierr
      character(len=:), allocatable :: message
      ALLOCATE( character(MDI_NAME_LENGTH) :: message )
c
c     do not listen if an "EXIT" command has been received
c
      if ( mdi_exit ) then
         return
      end if
c
c     check if this is the target node
c
      if ( target_node .ne. " " ) then
         if (target_node .eq. "@" .or. target_node .eq. node_name) then
            target_node = " "
         else
            return
         end if
      end if
c
c     listen for commands from the driver
c
      response_loop: do
c
c       receive a new command from the driver
c
        call MDI_Recv_Command(message, mdi_comm, ierr)
c
c       respond to this command
c
        call execute_command(message, mdi_comm, ierr)
c
c       check if the engine should stop listening for commands
c
        if ( target_node .ne. " " ) exit
        if ( mdi_exit ) exit
      end do response_loop
      return
      end subroutine mdi_listen
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine mdi_set_steps  --  Set the number of steps      ##
c     ##                                                             ##
c     #################################################################
c
      subroutine mdi_set_steps(istep, nstep)
      implicit none
      integer, intent(in) :: istep
      integer, intent(inout) :: nstep
      if ( mdi_exit ) then
c
c     if the EXIT command has been received, stop iterating
c
         nstep = istep
      else
c
c     otherwise, ensure that the md will continue iterating
c
         if ( nstep .le. istep ) then
            nstep = istep + 1
         end if
      end if
      return
      end subroutine mdi_set_steps
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine execute_command  --  Generic command response   ##
c     ##                                                             ##
c     #################################################################
c
      subroutine execute_command(command, comm, ierr)
      use iounit
      implicit none
      character(len=*), intent(in) :: command
      integer, intent(in)          :: comm
      integer, intent(in)          :: ierr

      select case( TRIM(command) )
      case( "EXIT" )
        call exit_mdi
      case( "<CHARGES" )
         call send_charges(comm)
      case( "<COORDS" )
         call send_coords(comm)
      case( ">COORDS" )
         call recv_coords(comm)
      case( "<NATOMS" )
         call send_natoms(comm)
      case( "<NPOLES" )
         call send_npoles(comm)
      case( "<POLES" )
         call send_poles(comm)
      case( "<FIELD" )
         call send_field(comm)
      case( "<DFIELDC" )
            call send_dfield_components(comm)
      case( ">NPROBES" )
         call recv_nprobes(comm)
      case( ">PROBES" )
            call recv_probes(comm)
      case( "@" )
         target_node = "@"
      case( "@INIT_MD" )
         target_node = "@INIT_MD"
      case( "@FORCES" )
         target_node = "@FORCES"
      case( "@INDUCE" )
         target_node = "@INDUCE"
      case default
        write(iout,*)'EXECUTE_COMMAND -- Command name not recognized: ',
     &                command
        call fatal
      end select
      return
      end subroutine execute_command
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine send_charges  --  Respond to "<CHARGES"         ##
c     ##                                                             ##
c     #################################################################
c
      subroutine send_charges(comm)
      use atoms , only  : n
      use charge , only  : nion, iion, pchg
      use iounit , only : iout
 1    use mdi , only    : MDI_DOUBLE, MDI_Send
c
c      use mpole
c
      implicit none
      integer, intent(in)          :: comm
      integer                      :: ierr, iatom
      real*8                       :: charges(n)
c
c     construct the charges array
c
      do iatom=1, n
         charges(iatom) = 0.0
      end do
      do iatom=1, nion
        charges(iion(iatom)) = pchg(iatom)
      end do
c
c     send the charges
c
      call MDI_Send(charges, n, MDI_DOUBLE, comm, ierr)
      if ( ierr .ne. 0 ) then
         write(iout,*)'SEND_CHARGES -- MDI_Send failed'
         call fatal
      end if
      return
      end subroutine send_charges
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine send_coords  --  Respond to "<COORDS"           ##
c     ##                                                             ##
c     #################################################################
c
      subroutine send_coords(comm)
      use atoms , only  : n, x, y, z
      use iounit , only : iout
 1    use mdi , only    : MDI_DOUBLE, MDI_Send, MDI_Conversion_Factor
      implicit none
      integer, intent(in)          :: comm
      integer                      :: ierr, iatom
      real*8                       :: coords(3*n)
      real*8                       :: conv
c
c     get the conversion factor from angstrom to a.u.
c
      call MDI_Conversion_Factor("angstrom", "atomic_unit_of_length",
     &                           conv, ierr)
      if ( ierr .ne. 0 ) then
         write(iout,*)'SEND_NCOORDS -- MDI_Conversion_Factor failed'
         call fatal
      end if
c
c     construct the coordinates array
c
      do iatom=1, n
        coords( 3*(iatom-1) + 1 ) = x(iatom) * conv
        coords( 3*(iatom-1) + 2 ) = y(iatom) * conv
        coords( 3*(iatom-1) + 3 ) = z(iatom) * conv
      end do
c
c     send the coordinates
c
      call MDI_Send(coords, 3*n, MDI_DOUBLE, comm, ierr)
      if ( ierr .ne. 0 ) then
         write(iout,*)'SEND_NCOORDS -- MDI_Send failed'
         call fatal
      end if
      return
      end subroutine send_coords
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine recv_coords  --  Respond to ">COORDS"           ##
c     ##                                                             ##
c     #################################################################
c
      subroutine recv_coords(comm)
      use atoms , only  : n, x, y, z
      use iounit , only : iout
 1    use mdi , only    : MDI_DOUBLE, MDI_Recv, MDI_Conversion_Factor
      implicit none
      integer, intent(in)          :: comm
      integer                      :: ierr, iatom
      real*8                       :: coords(3*n)
      real*8                       :: conv
c
c     get the conversion factor from a.u. to angstrom
c
      call MDI_Conversion_Factor("atomic_unit_of_length", "angstrom",
     &                           conv, ierr)
      if ( ierr .ne. 0 ) then
         write(iout,*)'RECV_NCOORDS -- MDI_Conversion_Factor failed'
         call fatal
      end if
c
c     receive the coordinates
c
      call MDI_Recv(coords, 3*n, MDI_DOUBLE, comm, ierr)
      if ( ierr .ne. 0 ) then
         write(iout,*)'RECV_NCOORDS -- MDI_Recv failed'
         call fatal
      end if
c
c     replace the system coords with the received coords
c
      do iatom=1, n
        x(iatom) = coords( 3*(iatom-1) + 1 ) * conv
        y(iatom) = coords( 3*(iatom-1) + 2 ) * conv
        z(iatom) = coords( 3*(iatom-1) + 3 ) * conv
      end do
      return
      end subroutine recv_coords

      end module mdiserv
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine send_natoms  --  Respond to "<NATOMS"           ##
c     ##                                                             ##
c     #################################################################
c
      subroutine send_natoms(comm)
      use atoms , only  : n
      use iounit , only : iout
 1    use mdi , only    : MDI_INT, MDI_Send
      implicit none
      integer, intent(in)          :: comm
      integer                      :: ierr
c
c     send the number of atoms
c
      call MDI_Send(n, 1, MDI_INT, comm, ierr)
      if ( ierr .ne. 0 ) then
         write(iout,*)'SEND_NATOMS -- MDI_Send failed'
         call fatal
      end if
      return
      end subroutine send_natoms
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine send_npoles  --  Respond to "<NPOLES"           ##
c     ##                                                             ##
c     #################################################################
c
      subroutine send_npoles(comm)
      use iounit , only : iout
 1    use mdi , only    : MDI_INT, MDI_Send
      use mpole , only  : npole
      implicit none
      integer, intent(in)          :: comm
      integer                      :: ierr
c
c     send the number of multipole sites
c
      call MDI_Send(npole, 1, MDI_INT, comm, ierr)
      if ( ierr .ne. 0 ) then
         write(iout,*)'SEND_NPOLES -- MDI_Send failed'
         call fatal
      end if
      return
      end subroutine send_npoles

c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine recv_nprobes --  Respond to ">NPROBES"          ##
c     ##                                                             ##
c     #################################################################
c
      subroutine recv_nprobes(comm)
         use iounit , only : iout
1        use mdi , only    : MDI_DOUBLE, MDI_INT, MDI_Recv
         use efield , only : nprobes
         implicit none
         integer, intent(in)          :: comm
         integer                      :: ierr, iprobe

c
c     receive the number of probes
c
         call MDI_Recv(nprobes, 1, MDI_INT, comm, ierr)
         if ( ierr .ne. 0 ) then
            write(iout,*)'RECV_ -- MDI_Recv failed'
            call fatal
         end if
      return
      end subroutine recv_nprobes

c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine recv_probes --  Respond to ">PROBES"            ##
c     ##                                                             ##
c     #################################################################
c
      subroutine recv_probes(comm)
        use iounit , only : iout
1       use mdi , only    : MDI_DOUBLE, MDI_INT, MDI_Recv
        use efield , only : nprobes, probes, probe_mask
        use mpole , only : npole
        implicit none
        integer, intent(in)          :: comm
        integer                      :: ierr, iprobe, i, count

        allocate (probes(nprobes))
        allocate (probe_mask(npole))

c
c     receive the probes
c
       call MDI_Recv(probes, nprobes, MDI_DOUBLE, comm, ierr)
       if ( ierr .ne. 0 ) then
          write(iout,*)'RECV_ -- MDI_Recv failed'
          call fatal
       end if

c
c     Make probe mask
c
      do i=1, npole
        probe_mask(i) = 0
      end do

      count = 1
      do i=1, nprobes
        probe_mask(probes(i)) = count
        count = count + 1
      end do

c      write (*,*) probe_mask

      return
      end subroutine recv_probes


c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine send_poles  --  Respond to "<POLES"             ##
c     ##                                                             ##
c     #################################################################
c
      subroutine send_poles(comm)
      use iounit , only : iout
 1    use mdi , only    : MDI_DOUBLE, MDI_Send, MDI_Conversion_Factor
      use mpole , only  : maxpole, npole, rpole
      implicit none
      integer, intent(in)          :: comm
      integer                      :: ierr, ipole, icomp
      real*8                       :: poles_buf(13*npole)
      real*8                       :: conv
c
c     prepare the poles buffer
c
      do ipole=1, npole
         do icomp=1, 13
            poles_buf(13*(ipole-1) + icomp) = rpole(icomp, ipole)
         end do
      end do
c
c     send the poles
c
      call MDI_Send(poles_buf, 13*npole, MDI_DOUBLE, comm, ierr)
      if ( ierr .ne. 0 ) then
         write(iout,*)'SEND_POLES -- MDI_Send failed'
         call fatal
      end if
      return
      end subroutine send_poles

c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine send_field  --  Respond to "<FIELD"             ##
c     ##                                                             ##
c     #################################################################
c
      subroutine send_field(comm)
      use atoms , only  : n
      use charge , only  : nion, iion, pchg
      use iounit , only : iout
      use efield , only : fielde
      use mpole , only : npole
1     use mdi , only    : MDI_DOUBLE, MDI_Send

      implicit none
      integer, intent(in)          :: comm
      integer                      :: ierr, ipole, dim
      real*8                       :: charges(n)
      real*8                       :: field(3*npole)

c
c     construct the field array
c
      do ipole=1, npole
         do dim=1, 3
            field(3*(ipole-1) + dim) = fielde(dim, ipole)
         end do
      end do
c
c     send the field
c
      call MDI_Send(field, 3*npole, MDI_DOUBLE, comm, ierr)
      if ( ierr .ne. 0 ) then
         write(iout,*)'SEND_CHARGES -- MDI_Send failed'
         call fatal
      end if
      return
      end subroutine send_field

c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine send_dfield_components  --  Respond to "<DFIELDC"##
c     ##                                                             ##
c     #################################################################
c
      subroutine send_dfield_components(comm)
      use iounit , only : iout
      use efield , only : dfieldx, dfieldy, dfieldz, nprobes, fielde
      use mpole , only : npole
1     use mdi , only    : MDI_DOUBLE, MDI_Send

      implicit none
      integer, intent(in)          :: comm
      integer                      :: ierr, i, j
      real*8                       :: field(3*nprobes*npole)

c
c     construct the field array
c
      do i=1, nprobes
         do j=1, npole
            field(npole*(i-1) + j) = dfieldx(j,i)
            field(npole*(i-1) + j + npole*nprobes) = dfieldy(j,i)
            field(npole*(i-1) + j + 2*npole*nprobes) = dfieldz(j,i)
         end do
      end do
c
c     send the field
c
      call MDI_Send(field, 3*npole*nprobes, MDI_DOUBLE, comm, ierr)
      if ( ierr .ne. 0 ) then
         write(iout,*)'SEND_CHARGES -- MDI_Send failed'
         call fatal
      end if
      return
      end subroutine send_dfield_components
