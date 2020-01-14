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
      integer mdi_comm
      logical mdi_terminate
      logical use_mdi
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
      use iso_c_binding
 1    use mdi , only : MDI_Init, MDI_Accept_Communicator
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
      mdi_terminate = .false.

      return
      end subroutine init_mdi
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine mdi_listen  --  Listen for commands             ##
c     ##                                                             ##
c     #################################################################
c
      subroutine mdi_listen()
 1    use mdi , only : MDI_NAME_LENGTH, MDI_Recv_Command
      implicit none
      integer ierr
      character(len=:), allocatable :: message
      ALLOCATE( character(MDI_NAME_LENGTH) :: message )
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
        if ( mdi_terminate ) exit
      end do response_loop
      return
      end subroutine mdi_listen
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
        mdi_terminate = .true.
      case( "<NATOMS" )
         call send_natoms(comm)
      case( "<COORDS" )
         call send_coords(comm)
      case( ">COORDS" )
         call recv_coords(comm)
      case default
        write(iout,*)'EXECUTE_COMMAND -- Command name not recognized'
        call fatal
      end select
      return
      end subroutine execute_command
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
      call MDI_Conversion_Factor("angstrom", "atomic_units_of_length", 
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
      call MDI_Send(coords, n, MDI_DOUBLE, comm, ierr)
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
      call MDI_Conversion_Factor("atomic_units_of_length", "angstrom", 
     &                           conv, ierr)
      if ( ierr .ne. 0 ) then
         write(iout,*)'RECV_NCOORDS -- MDI_Conversion_Factor failed'
         call fatal
      end if
c
c     receive the coordinates
c
      call MDI_Recv(coords, n, MDI_DOUBLE, comm, ierr)
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
