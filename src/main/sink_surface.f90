!--------------------------------------------------------------------------!
! The Phantom Smoothed Particle Hydrodynamics code, by Daniel Price et al. !
! Copyright (c) 2007-2025 The Authors (see AUTHORS)                        !
! See LICENCE file for usage and distribution conditions                   !
! http://phantomsph.github.io/                                             !
!--------------------------------------------------------------------------!
module sinksurface
!
! Various implementations for boundary conditions at the sink surface(s)
!
! :References:
!
! :Owner: Daniel Price
!
! :Runtime parameters:
!   - isurface  : *sink boundary model (0=surfaceless, 1=force field, 2=ghost, 3=resurrection)*
!
! :Dependencies: infile_utils, physcon, units
!
 implicit none

 public  :: write_options_surface,read_options_surface

 integer, public    :: isurface  = 0
 integer, parameter :: isurface_max = 3  ! maximum allowed value of isurface
 real, public :: rotW      = 0.
 real, public :: damp,r1in,r2in
 integer, public :: nghosts,nshells
 real, public :: Rres

contains

!-----------------------------------------------------------------------
!+
!  writes sink surface options to the input file
!+
!-----------------------------------------------------------------------
subroutine write_options_surface(iunit)
 use infile_utils, only:write_inopt
 integer, intent(in) :: iunit

 ! do not write sink surface options if isurface == 0 (i.e. it is a hidden option)
 if (isurface <= 0) return

 write(iunit,"(/,a)") '# options controlling sink surface model'
 call write_inopt(isurface,'isurface','sink surface model (0=surfaceless, 1=force field, 2=ghost, 3=resurrection)',iunit)
 call write_inopt(rotW,'rotW','fraction of critical rotation',iunit)

 select case(isurface)
 case(1)  ! option using pressure and velocity damping field in region about the sink
    call write_inopt(damp,'damp','damping timescale as fraction of orbital timescale',iunit)
    call write_inopt(r1in,'r1in','inner boundary of inner disc damping zone',iunit)
    call write_inopt(r2in,'r2in','outer boundary of inner disc damping zone',iunit)
 case(2)
    call write_inopt(nghosts,'nghosts','number of ghost boundary particles per shell',iunit)
    call write_inopt(nshells,'nshells','number of boundary shells',iunit)
 case(3)
    call write_inopt(Rres,'Rres','resurrection radius of accreted particles',iunit)
 end select

end subroutine write_options_surface

!-----------------------------------------------------------------------
!+
!  reads sink surface options from the input file
!+
!-----------------------------------------------------------------------
subroutine read_options_surface(db,nerr)
 use infile_utils, only:inopts,read_inopt
 type(inopts), intent(inout) :: db(:)
 integer,      intent(inout) :: nerr

 call read_inopt(isurface,'isurface',db,errcount=nerr,min=0,max=isurface_max,default=0)
 call read_inopt(rotW,'rotW',db,errcount=nerr,min=0.,max=1.,default=0.)

 select case(isurface)
 case(1)
    call read_inopt(damp,'damp',db,errcount=nerr,min=0.,max=1.,default=0.)
    call read_inopt(r1in,'r1in',db,errcount=nerr,min=0.)
    call read_inopt(r2in,'r2in',db,errcount=nerr,min=r1in)
 case(2)
    call read_inopt(nghosts,'nghosts',db,errcount=nerr,min=0)
    call read_inopt(nshells,'nshells',db,errcount=nerr,min=0)
 case(3)
    call read_inopt(Rres,'Rres',db,errcount=nerr,min=0)
 end select

end subroutine read_options_surface



end module sinksurface
