! *****************************COPYRIGHT*******************************
!
! (c) [University of Cambridge] [2008]. All rights reserved.
! This routine has been licensed to the Met Office for use and
! distribution under the UKCA collaboration agreement, subject
! to the terms and conditions set out therein.
! [Met Office Ref SC138]
!
! *****************************COPYRIGHT*******************************
!
! Purpose: Calculates the total number density in gridboxes
!
!  Part of the UKCA model, a community model supported by
!  The Met Office and NCAS, with components provided initially
!  by The University of Cambridge, University of Leeds and
!  The Met. Office.  See www.ukca.ac.uk
!
!          Called from ASAD_CDRIVE
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: UKCA
!
!     Method
!     ------
!     The total number density at a point is given by: p=nkt
!     p-pressure , n-number density, k-boltzmann's constant,
!     t-temperature.
!
! Code description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v6 programming standards.
!
! ---------------------------------------------------------------------
!
MODULE asad_totnud_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName = 'ASAD_TOTNUD_MOD'

CONTAINS

SUBROUTINE asad_totnud(n_points)

USE asad_mod, ONLY: tnd, p, t, pmintnd, pmin
USE ukca_config_constants_mod, ONLY: boltzmann
USE parkind1, ONLY: jprb, jpim
USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: LPROF_UKCA
IMPLICIT NONE


INTEGER, INTENT(IN) :: n_points

!       Local variables

INTEGER :: jl

REAL :: zb

INTEGER(KIND=vik) :: vnr_handle_1

CHARACTER(LEN=*), PARAMETER :: RoutineName='ASAD_TOTNUD'


IF (LPROF_UKCA) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)
zb = boltzmann*1.0e6

!       1. Total number density (1e6 converts numbers to /cm**3).
!          ----- ------ ------- ---- -------- ------- -- --------

DO jl = 1, n_points
  tnd(jl)     = p(jl) / ( zb * t(jl) )
  pmintnd(jl) = pmin * tnd(jl)
END DO

IF (LPROF_UKCA) CALL vernier_stop(vnr_handle_1)
RETURN
END SUBROUTINE asad_totnud
END MODULE asad_totnud_mod
