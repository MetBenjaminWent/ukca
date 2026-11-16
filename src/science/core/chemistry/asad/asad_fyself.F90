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
! Purpose: Calculates self-reacting terms for individual species.
!
!  Part of the UKCA model, a community model supported by
!  The Met Office and NCAS, with components provided initially
!  by The University of Cambridge, University of Leeds and
!  The Met. Office.  See www.ukca.ac.uk
!
!          Called from ASAD_FTOY
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: UKCA
!
!     Method
!     ------
!     The reaction table is scanned, and the self-reacting terms are
!     calculated from the appropriate rate coefficients.
!
!     Local variables
!     ---------------
!     isp            Index of self reacting species.
!
! Code description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v6 programming standards.
!
! ---------------------------------------------------------------------
!
MODULE asad_fyself_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName = 'ASAD_FYSELF_MOD'

CONTAINS

SUBROUTINE asad_fyself(n_points)

USE asad_mod,        ONLY: qa, rk, nstst, nlstst, nspi, jpnr
USE parkind1,        ONLY: jprb, jpim
USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: LPROF_UKCA
IMPLICIT NONE

INTEGER, INTENT(IN) :: n_points    ! No of spatial points

!       Local variables

INTEGER :: j                       ! Loop variable
INTEGER :: jl                      ! Loop variable
INTEGER :: jr                      ! Loop variable
INTEGER :: js                      ! Index
INTEGER :: isp                     ! Index

INTEGER(KIND=vik) :: vnr_handle_1

CHARACTER(LEN=*), PARAMETER :: RoutineName='ASAD_FYSELF'


!       1.  Initialisation
!           --------------

IF (LPROF_UKCA) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)
DO j = 1, nstst
  js = nlstst(j)
  DO jl = 1, n_points
    qa(jl,js) = 0.0
  END DO
END DO

!       2.  Calculate self-reacting terms
!           --------- ------------- -----

DO jr = 1, jpnr
  isp = nspi(jr,1)
  IF ( isp == nspi(jr,2) ) THEN
    DO jl = 1, n_points
      qa(jl,isp) = qa(jl,isp) + 2.0 * rk(jl,jr)
    END DO
  END IF
END DO

IF (LPROF_UKCA) CALL vernier_stop(vnr_handle_1)
RETURN
END SUBROUTINE asad_fyself
END MODULE asad_fyself_mod
