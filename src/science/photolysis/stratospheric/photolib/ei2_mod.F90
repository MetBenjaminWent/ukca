! *****************************COPYRIGHT*******************************

! (c) [University of Cambridge] [2008]. All rights reserved.
! This routine has been licensed to the Met Office for use and
! distribution under the UKCA collaboration agreement, subject
! to the terms and conditions set out therein.
! [Met Office Ref SC138]

! *****************************COPYRIGHT*******************************
MODULE ei2_mod

USE parkind1, ONLY: jprb, jpim
USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: lprof_ukca
IMPLICIT NONE


! Description:
!     This function calculates the second exponential integral of
!     the real number x.

!  UKCA is a community model supported by The Met Office and
!  NCAS, with components initially provided by The University of
!  Cambridge, University of Leeds and The Met Office. See
!  www.ukca.ac.uk

! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: UKCA

!  Code Description:
!    Language:  Fortran 95
!    This code is written to UMDP3 standards.


CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='EI2_MOD'

CONTAINS
REAL FUNCTION ei2(x)
USE ei1_mod, ONLY: ei1
USE parkind1, ONLY: jprb, jpim
USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: lprof_ukca
IMPLICIT NONE

! Function interface
REAL, INTENT(IN) :: x ! Argument of the second exponential integral.

! Local variables
REAL, PARAMETER :: thresh=0.10e-15
REAL, PARAMETER :: xmax=50.0

INTEGER(KIND=vik) :: vnr_handle_1

CHARACTER(LEN=*), PARAMETER :: RoutineName='EI2'

IF (lprof_ukca) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)

!     Calculate integral
IF ( x <= thresh ) THEN
  ei2 = 1.00
ELSE IF ( x >= xmax ) THEN
  ei2 = 0.0
ELSE
  ei2 = EXP(-x) - x*ei1(x)
END IF
IF (lprof_ukca) CALL vernier_stop(vnr_handle_1)
RETURN

END FUNCTION ei2
END MODULE ei2_mod
