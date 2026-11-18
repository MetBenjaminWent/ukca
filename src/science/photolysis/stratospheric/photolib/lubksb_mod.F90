! *****************************COPYRIGHT*******************************

! (c) [University of Cambridge] [2008]. All rights reserved.
! This routine has been licensed to the Met Office for use and
! distribution under the UKCA collaboration agreement, subject
! to the terms and conditions set out therein.
! [Met Office Ref SC138]

! *****************************COPYRIGHT*******************************
MODULE lubksb_mod

USE parkind1, ONLY: jprb, jpim
USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: lprof_ukca
IMPLICIT NONE


! Description:
!     For matrix inversion

!  UKCA is a community model supported by The Met Office and
!  NCAS, with components initially provided by The University of
!  Cambridge, University of Leeds and The Met Office. See
!  www.ukca.ac.uk

! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: UKCA

!  Code Description:
!    Language:  Fortran 95
!    This code is written to UMDP3 standards.


CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='LUBKSB_MOD'

CONTAINS
SUBROUTINE lubksb(a,n,np,indx,b)
USE parkind1, ONLY: jprb, jpim
USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: lprof_ukca
IMPLICIT NONE

! Subroutine interface
INTEGER, INTENT(IN) :: n
INTEGER, INTENT(IN) :: np
INTEGER, INTENT(IN) :: indx(n)
REAL, INTENT(IN)    :: a(np,np)
REAL, INTENT(IN OUT) :: b(n)

! Local variables
INTEGER :: i
INTEGER :: ii
INTEGER :: j
INTEGER :: ll

REAL :: var_sum

INTEGER(KIND=vik) :: vnr_handle_1

CHARACTER(LEN=*), PARAMETER :: RoutineName='LUBKSB'


IF (lprof_ukca) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)
ii=0
DO i=1,n
  ll=indx(i)
  var_sum=b(ll)
  b(ll)=b(i)
  IF (ii /= 0) THEN
    DO j=ii,i-1
      var_sum=var_sum-a(i,j)*b(j)
    END DO
  ELSE IF (var_sum /= 0.0) THEN
    ii=i
  END IF
  b(i)=var_sum
END DO
DO i=n,1,-1
  var_sum=b(i)
  IF (i < n) THEN
    DO j=i+1,n
      var_sum=var_sum-a(i,j)*b(j)
    END DO
  END IF
  b(i)=var_sum/a(i,i)
END DO
IF (lprof_ukca) CALL vernier_stop(vnr_handle_1)
RETURN
END SUBROUTINE lubksb
END MODULE lubksb_mod
