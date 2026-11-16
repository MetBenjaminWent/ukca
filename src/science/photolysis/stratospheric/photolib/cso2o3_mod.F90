! *****************************COPYRIGHT*******************************

! (c) [University of Cambridge] [2008]. All rights reserved.
! This routine has been licensed to the Met Office for use and
! distribution under the UKCA collaboration agreement, subject
! to the terms and conditions set out therein.
! [Met Office Ref SC138]

! *****************************COPYRIGHT*******************************
MODULE cso2o3_mod

USE parkind1, ONLY: jprb, jpim
USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: LPROF_UKCA
IMPLICIT NONE


! Description:
!     Calculate current O2 and O3 cross sections

!  UKCA is a community model supported by The Met Office and
!  NCAS, with components initially provided by The University of
!  Cambridge, University of Leeds and The Met Office. See
!  www.ukca.ac.uk

! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: UKCA

!  Code Description:
!    Language:  Fortran 95
!    This code is written to UMDP3 standards.


CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CSO2O3_MOD'

CONTAINS
SUBROUTINE cso2o3(ao2sr,ao3,tempc,tspo2,jw,jpwav)
USE acso3w_mod, ONLY: acso3w
USE acssrw_mod, ONLY: acssrw
USE parkind1, ONLY: jprb, jpim
USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: LPROF_UKCA
IMPLICIT NONE

! Subroutine interface
INTEGER, INTENT(IN) :: jpwav
INTEGER, INTENT(IN) :: jw
REAL, INTENT(IN)    :: tempc
REAL, INTENT(IN)    :: tspo2
REAL, INTENT(IN OUT) :: ao2sr(jpwav)
REAL, INTENT(IN OUT) :: ao3(jpwav)

INTEGER(KIND=vik) :: vnr_handle_1

CHARACTER(LEN=*), PARAMETER :: RoutineName='CSO2O3'


!     Calculate the T dependent O3 cross-section.
IF (LPROF_UKCA) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)
IF ( jw >= 84 .AND. jw <= 102 ) THEN
  CALL acso3w(jw,tempc,jpwav,ao3)
END IF

!     Calculate O2 cross section using the Frederick parameterisation.
!     Note - this depends on the slant path column.
IF ( jw >= 46 .AND. jw <= 62 ) THEN
  CALL acssrw(tspo2,jw,jpwav,ao2sr)
END IF
IF (LPROF_UKCA) CALL vernier_stop(vnr_handle_1)
RETURN

END SUBROUTINE cso2o3
END MODULE cso2o3_mod
