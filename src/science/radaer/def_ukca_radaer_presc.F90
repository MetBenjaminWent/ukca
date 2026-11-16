! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Description:
!  Module defining and managing structures to store UKCA RADAER
!  prescribed distributions.
!
! Method:
!
!  Provide structure and memory allocation/deallocation routines.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: UKCA
!
! Code description:
!   Language: FORTRAN 95
!   This code is written to UMDP3 programming standards.
!
! Contained subroutines in this module:
!   allocate_ukca_radaer_presc           (Public)
!   deallocate_ukca_radaer_presc         (Public)
!
! --------------------------------------------------------------------------
MODULE def_ukca_radaer_presc

IMPLICIT NONE

!
! Optical properties: 3D distributions with also a dependence on
! spectral waveband.
!
TYPE :: t_ukca_radaer_presc
  INTEGER :: dim1
  INTEGER :: dim2
  INTEGER :: dim3
  INTEGER :: dim4
  REAL, ALLOCATABLE :: extinction(:,:,:,:)
  REAL, ALLOCATABLE :: absorption(:,:,:,:)
END TYPE t_ukca_radaer_presc

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='DEF_UKCA_RADAER_PRESC'

CONTAINS

!
! Memory allocation routines
!
SUBROUTINE allocate_ukca_radaer_presc(rad, row_length, rows, model_levels,     &
                                      n_wavebands)

USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: LPROF_UKCA
USE parkind1,        ONLY: jprb, jpim

IMPLICIT NONE

INTEGER, INTENT(IN) :: row_length
INTEGER, INTENT(IN) :: rows
INTEGER, INTENT(IN) :: model_levels
INTEGER, INTENT(IN) :: n_wavebands

TYPE (t_ukca_radaer_presc), INTENT(IN OUT) :: rad

INTEGER(KIND=vik) :: vnr_handle_1

CHARACTER (LEN=*), PARAMETER :: RoutineName = 'ALLOCATE_UKCA_RADAER_PRESC'

IF (LPROF_UKCA) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)

rad%dim1 = row_length
rad%dim2 = rows
rad%dim3 = model_levels
rad%dim4 = n_wavebands

IF (.NOT. ALLOCATED(rad%extinction)) THEN
  ALLOCATE(rad%extinction(rad%dim1, rad%dim2, rad%dim3, rad%dim4))
END IF
rad%extinction(:,:,:,:) = 0.0

IF (.NOT. ALLOCATED(rad%absorption)) THEN
  ALLOCATE(rad%absorption(rad%dim1, rad%dim2, rad%dim3, rad%dim4))
END IF
rad%absorption(:,:,:,:) = 0.0

IF (LPROF_UKCA) CALL vernier_stop(vnr_handle_1)
RETURN

END SUBROUTINE allocate_ukca_radaer_presc

!
! Memory deallocation routines
!
SUBROUTINE deallocate_ukca_radaer_presc(rad)

IMPLICIT NONE

TYPE (t_ukca_radaer_presc), INTENT(IN OUT) :: rad

IF (ALLOCATED(rad%extinction)) DEALLOCATE(rad%extinction)
IF (ALLOCATED(rad%absorption)) DEALLOCATE(rad%absorption)

RETURN

END SUBROUTINE deallocate_ukca_radaer_presc

END MODULE def_ukca_radaer_presc
