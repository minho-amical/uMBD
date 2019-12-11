MODULE Mympi
IMPLICIT NONE
INCLUDE 'mpif.h'
 INTEGER :: myid
 INTEGER :: mpiierror
 INTEGER :: n_tasks
 INTEGER, DIMENSION(:), ALLOCATABLE :: task_list
CONTAINS
 SUBROUTINE start_mpi()
  CALL MPI_INIT (mpiierror)
  CALL MPI_COMM_RANK (MPI_COMM_WORLD, myid,mpiierror)
  CALL MPI_COMM_SIZE (MPI_COMM_WORLD, n_tasks,mpiierror)
 END SUBROUTINE start_mpi

 SUBROUTINE stop_mpi()
  CALL MPI_FINALIZE(mpiierror)
  IF (mpiierror .NE. 0) WRITE(*, *) "ERROR in terminating MPI"
  IF (mpiierror .NE. 0) WRITE(*, *) "Normal Termination"
  STOP
 END SUBROUTINE stop_mpi

 SUBROUTINE allocate_task(n_atoms)
  INTEGER, INTENT(IN) :: n_atoms
  INTEGER :: i_row

  IF (.NOT.ALLOCATED(task_list)) ALLOCATE(task_list(1:n_atoms))
  DO i_row = 1, n_atoms
   task_list(i_row) = MOD(i_row,n_tasks)
  END DO
 END SUBROUTINE allocate_task

 SUBROUTINE deallocate_task()
  IF (ALLOCATED(task_list)) DEALLOCATE(task_list)
 END SUBROUTINE deallocate_task

 SUBROUTINE sync_tensors(temp_matrix,num_element)
!      use dimensions
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: num_element
  REAL*8,DIMENSION(1:num_element,1:num_element), INTENT(INOUT) :: temp_matrix
  REAL*8, DIMENSION(1:num_element,1:num_element) :: temp_matrix_mpi
  INTEGER :: mpierr
  CALL MPI_ALLREDUCE(temp_matrix,temp_matrix_mpi,num_element*num_element,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,mpierr)
  temp_matrix = temp_matrix_mpi
 END SUBROUTINE sync_tensors
END MODULE Mympi
