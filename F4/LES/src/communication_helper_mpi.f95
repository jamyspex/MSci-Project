module communication_helper_mpi
use communication_common
use mpi
implicit none

integer(kind=4) :: rank, cartRank, mpi_size, ierror, status(MPI_STATUS_SIZE)
integer :: communicator, cartTopComm
#ifdef MPI_NEW_WV
    integer :: bottomRowComm
#endif

contains

subroutine initialise_mpi()
    logical :: alreadyInitialised
    communicator = MPI_COMM_WORLD
    call MPI_Initialized(alreadyInitialised, ierror)
    call checkMPIError()
    if (.not. alreadyInitialised) then
        call MPI_Init(ierror)
        call checkMPIError()
    end if
    call MPI_COMM_Rank(communicator, rank, ierror)
    call checkMPIError()
    call MPI_COMM_Size(communicator, mpi_size, ierror)
    call checkMPIError()
end subroutine initialise_mpi

subroutine finalise_mpi()
    call MPI_Finalize(ierror)
    call checkMPIError()
end subroutine

subroutine checkMPIError()
    integer :: abortError
    if (ierror .ne. MPI_SUCCESS) then
        print*, ierror, " MPI error!"
        call MPI_Abort(communicator, ierror, abortError)
    end if
end subroutine checkMPIError

subroutine setupCartesianVirtualTopology(dimensions, dimensionSizes, periodicDimensions, coordinates, neighbours, reorder)
    integer, intent(in) :: dimensions
    integer, intent(in) :: dimensionSizes(dimensions)
    logical, intent(in) :: periodicDimensions(dimensions)
    integer, intent(out) :: coordinates(dimensions)
    integer, intent(out) :: neighbours(2*dimensions)
    logical, intent(in) :: reorder
    call MPI_Cart_Create(communicator, dimensions, dimensionSizes, &
                         periodicDimensions, reorder, cartTopComm, ierror)
    call checkMPIError()
    call MPI_Comm_Rank(cartTopComm, cartRank, ierror)
    call checkMPIError()
    call MPI_Cart_Coords(cartTopComm, cartRank, dimensions, coordinates, ierror)
    call checkMPIError()
    call MPI_Cart_Shift(cartTopComm, 0, 1, neighbours(topNeighbour), neighbours(bottomNeighbour), ierror)
    call checkMPIError()
    call MPI_Cart_Shift(cartTopComm, 1, 1, neighbours(leftNeighbour), neighbours(rightNeighbour), ierror)
end subroutine setupCartesianVirtualTopology

#ifdef MPI_NEW_WV
subroutine createBottomRowCommunicator(procPerRow)
    integer, intent(In) :: procPerRow
    integer :: row_comm
!print *,'Create a new communicator per row', rank
    ! Create a new communicator per row
    ! WV: should this be cartTopComm?
    call MPI_Comm_split(communicator, (rank / procPerRow), rank, row_comm,ierror)
    call checkMPIError()
    ! If a process is in the bottom row, use this communicator
    if (rank >= mpi_size - procPerRow) then  !WV: this is the bottom row
        bottomRowComm = row_comm
    else
        call MPI_Comm_free(row_comm,ierror)
        call checkMPIError()
    end if
end subroutine createBottomRowCommunicator
#endif

logical function isMaster()
    isMaster = rank .eq. 0
end function isMaster

logical function isTopRow(procPerRow)
    integer, intent(in) :: procPerRow
    isTopRow = rank .lt. procPerRow
end function isTopRow

logical function isTopRowNeighbours(neighbours)
    integer, dimension(:), intent(in) :: neighbours
    isTopRowNeighbours = neighbours(topNeighbour) .eq. -1
end function isTopRowNeighbours

logical function isBottomRow(procPerRow)
    integer, intent(in) :: procPerRow
    isBottomRow = rank .gt. (mpi_size - procPerRow - 1)
end function isBottomRow

logical function isBottomRowNeighbours(neighbours)
    integer, dimension(:), intent(in) :: neighbours
    isBottomRowNeighbours = neighbours(bottomNeighbour) .eq. -1
end function isBottomRowNeighbours

logical function isLeftmostColumn(procPerRow)
    integer, intent(in) :: procPerRow
    isLeftmostColumn = modulo(rank, procPerRow) .eq. 0
end function isLeftmostColumn

logical function isLeftmostColumnNeighbours(neighbours)
    integer, dimension(:), intent(in) :: neighbours
    isLeftmostColumnNeighbours = neighbours(leftNeighbour) .eq. -1
end function isLeftmostColumnNeighbours

logical function isRightmostColumn(procPerRow)
    integer, intent(in) :: procPerRow
    isRightmostColumn = modulo(rank, procPerRow) .eq. (procPerRow - 1)
end function isRightmostColumn

logical function isRightmostColumnNeighbours(neighbours)
    integer, dimension(:), intent(in) :: neighbours
    isRightmostColumnNeighbours = neighbours(rightNeighbour) .eq. -1
end function isRightmostColumnNeighbours

integer function topLeftRowValue(process, procPerRow, rowCount)
    integer, intent(in) :: process, procPerRow, rowCount
    topLeftRowValue = process / procPerRow * rowCount
end function topLeftRowValue

integer function topLeftColValue(process, procPerRow, colCount)
    integer, intent(in) :: process, procPerRow, colCount
    topLeftColValue = modulo(process, procPerRow) * colCount
end function topLeftColValue

end module
