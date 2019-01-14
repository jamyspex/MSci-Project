module communication_helper_real
#ifdef MPI
use communication_helper_mpi
#endif

contains

subroutine getGlobalSumOf(value)
    real(kind=4), intent(inout) :: value
#ifdef GR_DEBUG
    print*, 'Rank: ', rank, ' before sum: ', value
#endif
    call MPI_AllReduce(MPI_IN_PLACE, value, 1, MPI_REAL, MPI_SUM, communicator, ierror)
#ifdef GR_DEBUG
    print*, 'Rank: ', rank, ' after sum: ', value
#endif
#ifdef MPI
    call checkMPIError()
#endif
end subroutine getGlobalSumOf

subroutine getGlobalMaxOf(value)
    real(kind=4), intent(inout) :: value
#ifdef GR_DEBUG
    print*, 'Rank: ', rank, ' before max: ', value
#endif
    call MPI_AllReduce(MPI_IN_PLACE, value, 1, MPI_REAL, MPI_MAX, communicator, ierror)
#ifdef GR_DEBUG
    print*, 'Rank: ', rank, ' after max: ', value
#endif
#ifdef MPI
    call checkMPIError()
#endif
end subroutine getGlobalMaxOf

subroutine getGlobalMinOf(value)
    real(kind=4), intent(inout) :: value
#ifdef GR_DEBUG
    print*, 'Rank: ', rank, ' before min: ', value
#endif
    call MPI_AllReduce(MPI_IN_PLACE, value, 1, MPI_REAL, MPI_MIN, communicator, ierror)
#ifdef GR_DEBUG
    print*, 'Rank: ', rank, ' after min: ', value
#endif
#ifdef MPI
    call checkMPIError()
#endif
end subroutine getGlobalMinOf

subroutine exchangeRealHalos(array, procPerRow, neighbours, leftThickness, &
                                rightThickness, topThickness, &
                                bottomThickness)
    real(kind=4), dimension(:,:,:), intent(inout) :: array
    integer, dimension(:), intent(in) :: neighbours
    integer, intent(in) :: procPerRow, leftThickness, rightThickness, topThickness, bottomThickness
    integer :: i, commWith, r, c, d, rowCount, colCount, depthSize
#ifdef MPI
    integer :: requests(8)
#endif
    real(kind=4), dimension(:,:,:), allocatable :: leftRecv, leftSend, rightSend, rightRecv
    real(kind=4), dimension(:,:,:), allocatable :: topRecv, topSend, bottomSend, bottomRecv
#ifdef MPI
    if (size(neighbours, 1) .lt. 4) then
        print*, "Error: cannot have a 4-way halo exchange with less than 4 neighbours"
        call finalise_mpi()
        return
    end if
#endif
    rowCount = size(array, 1) - topThickness - bottomThickness
    colCount = size(array, 2) - leftThickness - rightThickness
    depthSize = size(array, 3)
    allocate(leftRecv(rowCount, rightThickness, depthSize)) 
    allocate(rightSend(rowCount, leftThickness, depthSize)) 
    allocate(rightRecv(rowCount, leftThickness, depthSize)) 
    allocate(leftSend(rowCount, rightThickness, depthSize)) 
    allocate(topRecv(bottomThickness, colCount, depthSize)) 
    allocate(bottomSend(topThickness, colCount, depthSize)) 
    allocate(bottomRecv(topThickness, colCount, depthSize)) 
    allocate(topSend(bottomThickness, colCount, depthSize)) 
#ifdef MPI
    do i=1,8
        requests(i)= MPI_REQUEST_NULL
    end do
#endif
    ! Top edge to send, bottom edge to receive. 
    commWith = neighbours(topNeighbour)
    if (commWith .ne. -1) then
        !print*, 'rank ', rank, ' communicating with top neighbour ', commWith
        do r=1, bottomThickness
            do c=1, colCount
                do d=1, depthSize
                    topSend(r, c, d) = array(r + topThickness, c+leftThickness, d) 
                end do
            end do
        end do
        call MPI_ISend(topSend, bottomThickness*colCount*depthSize, MPI_REAL, commWith, topTag, &
                      cartTopComm, requests(1), ierror) 
        call checkMPIError()
        call MPI_IRecv(bottomRecv, topThickness*colCount*depthSize, MPI_REAL, commWith, bottomTag, &
                      communicator, requests(2), ierror)  
        call checkMPIError()
    end if
    ! Bottom edge to send, top edge to receive
    commWith = neighbours(bottomNeighbour)
    if (commWith .ne. -1) then
        !print*, 'rank ', rank, ' communicating with bottom neighbour ', commWith
        do r=1, topThickness 
            do c=1, colCount
                do d=1, depthSize
                    bottomSend(r, c, d) = array(size(array, 1) - bottomThickness - topThickness + r, &
                                          c+leftThickness, &
                                          d) 

                end do
            end do
        end do
        call MPI_IRecv(topRecv, bottomThickness*colCount*depthSize, MPI_REAL, commWith, topTag, &
                      cartTopComm, requests(3), ierror) 
        call checkMPIError()
        call MPI_ISend(bottomSend, topThickness*colCount*depthSize, MPI_REAL, commWith, bottomTag, &
                      communicator, requests(4), ierror) 
        call checkMPIError()
    end if
    ! Left edge to send, right edge to receive
    commWith = neighbours(leftNeighbour)
    if (commWith .ne. -1) then
        !print*, 'rank ', rank, ' communicating with left neighbour ', commWith
        do r=1, rowCount
            do c=1, rightThickness
                do d=1, depthSize
                leftSend(r, c, d) = array(r+topThickness, c + leftThickness, d) 
                end do
            end do
        end do
        call MPI_ISend(leftSend, rightThickness*rowCount*depthSize, MPI_REAL, commWith, leftTag, &
                      communicator, requests(5), ierror)
        call checkMPIError()
        call MPI_IRecv(rightRecv, leftThickness*rowCount*depthSize, MPI_REAL, commWith, rightTag, &
                      communicator, requests(6), ierror)
        call checkMPIError()
    end if
    ! Right edge to send, left edge to receive
    commWith = neighbours(rightNeighbour)
    if (commWith .ne. -1) then
        !print*, 'rank ', rank, ' communicating with right neighbour ', commWith
        do r=1, rowCount
            do c=1, leftThickness
                do d=1, depthSize
                    rightSend(r, c, d) = array(r+topThickness, &
                                               size(array, 2) - rightThickness - leftThickness + c,&
                                               d) 
                end do
            end do
        end do
        call MPI_IRecv(leftRecv, rightThickness*rowCount*depthSize, MPI_REAL, commWith, leftTag, &
                      communicator, requests(7), ierror)
        call checkMPIError()
        call MPI_ISend(rightSend, leftThickness*rowCount*depthSize, MPI_REAL, commWith, rightTag, &
                      communicator, requests(8), ierror)
        call checkMPIError()
    end if
    do i=1,8
        if (requests(i) .ne. MPI_REQUEST_NULL) then
            call MPI_Wait(requests(i), status, ierror)
            call checkMPIError()
        end if
    end do
    if (.not. isTopRow(procPerRow)) then
        do r=1, topThickness
            do c=1, colCount
                do d=1, depthSize
                    array(r, c+leftThickness, d) = bottomRecv(r, c, d) 
                end do
            end do
        end do
    end if
    if (.not. isBottomRow(procPerRow)) then
        do r=1, bottomThickness
            do c=1, colCount
                do d=1, depthSize
                    array(size(array, 1) - bottomThickness + r, c+leftThickness, d) = topRecv(r, c, d) 
                end do
            end do
        end do
    end if
    if (.not. isLeftmostColumn(procPerRow)) then
        do r=1, rowCount
            do c=1, leftThickness
                do d=1, depthSize
                    array(r+topThickness, c, d) = rightRecv(r, c, d) ! OK
                end do
            end do
        end do
    end if
    if (.not. isRightmostColumn(procPerRow)) then
        do r=1, rowCount
            do c=1, rightThickness
                do d=1, depthSize
                    array(r+topThickness, size(array, 2) - rightThickness + c, d) = leftRecv(r, c, d) 
                end do
            end do
        end do
    end if
    call exchangeRealCorners(array, procPerRow, leftThickness, rightThickness, topThickness, bottomThickness)
    deallocate(leftRecv)
    deallocate(leftSend)
    deallocate(rightSend)
    deallocate(rightRecv)
    deallocate(topRecv)
    deallocate(topSend)
    deallocate(bottomSend)
    deallocate(bottomRecv)
end subroutine exchangeRealHalos

subroutine exchangeRealCorners(array, procPerRow, leftThickness, rightThickness, topThickness, bottomThickness)
    integer, intent(in) :: procPerRow, leftThickness, rightThickness, topThickness, bottomThickness
    real(kind=4), dimension(:,:,:), intent(inout) :: array
    real(kind=4), dimension(:,:,:), allocatable :: topLeftRecv, topRightRecv, bottomLeftRecv, bottomRightRecv
    real(kind=4), dimension(:,:,:), allocatable :: topLeftSend, topRightSend, bottomLeftSend, bottomRightSend
    integer :: depthSize, commWith, r, c, d
#ifdef MPI
    integer :: i, requests(8)
#endif
    depthSize = size(array, 3)
    allocate(topLeftRecv(bottomThickness, rightThickness, depthSize))
    allocate(topLeftSend(bottomThickness, rightThickness, depthSize))
    allocate(topRightRecv(bottomThickness, leftThickness, depthSize))
    allocate(topRightSend(bottomThickness, leftThickness, depthSize))
    allocate(bottomLeftRecv(topThickness, rightThickness, depthSize))
    allocate(bottomLeftSend(topThickness, rightThickness, depthSize))
    allocate(bottomRightRecv(topThickness, leftThickness, depthSize))
    allocate(bottomRightSend(topThickness, leftThickness, depthSize))
#ifdef MPI
    do i=1,8
        requests(i) = MPI_REQUEST_NULL
    end do
#endif
    if (.not. isTopRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        commWith = rank - procPerRow - 1
        !print*, 'Rank ', rank, ' has a top left neighbour with rank ', commWith
        do r=1,bottomThickness
            do c=1,rightThickness
                do d=1,depthSize
                    topLeftSend(r, c, d) = array(r + topThickness, c + leftThickness, d)
                end do
            end do
        end do
#ifdef MPI
        call MPI_ISend(topLeftSend, bottomThickness*rightThickness*depthSize, MPI_REAL, &
                       commWith, topLeftTag, communicator, requests(1), ierror)
        call checkMPIError()
        call MPI_IRecv(bottomRightRecv, topThickness*leftThickness*depthSize, &
                       MPI_Real, commWith, bottomRightTag, communicator, requests(2), ierror)
        call checkMPIError()
#endif
    end if
    if (.not. isTopRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        commWith = rank - procPerRow + 1
        !print*, 'Rank ', rank, ' has a top right neighbour with rank ', commWith
        do r=1, bottomThickness
            do c=1, leftThickness
                do d=1, depthSize
                    topRightSend(r, c, d) = array(r + topThickness, size(array, 2) - leftThickness - rightThickness + c, d)
                end do
            end do
        end do
#ifdef MPI
        call MPI_ISend(topRightSend, bottomThickness*leftThickness*depthSize, MPI_REAL, &
                       commWith, topRightTag, communicator, requests(3), ierror)
        call checkMPIError()
        call MPI_IRecv(bottomLeftRecv, topThickness*rightThickness*depthSize, MPI_REAL, &
                       commWith, bottomLeftTag, communicator, requests(4), ierror)
        call checkMPIError()
#endif
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        commWith = rank + procPerRow - 1
        !print*, 'Rank ', rank, ' has a bottom left neighbour with rank ', commWith
        do r=1, topThickness
            do c=1, rightThickness
                do d=1, depthSize
                    bottomLeftSend(r, c, d) = array(size(array, 1) - topThickness - bottomThickness + r, &
                                                    c + leftThickness, d)
                end do
            end do
        end do
#ifdef MPI
        call MPI_ISend(bottomLeftSend, topThickness*rightThickness*depthSize, MPI_REAL, &
                      commWith, bottomLeftTag, communicator, requests(5), ierror)
        call checkMPIError()
        call MPI_IRecv(topRightRecv, bottomThickness*leftThickness*depthSize, MPI_REAL, &
                       commWith, topRightTag, communicator, requests(6), ierror)
        call checkMPIError()
#endif
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        commWith = rank + procPerRow + 1
        !print*, 'Rank ', rank, ' has a bottom right neighbour with rank ', commWith
        do r=1,topThickness
            do c=1,leftThickness
                do d=1,depthSize
                    bottomRightSend(r, c, d) = array(size(array, 1) - topThickness - bottomThickness + r, &
                                                     size(array, 2) - leftThickness - rightThickness + c,d)
                end do
            end do
        end do
#ifdef MPI
        call MPI_ISend(bottomRightSend, topThickness*leftThickness*depthSize, MPI_REAL, &
                       commWith, bottomRightTag, communicator, requests(7), ierror)
        call checkMPIError()
        call MPI_IRecv(topLeftRecv, bottomThickness*rightThickness*depthSize, &
                       MPI_Real, commWith, topLeftTag, communicator, requests(8), ierror)
        call checkMPIError()
#endif
    end if
#ifdef MPI
    do i=1,8
        if (requests(i) .ne. MPI_REQUEST_NULL) then
            call MPI_Wait(requests(i), status, ierror)
            call checkMPIError()
        end if
    end do
#endif
    if (.not. isTopRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        do r=1,topThickness
            do c=1,leftThickness
                do d=1,depthSize
                    array(r, c, d) = bottomRightRecv(r, c, d)
                end do
            end do
        end do
    end if
    if (.not. isTopRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        do r=1,topThickness
            do c=1,rightThickness
                do d=1,depthSize
                    array(r, size(array, 2) - rightThickness + c, d) = bottomLeftRecv(r, c, d)
                end do
            end do
        end do
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        do r=1,bottomThickness
            do c=1, leftThickness
                do d=1, rightThickness
                    array(size(array, 1) - bottomThickness + r, c, d) = topRightRecv(r, c, d)
                end do
            end do
        end do
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        do r=1, bottomThickness
            do c=1, rightThickness
                do d=1, depthSize
                    array(size(array, 1) - bottomThickness + r, size(array, 2) - rightThickness + c, d) = topLeftRecv(r, c, d)
                end do
            end do
        end do
    end if
    deallocate(topLeftRecv)
    deallocate(topLeftSend)
    deallocate(topRightRecv)
    deallocate(topRightSend)
    deallocate(bottomLeftRecv)
    deallocate(bottomLeftSend)
    deallocate(bottomRightRecv)
    deallocate(bottomRightSend)
end subroutine exchangeRealCorners

subroutine sideflowRightLeft(array, procPerRow, colToSend, colToRecv, &
                             topThickness, bottomThickness, ignoreFirstK, ignoreLastK)
    integer, intent(in) :: procPerRow, colToSend, colToRecv, topThickness, bottomThickness
    real(kind=4), dimension(:,:,:), intent(inout) :: array
    real(kind=4), dimension(:,:), allocatable :: leftRecv, rightSend
    integer :: r, d, commWith, rowCount, depthSize, ignoreFirstK, ignoreLastK
    rowCount = size(array, 1) - topThickness - bottomThickness
    depthSize = size(array, 3) - ignoreFirstK - ignoreLastK
    if (isLeftmostColumn(procPerRow)) then
        allocate(leftRecv(rowCount, depthSize))
        commWith = rank + procPerRow - 1
        call MPI_Recv(leftRecv, rowCount*depthSize, MPI_REAL, commWith, rightSideTag, &
                      communicator, status, ierror)
        call checkMPIError()
        do r=1, rowCount
            do d=1+ignoreFirstK, size(array,3) - ignoreLastK
                array(r+topThickness, colToRecv, d) = leftRecv(r, d-ignoreFirstK)
            end do
        end do
        deallocate(leftRecv)
    else if (isRightmostColumn(procPerRow)) then
        allocate(rightSend(rowCount, depthSize))
        commWith = rank - procPerRow + 1
        do r=1, rowCount
            do d=1+ignoreFirstK, size(array,3) - ignoreLastK
                rightSend(r, d-ignoreFirstK) = array(r+topThickness, colToSend, d)
            end do
        end do
        call MPI_Send(rightSend, rowCount*depthSize, MPI_REAL, commWith, rightSideTag, &
                      communicator, ierror)
        call checkMPIError()
        deallocate(rightSend)
    end if
#ifdef GR_DEBUG
    !print*, 'GR: rank ', rank, ' has finished sideflowRightLeft'
#endif
end subroutine sideflowRightLeft

subroutine sideflowLeftRight(array, procPerRow, colToSend, colToRecv, &
                             topThickness, bottomThickness, ignoreFirstK, ignoreLastK)
    integer, intent(in) :: procPerRow, colToSend, colToRecv, topThickness, bottomThickness
    real(kind=4), dimension(:,:,:), intent(inout) :: array
    real(kind=4), dimension(:,:), allocatable :: leftSend, rightRecv
    integer :: r, d, commWith, rowCount, depthSize, ignoreFirstK, ignoreLastK
    rowCount = size(array, 1) - topThickness - bottomThickness
    depthSize = size(array, 3) - ignoreFirstK - ignoreLastK
    if (isLeftmostColumn(procPerRow)) then
        allocate(leftSend(rowCount, depthSize))
        commWith = rank + procPerRow - 1
        do r=1, rowCount
            do d=1+ignoreFirstK, size(array,3) - ignoreLastK
                leftSend(r, d-ignoreFirstK) = array(r+topThickness, colToSend, d)
            end do
        end do
        call MPI_Send(leftSend, rowCount*depthSize, MPI_REAL, commWith, leftSideTag, &
                      communicator, ierror)
        call checkMPIError()
        deallocate(leftSend)
    else if (isRightmostColumn(procPerRow)) then
        allocate(rightRecv(rowCount, depthSize))
        commWith = rank - procPerRow + 1
        call MPI_Recv(rightRecv, rowCount*depthSize, MPI_REAL, commWith, leftSideTag, &
                      communicator, status, ierror)
        call checkMPIError()
        do r=1, rowCount
            do d=1+ignoreFirstK, size(array,3) - ignoreLastK
                array(r+topThickness, colToRecv, d) = rightRecv(r, d-ignoreFirstK)
            end do
        end do
        deallocate(rightRecv)
    end if
end subroutine sideflowLeftRight

subroutine distributeZBM(zbm, ip, jp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) , intent(InOut) :: zbm
    integer :: startRow, startCol, i, r, c
    real(kind=4), dimension(ip, jp) :: sendBuffer, recvBuffer
    if (isMaster()) then
        ! Send appropriate 2D section to the other ranks
        do i = 1, mpi_size - 1
            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
#ifdef GR_DEBUG
            print*, 'GR: zbm Sending (', (startRow+1), '-', (startRow+ip), ',', &
                     (startCol+1), '-', (startCol+jp), ') to rank ', i
#endif
            do r=1, ip
                do c=1, jp
                    sendBuffer(r, c) = zbm(startRow + r, startCol + c)
                end do
            end do
            print*, 'GR: sendBuffer zbm sum: ', sum(sendBuffer)
            call MPI_Send(sendBuffer, (ip*jp), MPI_REAL, i, zbmTag, &
                          communicator, ierror)
            call checkMPIError()
        end do
    else
        ! Receive appropriate 2D section from master
        call MPI_Recv(recvBuffer, (ip*jp), MPI_REAL, 0, zbmTag, communicator, &
                      status, ierror)
        call checkMPIError()
        print*, 'GR: recvBuffer zbm sum: ', sum(recvBuffer)
        do r=1, ip
            do c=1, jp
                zbm(r, c) = recvBuffer(r, c)
            end do
        end do
    end if
end subroutine distributeZBM

subroutine distribute1DRealRowWiseArray(arrayToBeSent, receivingArray, leftBoundary, rightBoundary, procPerRow)
    real(kind=4), dimension(:), intent(in) :: arrayToBeSent
    real(kind=4), dimension(:), intent(out) :: receivingArray
    real(kind=4), dimension(:), allocatable :: sendBuffer
    integer, intent(in) :: leftBoundary, rightBoundary, procPerRow
    integer :: totalSize, receivingSize, i, startI, endI, currentI
    totalSize = size(arrayToBeSent, 1)
    receivingSize = size(receivingArray, 1)
    if (isMaster()) then
        allocate(sendBuffer(receivingSize))
#ifdef VERBOSE
        print*, ' Rank ', rank, ' needs to row wise send ', receivingSize, &
                ' values out of ', totalSize, ' values to each process with a ', &
                ' left boundary of ', leftBoundary, ' and a right boundary of ', &
                rightBoundary
#endif
        ! Master needs to memory copy its required portion
        receivingArray = arrayToBeSent(1:receivingSize)
        do i=1, mpi_size-1
            ! MPI_Send
            startI = 1 + ((i / procPerRow) * (receivingSize - leftBoundary - rightBoundary))
            endI = startI + receivingSize - 1
#ifdef VERBOSE
            print*, ' Rank ', i, ' is getting values row wise, (', startI, ',', endI, ')'
#endif
            do currentI=startI,endI
                sendBuffer(currentI-startI+1) = arrayToBeSent(currentI)
            end do
            call MPI_Send(sendBuffer, receivingSize, MPI_Real, i, dxTag, communicator, &
                          ierror)
            call checkMPIError()
        end do
        deallocate(sendBuffer)
    else
        ! Receive receivingSize reals
        call MPI_Recv(receivingArray, receivingSize, MPI_REAL, 0, dxTag, communicator, &
                      status, ierror)
        call checkMPIError()
    end if
    print*, 'GR: rank ', rank, ' row wise sum ', sum(receivingArray)
end subroutine distribute1DRealRowWiseArray

subroutine distribute1DRealColumnWiseArray(arrayToBeSent, receivingArray, leftBoundary, rightBoundary, procPerRow)
    real(kind=4), dimension(:), intent(in) :: arrayToBeSent
    real(kind=4), dimension(:), intent(out) :: receivingArray
    real(kind=4), dimension(:), allocatable :: sendBuffer
    integer, intent(in) :: leftBoundary, rightBoundary, procPerRow
    integer :: totalSize, receivingSize, i, startI, endI, currentI
    totalSize = size(arrayToBeSent, 1)
    receivingSize = size(receivingArray, 1)
    if (isMaster()) then
        allocate(sendBuffer(receivingSize))
#ifdef VERBOSE
        print*, ' Rank ', rank, ' needs to column wise send ', receivingSize, &
                ' values out of ', totalSize, ' values to each process with a ', &
                ' left boundary of ', leftBoundary, ' and a right boundary of ', &
                rightBoundary
#endif
        ! Master needs to memory copy its required portion
        receivingArray = arrayToBeSent(1:receivingSize)
        do i=1, mpi_size-1
            ! MPI_Send
            startI = 1 + (modulo(i, procPerRow) * (receivingSize - leftBoundary - rightBoundary))
            endI = startI + receivingSize - 1
#ifdef VERBOSE
            print*, ' Rank ', i, ' is getting values column wise, (', startI, ',', endI, ')'
#endif
            do currentI=startI,endI
                sendBuffer(currentI-startI+1) = arrayToBeSent(currentI)
            end do
            call MPI_Send(sendBuffer, receivingSize, MPI_Real, i, dyTag, communicator, &
                          ierror)
            call checkMPIError()
        end do
        deallocate(sendBuffer)
    else
        ! Receive receivingSize reals
        call MPI_Recv(receivingArray, receivingSize, MPI_REAL, 0, dyTag, communicator, &
                      status, ierror)
        call checkMPIError()
    end if
end subroutine distribute1DRealColumnWiseArray

subroutine collect3DReal4Array(array, arrayTot, leftBoundary, rightBoundary, &
                               topBoundary, bottomBoundary, ip, jp, kp, procPerRow)
    real(kind=4), dimension(:,:,:), intent(in) :: array
    real(kind=4), dimension(:,:,:), intent(out) :: arrayTot
    integer, intent(in) :: leftBoundary, rightBoundary, topBoundary, bottomBoundary
    integer, intent(in) :: ip, jp, kp, procPerRow
    integer :: i, startRow, startCol, r, c, d, bufferSize
    real(kind=4), dimension(:,:,:), allocatable :: recvBuffer
    bufferSize = size(array, 1) * size(array, 2) * size(array, 3)
    if (isMaster()) then
        allocate(recvBuffer(size(array, 1), size(array, 2), size(array, 3)))
        do r=1, size(array, 1)
            do c=1, size(array, 2)
                do d=1, size(array, 3)
                    arrayTot(r, c, d) = array(r, c, d)
                end do
            end do
        end do
        do i=1, mpi_size-1
            startRow = (ip) * (i / procPerRow)
            startCol = (jp) * (modulo(i, procPerRow))
#ifdef MPI
            call MPI_Recv(recvBuffer, bufferSize, MPI_Real, i, collect3DReal4Tag, &
                          communicator, status, ierror)
            call checkMPIError()
#endif
            do r=1, size(recvBuffer, 1)
                do c=1, size(recvBuffer, 2)
                    do d=1, size(recvBuffer, 3)
                        arrayTot(r + startRow, c + startCol, d) = recvBuffer(r, c, d)
                    end do
                end do
            end do
        end do
        deallocate(recvBuffer)
    else
#ifdef MPI
        call MPI_Send(array, bufferSize, MPI_Real, 0, collect3DReal4Tag, &
                      communicator, ierror)
        call checkMPIError()
#endif
    end if
end subroutine collect3DReal4Array




subroutine distributeamask(ua, u,ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow
    real(kind=4), dimension(0:ipmax+1,0:jpmax+1,0:kp+1) , intent(InOut) :: ua
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(In) :: u
    integer :: startRow, startCol, i, r, c, k, rank,j
    character(70) :: cha
    real(kind=4), dimension(ip, jp, kp) :: sendBuffer, recvBuffer

    if (.not.isMaster()) then

            do k=1, kp
                do c=1, jp
                   do r=1, ip
                    sendBuffer(r, c, k) = u(r, c, k)
                   end do
                end do
            end do
            print*, 'GR: sendBuffer  sum: ', sum(sendBuffer)
         

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'send_rank=',rank


            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, &
                          communicator, ierror)
            call checkMPIError()


    else
  
        do i = 1, mpi_size - 1

        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, i, zbmTag,communicator,&
                      status, ierror)
        call checkMPIError()

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()

        print*, 'GR: recvBuffer  sum: ', sum(recvBuffer)

            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
        write(*,*) 'startRow=',startRow,'startCol=',startCol

        do k=1, kp
             do c=1, jp
               do r=1, ip
                ua(startRow +r, startCol + c, k) = recvBuffer(r, c, k)
               end do
            end do
        end do
   
       
        end do
       end if
    
            

end subroutine distributeamask


subroutine distributeu(ua, u,ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow
    real(kind=4), dimension(0:ipmax+1,-1:jpmax+1,0:kp+1) , intent(InOut) :: ua
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
    integer :: startRow, startCol, i, r, c, k, rank,j
    character(70) :: cha
    real(kind=4), dimension(ip, jp, kp) :: sendBuffer,recvBuffer
    if (.not.isMaster()) then

            do k=1, kp
                do c=1, jp
                   do r=1, ip
                    sendBuffer(r, c, k) = u(r, c, k)
                   end do
                end do
            end do
            print*, 'GR: sendBuffer  sum: ', sum(sendBuffer)


!       call MPI_COMM_Rank(communicator, rank, ierror)
!      call checkMPIError()



            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, &
                          communicator, ierror)
            call checkMPIError()


    else
  
        do i = 1, mpi_size - 1

        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, i,zbmTag,communicator,&
                      status, ierror)
        call checkMPIError()

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()

        print*, 'GR: recvBuffer  sum: ', sum(recvBuffer)


            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
        write(*,*) 'startRow=',startRow,'startCol=',startCol


        do k=1, kp
             do c=1, jp
               do r=1, ip
                ua(startRow +r, startCol + c, k) = recvBuffer(r, c, k)
               end do
            end do
        end do


        end do
       end if
    
            

end subroutine distributeu



subroutine distributev(ua, u,ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow
    real(kind=4), dimension(0:ipmax+1,-1:jpmax+1,0:kp+1) , intent(InOut) :: ua
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
    integer :: startRow, startCol, i, r, c, k, rank,j
    character(70) :: cha
    real(kind=4), dimension(ip, jp, kp) :: sendBuffer, recvBuffer
    if (.not.isMaster()) then

            do k=1, kp
                do c=1, jp
                   do r=1, ip
                    sendBuffer(r, c, k) = u(r, c, k)
                   end do
                end do
            end do
            print*, 'GR: sendBuffer  sum: ', sum(sendBuffer)


!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'send_rank=',rank


            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, &
                          communicator, ierror)
            call checkMPIError()


    else
  
        do i = 1, mpi_size - 1

        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, i,zbmTag,communicator,&
                      status, ierror)
        call checkMPIError()

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()

        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)


            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
        write(*,*) 'startRow=',startRow,'startCol=',startCol

        do k=1, kp
             do c=1, jp
               do r=1, ip
                ua(startRow +r, startCol + c, k) = recvBuffer(r, c, k)
               end do
            end do
        end do
   

        end do
       end if
    
            

end subroutine distributev



subroutine distributew(ua, u,ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow
    real(kind=4), dimension(0:ipmax+1,-1:jpmax+1,-1:kp+1) , intent(InOut) :: ua
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: u
    integer :: startRow, startCol, i, r, c, k, rank,j
    character(70) :: cha
    real(kind=4), dimension(ip,jp, kp) :: sendBuffer, recvBuffer
    if (.not.isMaster()) then

            do k=1, kp
                do c=1, jp
                   do r=1, ip
                    sendBuffer(r, c, k) = u(r, c, k)
                   end do
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)
         

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'send_rank=',rank


            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, &
                          communicator, ierror)
            call checkMPIError()


    else
  
        do i = 1, mpi_size - 1

        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, i,zbmTag,communicator,&
                      status, ierror)
        call checkMPIError()

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)


            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
        write(*,*) 'startRow=',startRow,'startCol=',startCol

        do k=1, kp
             do c=1, jp
               do r=1, ip
                ua(startRow +r, startCol + c, k) = recvBuffer(r, c, k)
               end do
            end do
        end do
   
       
        end do
       end if
    
            

end subroutine distributew


subroutine distributebondu(u, ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow, kp
    real(kind=4), dimension(1,-1:jpmax+1,0:kp+1) , intent(InOut) :: u
    integer :: startRow, startCol, i, r, c, k
    real(kind=4), dimension(1, jp, kp) :: sendBuffer, recvBuffer

    if (isMaster()) then
        ! Send appropriate 2D section to the other ranks
        do i = 1, procPerRow - 1
            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)

            do k=1, kp
                do c=1, jp
                    sendBuffer(1, c, k) = u(1, startCol + c, k)
                end do
            end do
            print*, 'GR: sendBuffer  sum: ', sum(sendBuffer)


            call MPI_Send(sendBuffer, (jp*kp), MPI_REAL, i, zbmTag, &
                          communicator, ierror)
            call checkMPIError()
        end do
    else
        ! Receive appropriate 2D section from master
        call MPI_Recv(recvBuffer, (jp*kp), MPI_REAL, 0, zbmTag, communicator, &
                      status, ierror)
        call checkMPIError()



!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'recv_rank=',rank


        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)
        do k=1, kp
            do c=1, jp
                u(1, c, k) = recvBuffer(1, c, k)
            end do
        end do
    end if
end subroutine distributebondu


subroutine distributebondv(u, ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow, kp
    real(kind=4), dimension(1,-1:jpmax+1,0:kp+1) , intent(InOut) :: u
    integer :: startRow, startCol, i, r, c, k
    real(kind=4), dimension(1, jp, kp) :: sendBuffer, recvBuffer
    if (isMaster()) then
        ! Send appropriate 2D section to the other ranks
        do i = 1, procPerRow - 1
            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)

            do k=1, kp
                do c=1, jp
                    sendBuffer(1, c, k) = u(1, startCol + c, k)
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)
            call MPI_Send(sendBuffer, (jp*kp), MPI_REAL, i, zbmTag, &
                          communicator, ierror)
            call checkMPIError()
        end do


    else
        ! Receive appropriate 2D section from master
        call MPI_Recv(recvBuffer, (jp*kp), MPI_REAL, 0, zbmTag ,communicator, &
                      status, ierror)
        call checkMPIError()
        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)
        do k=1, kp
            do c=1, jp
                u(1, c, k) = recvBuffer(1, c, k)
            end do
        end do
    end if
end subroutine distributebondv






subroutine distributebondw(u, ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow, kp
    real(kind=4), dimension(1,-1:jpmax+1,-1:kp+1) , intent(InOut) :: u
    integer :: startRow, startCol, i, r, c, k
    real(kind=4), dimension(1, jp, kp) :: sendBuffer, recvBuffer

    if (isMaster()) then
        ! Send appropriate 2D section to the other ranks
        do i = 1, procPerRow - 1
            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)

            do k=1, kp
                do c=1, jp
                    sendBuffer(1, c, k) = u(1, startCol + c, k)
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)


            call MPI_Send(sendBuffer, (jp*kp), MPI_REAL, i, zbmTag, &
                          communicator, ierror)
            call checkMPIError()
        end do
    else
        ! Receive appropriate 2D section from master
        call MPI_Recv(recvBuffer, (jp*kp), MPI_REAL, 0, zbmTag, communicator, &
                      status, ierror)
        call checkMPIError()


!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'recv_rank=',rank


        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)
        do k=1, kp
            do c=1, jp
                u(1, c, k) = recvBuffer(1, c, k)
            end do
        end do
    end if
end subroutine distributebondw


subroutine distributeusum(usuma, usum,ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow
    real(kind=4), dimension(0:ipmax,0:jpmax,0:kp) , intent(InOut) :: usuma
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: usum
    integer :: startRow, startCol, i, r, c, k, rank,j
    character(70) :: cha
    real(kind=4), dimension(ip,jp, kp) :: sendBuffer, recvBuffer
    if (.not.isMaster()) then

            do k=1, kp
                do c=1, jp
                   do r=1, ip
                    sendBuffer(r, c, k) = usum(r, c, k)
                   end do
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)
        

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'send_rank=',rank


            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, &
                          communicator, ierror)
            call checkMPIError()


    else
  
        do i = 1, mpi_size - 1

        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, i,zbmTag,communicator,&
                      status, ierror)
        call checkMPIError()

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)


            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
        write(*,*) 'startRow=',startRow,'startCol=',startCol


        do k=1, kp
             do c=1, jp
               do r=1, ip
                usuma(startRow +r, startCol + c, k) = recvBuffer(r, c, k)
               end do
            end do
        end do
   
       
        end do
       end if
    
            

end subroutine distributeusum


subroutine distributep(pa, p,ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow
    real(kind=4), dimension(0:ipmax+2,0:jpmax+2,0:kp+1) , intent(InOut) :: pa
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(In) :: p
    integer :: startRow, startCol, i, r, c, k, rank,j
    character(70) :: cha
    real(kind=4), dimension(ip,jp, kp) :: sendBuffer, recvBuffer
    if (.not.isMaster()) then

            do k=1, kp
                do c=1, jp
                   do r=1, ip
                    sendBuffer(r, c, k) = p(r, c, k)
                   end do
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)
         

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'send_rank=',rank


            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, &
                          communicator, ierror)
            call checkMPIError()


    else
  
        do i = 1, mpi_size - 1

        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, i,zbmTag,communicator,&
                      status, ierror)
        call checkMPIError()

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)

            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
        write(*,*) 'startRow=',startRow,'startCol=',startCol

        do k=1, kp
             do c=1, jp
               do r=1, ip
                pa(startRow +r, startCol + c, k) = recvBuffer(r, c, k)
               end do
            end do
        end do
   
       
        end do
       end if
             

end subroutine distributep


subroutine distributef(fa, f,ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow
    real(kind=4), dimension(0:ipmax,0:jpmax,0:kp) , intent(InOut) :: fa
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In) :: f
    integer :: startRow, startCol, i, r, c, k, rank,j
    character(70) :: cha
    real(kind=4), dimension(ip,jp, kp) :: sendBuffer, recvBuffer
    if (.not.isMaster()) then

            do k=1, kp
                do c=1, jp
                   do r=1, ip
                    sendBuffer(r, c, k) = f(r, c, k)
                   end do
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)
         


!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'send_rank=',rank


            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, &
                          communicator, ierror)
            call checkMPIError()


    else
  
        do i = 1, mpi_size - 1

        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, i,zbmTag,communicator,&
                      status, ierror)
        call checkMPIError()

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)


            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
        write(*,*) 'startRow=',startRow,'startCol=',startCol

        do k=1, kp
             do c=1, jp
               do r=1, ip
                fa(startRow +r, startCol + c, k) = recvBuffer(r, c, k)
               end do
            end do
        end do
   
       
        end do
       end if
    
            

end subroutine distributef


subroutine distributefold(folda, fold,ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow
    real(kind=4), dimension(ipmax,jpmax,kp) , intent(InOut) :: folda
    real(kind=4), dimension(ip,jp,kp) , intent(In) :: fold
    integer :: startRow, startCol, i, r, c, k, rank,j
    character(70) :: cha
    real(kind=4), dimension(ip,jp, kp) :: sendBuffer, recvBuffer
    if (.not.isMaster()) then

            do k=1, kp
                do c=1, jp
                   do r=1, ip
                    sendBuffer(r, c, k) = fold(r, c, k)
                   end do
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)

         

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'send_rank=',rank


            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, &
                          communicator, ierror)
            call checkMPIError()


    else
  
        do i = 1, mpi_size - 1

        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, i,zbmTag,communicator,&
                      status, ierror)
        call checkMPIError()

        call MPI_COMM_Rank(communicator, rank, ierror)
        call checkMPIError()

        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)


            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
        write(*,*) 'startRow=',startRow,'startCol=',startCol

        do k=1, kp
             do c=1, jp
               do r=1, ip
                folda(startRow +r, startCol + c, k) = recvBuffer(r, c, k)
               end do
            end do
        end do
   
       
        end do
       end if
    
            

end subroutine distributefold


subroutine distributeifu(ua, ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow, kp
    real(kind=4), dimension(0:ipmax+1,-1:jpmax+1,0:kp+1), intent(InOut) :: ua
    integer :: startRow, startCol, i, r, c, k
    real(kind=4), dimension(ip, jp, kp) :: sendBuffer, recvBuffer

    if (isMaster()) then
        ! Send appropriate 2D section to the other ranks
        do i = 1, mpi_size - 1
            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)

            do k=1, kp
                do c=1, jp
                 do r=1,ip
                    sendBuffer(r, c, k) = ua(startRow + r, startCol + c, k)
                  end do
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)

            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, i, zbmTag, &
                          communicator, ierror)
            call checkMPIError()
        end do
    else
        ! Receive appropriate 2D section from master
        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, communicator, &
                      status, ierror)
        call checkMPIError()



!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'recv_rank=',rank


        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)
        do k=1, kp
            do c=1, jp
             do r=1, ip
                ua(r, c, k) = recvBuffer(r, c, k)
             end do
            end do
        end do
    end if
end subroutine distributeifu


subroutine distributeifw(wa, ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow, kp
    real(kind=4), dimension(0:ipmax+1,-1:jpmax+1,-1:kp+1), intent(InOut) :: wa
    integer :: startRow, startCol, i, r, c, k
    real(kind=4), dimension(ip, jp, kp) :: sendBuffer, recvBuffer

    if (isMaster()) then
        ! Send appropriate 2D section to the other ranks
        do i = 1, mpi_size - 1
            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)

            do k=1, kp
                do c=1, jp
                 do r=1, ip
                    sendBuffer(r, c, k) = wa(startRow + r, startCol + c, k)
                 end do
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)


            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, i, zbmTag, &
                          communicator, ierror)
            call checkMPIError()
        end do
    else
        ! Receive appropriate 2D section from master
        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, communicator, &
                      status, ierror)
        call checkMPIError()



!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'recv_rank=',rank


        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)
        do k=1, kp
            do c=1, jp
             do r=1, ip
                wa(r, c, k) = recvBuffer(r, c, k)
             end do
            end do
        end do
    end if
end subroutine distributeifw


subroutine distributeifusum(usuma, ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow, kp
    real(kind=4), dimension(0:ipmax,0:jpmax,0:kp), intent(InOut) :: usuma
    integer :: startRow, startCol, i, r, c, k
    real(kind=4), dimension(ip, jp, kp) :: sendBuffer, recvBuffer

    if (isMaster()) then
        ! Send appropriate 2D section to the other ranks
        do i = 1, mpi_size - 1
            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)

            do k=1, kp
                do c=1, jp
                 do r=1, ip
                    sendBuffer(r, c, k) = usuma(startRow + r, startCol + c, k)
                 end do
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)


            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, i, zbmTag, &
                          communicator, ierror)
            call checkMPIError()
        end do
    else
        ! Receive appropriate 2D section from master
        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, communicator, &
                      status, ierror)
        call checkMPIError()



!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'recv_rank=',rank


        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)
        do k=1, kp
            do c=1, jp
             do r=1, ip
                usuma(r, c, k) = recvBuffer(r, c, k)
             end do
            end do
        end do
    end if
end subroutine distributeifusum


subroutine distributeifp(pa, ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow, kp
    real(kind=4), dimension(0:ipmax+2,0:jpmax+2,0:kp+1), intent(InOut) :: pa
    integer :: startRow, startCol, i, r, c, k
!    real(kind=4), dimension(0:1, -1:jp+1, 0:kp+1) :: sendBuffer, recvBuffer
    real(kind=4), dimension(ip, jp, kp) :: sendBuffer, recvBuffer

    if (isMaster()) then
        ! Send appropriate 2D section to the other ranks
        do i = 1, mpi_size - 1
            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)

            do k=1, kp
                do c=1, jp
                 do r=1, ip
                    sendBuffer(r, c, k) = pa(startRow + r, startCol + c, k)
                 end do
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)


            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, i, zbmTag, &
                          communicator, ierror)
            call checkMPIError()
        end do
    else
        ! Receive appropriate 2D section from master
        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, communicator, &
                      status, ierror)
        call checkMPIError()



!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'recv_rank=',rank


        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)
        do k=1, kp
            do c=1, jp
             do r=1, ip
                pa(r, c, k) = recvBuffer(r, c, k)
             end do
            end do
        end do
    end if
end subroutine distributeifp


subroutine distributeiff(fa, ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow, kp
    real(kind=4), dimension(0:ipmax,0:jpmax,0:kp), intent(InOut) :: fa
    integer :: startRow, startCol, i, r, c, k
    real(kind=4), dimension(ip, jp, kp) :: sendBuffer, recvBuffer

    if (isMaster()) then
        ! Send appropriate 2D section to the other ranks
        do i = 1, mpi_size - 1
            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)

            do k=1, kp
                do c=1, jp
                 do r=1, ip
                    sendBuffer(r, c, k) = fa(startRow + r, startCol + c, k)
                 end do
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)


            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, i, zbmTag, &
                          communicator, ierror)
            call checkMPIError()
        end do
    else
        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, communicator, &
                      status, ierror)
        call checkMPIError()


!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'recv_rank=',rank


        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)
        do k=1, kp
            do c=1, jp
             do r=1, ip
                fa(r, c, k) = recvBuffer(r, c, k)
             end do
            end do
        end do
    end if
end subroutine distributeiff


subroutine distributeiffold(folda, ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow, kp
    real(kind=4), dimension(ipmax,jpmax,kp), intent(InOut) :: folda
    integer :: startRow, startCol, i, r, c, k
    real(kind=4), dimension(ip, jp, kp) :: sendBuffer, recvBuffer

    if (isMaster()) then
        ! Send appropriate 2D section to the other ranks
        do i = 1, mpi_size - 1
            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)

            do k=1, kp
                do c=1, jp
                 do r=1, ip
                    sendBuffer(r, c, k) = folda(startRow + r, startCol + c, k)
                 end do
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)

            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, i, zbmTag, &
                          communicator, ierror)
            call checkMPIError()
        end do
    else
        ! Receive appropriate 2D section from master
        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, communicator, &
                      status, ierror)
        call checkMPIError()



!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'recv_rank=',rank


        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)
        do k=1, kp
            do c=1, jp
             do r=1, ip
                folda(r, c, k) = recvBuffer(r, c, k)
             end do
            end do
        end do
    end if
end subroutine distributeiffold


subroutine distributeaveu(aveua, aveu,ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow
    real(kind=4), dimension(0:ipmax,0:jpmax,0:kp) , intent(InOut) :: aveua
    real(kind=4), dimension(ip,jp,0:kp) , intent(In) :: aveu
    integer :: startRow, startCol, i, r, c, k, rank,j
    character(70) :: cha
    real(kind=4), dimension(ip,jp, kp) :: sendBuffer, recvBuffer
    if (.not.isMaster()) then

            do k=1, kp
                do c=1, jp
                   do r=1, ip
                    sendBuffer(r, c, k) = aveu(r, c, k)
                   end do
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)
        

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'send_rank=',rank



            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, &
                          communicator, ierror)
            call checkMPIError()


    else
  
        do i = 1, mpi_size - 1

        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, i,zbmTag,communicator,&
                      status, ierror)
        call checkMPIError()

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)


            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
        write(*,*) 'startRow=',startRow,'startCol=',startCol

        do k=1, kp
             do c=1, jp
               do r=1, ip
                aveua(startRow +r, startCol + c, k) = recvBuffer(r, c, k)
               end do
            end do
        end do

       
        end do
       end if
    
            

end subroutine distributeaveu


subroutine distributeavew(avewa, avew,ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow
    real(kind=4), dimension(ipmax+1,jpmax,0:kp+2) , intent(InOut) :: avewa
    real(kind=4), dimension(ip+1,jp,0:kp+2) , intent(In) :: avew
    integer :: startRow, startCol, i, r, c, k, rank,j
    character(70) :: cha
    real(kind=4), dimension(ip,jp, kp) :: sendBuffer, recvBuffer
    if (.not.isMaster()) then

            do k=1, kp
                do c=1, jp
                   do r=1, ip
                    sendBuffer(r, c, k) = avew(r, c, k)
                   end do
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)
         

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'send_rank=',rank


            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, &
                          communicator, ierror)
            call checkMPIError()


    else
  
        do i = 1, mpi_size - 1

        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, i,zbmTag,communicator,&
                      status, ierror)
        call checkMPIError()

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)


            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
        write(*,*) 'startRow=',startRow,'startCol=',startCol

        do k=1, kp
             do c=1, jp
               do r=1, ip
                avewa(startRow +r, startCol + c, k) = recvBuffer(r, c, k)
               end do
            end do
        end do
   
       
        end do
       end if
    

end subroutine distributeavew


subroutine distributeaveuu(aveuua, aveuu,ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow
    real(kind=4), dimension(0:ipmax,0:jpmax,0:kp) , intent(InOut) :: aveuua
    real(kind=4), dimension(ip,jp,kp) , intent(In) :: aveuu
    integer :: startRow, startCol, i, r, c, k, rank,j
    character(70) :: cha
    real(kind=4), dimension(ip,jp, kp) :: sendBuffer, recvBuffer
    if (.not.isMaster()) then

            do k=1, kp
                do c=1, jp
                   do r=1, ip
                    sendBuffer(r, c, k) = aveuu(r, c, k)
                   end do
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)
         

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'send_rank=',rank


            call MPI_Send(sendBuffer, (ip*jp*kp), MPI_REAL, 0, zbmTag, &
                          communicator, ierror)
            call checkMPIError()


    else
  
        do i = 1, mpi_size - 1

        call MPI_Recv(recvBuffer, (ip*jp*kp), MPI_REAL, i,zbmTag,communicator,&
                      status, ierror)
        call checkMPIError()

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)


            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
        write(*,*) 'startRow=',startRow,'startCol=',startCol

        do k=1, kp
             do c=1, jp
               do r=1, ip
                aveuua(startRow +r, startCol + c, k) = recvBuffer(r, c, k)
               end do
            end do
        end do
   
       
        end do
       end if
    
            

end subroutine distributeaveuu



subroutine distributebondoutu(ubonda, u,ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, kp, ipmax, jpmax, procPerRow
    real(kind=4), dimension(1,jpmax,kp) , intent(InOut) :: ubonda
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
    integer :: startRow, startCol, i, r, c, k, rank,j
    character(70) :: cha
    real(kind=4), dimension( 1, jp, kp) :: sendBuffer, recvBuffer

    if (isBottomRow(procPerRow)) then

            do k=1, kp
                do c=1, jp
                    sendBuffer(1, c, k) = u(ip, c, k)
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)
         

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'send_rank=',rank



            call MPI_Send(sendBuffer, (jp*kp), MPI_REAL, 0, zbmTag, &
                          communicator, ierror)
            call checkMPIError()

    end if

        if (isMaster()) then

        do i = mpi_size - procPerRow, mpi_size - 1

        call MPI_Recv(recvBuffer, (jp*kp), MPI_REAL, i,zbmTag,communicator,&
                      status, ierror)
        call checkMPIError()

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)


            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
        write(*,*) 'startRow=',startRow,'startCol=',startCol

        do k=1, kp
             do c=1, jp
                ubonda(1, startCol + c, k) = recvBuffer(1, c, k)
            end do
        end do

       
        end do
       end if
 end subroutine distributebondoutu



subroutine distributebondoutw(wbonda, w,ip, jp, kp, ipmax, jpmax, procPerRow)
    integer, intent(in) :: ip, jp, kp,ipmax, jpmax, procPerRow
    real(kind=4), dimension(1,jpmax,kp) , intent(InOut) :: wbonda
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
    integer :: startRow, startCol, i, r, c, k, rank,j
    character(70) :: cha
    real(kind=4), dimension( 1, jp, kp) :: sendBuffer, recvBuffer
    if (isBottomRow(procPerRow)) then

            do k=1, kp
                do c=1, jp
                    sendBuffer(1, c, k) = w(ip, c, k)
                end do
            end do
            print*, 'GR: sendBuffer sum: ', sum(sendBuffer)
         

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
!        write(*,*) 'send_rank=',rank



            call MPI_Send(sendBuffer, (jp*kp), MPI_REAL, 0, zbmTag, &
                          communicator, ierror)
            call checkMPIError()


    end if

        if (isMaster()) then
  
        do i = mpi_size - procPerRow, mpi_size - 1

        call MPI_Recv(recvBuffer, (jp*kp), MPI_REAL, i,zbmTag,communicator,&
                      status, ierror)
        call checkMPIError()

!        call MPI_COMM_Rank(communicator, rank, ierror)
!        call checkMPIError()
        print*, 'GR: recvBuffer sum: ', sum(recvBuffer)


            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
        write(*,*) 'startRow=',startRow,'startCol=',startCol

        do k=1, kp
             do c=1, jp
               do r=1, ip
                wbonda(1, startCol + c, k) = recvBuffer(1, c, k)
               end do
            end do
        end do

       
        end do
       end if

  end subroutine distributebondoutw
end module
