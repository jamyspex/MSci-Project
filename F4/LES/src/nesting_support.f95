module nesting_support
    use params_common_sn
    use communication_helper_mpi
    implicit none
!    integer, dimension(0:procPerCol*procPerRow-1) :: syncTicks
    integer :: syncTicks
!    save syncTicks

contains

    subroutine calcSubgridCoords(local_rank,i_s,j_s)
        integer, intent(In) :: local_rank
        integer, intent(Out) :: i_s,j_s
        integer, dimension(2) :: ij_s
!        call MPI_Cart_coords(cartTopComm, local_rank, 2, ij_s, ierror)
!        call checkMPIError()
!        i_s = ij_s(1)
!        j_s = ij_s(2)
!        print *, 'Rank:',local_rank,'Subgrid coords:',
        i_s = local_rank / procPerRow
        j_s = mod(local_rank,procPerRow)
!                i_s = rank / procPerRow ! => row, base 0
!        j_s = rank % procPerRow !=> column, base 0
    end subroutine calcSubgridCoords

    subroutine calcRankBySubgridCoords(i_s,j_s,local_rank)
        integer, intent(Out) :: i_s,j_s
        integer, intent(Out) :: local_rank
        local_rank = i_s*procPerRow+j_s
    end subroutine calcRankBySubgridCoords


    subroutine currentSubgridCoords(i_s,j_s)
        integer, intent(Out) :: i_s,j_s
        integer :: local_rank, local_rank_cart
        call MPI_COMM_Rank(communicator, local_rank, ierror)
!        print *, 'World Rank:',local_rank, 'Comm:',cartTopComm
        call checkMPIError()
        call MPI_COMM_Rank(cartTopComm, local_rank_cart, ierror)
!        print *, 'Cart Rank:',local_rank_cart, 'Comm:',cartTopComm
        call checkMPIError()
        call calcSubgridCoords(local_rank_cart,i_s,j_s)
    end subroutine currentSubgridCoords

    logical function inNestedGridByRank(local_rank) result(in_grid)
            integer, intent(In) :: local_rank
            integer :: i_s, j_s
            call calcSubgridCoords(local_rank,i_s,j_s)
            in_grid = (local_rank > 0) .and. i_s >= i_s_nest_start .and. i_s <= i_s_nest_end .and. j_s >= j_s_nest_start .and. j_s <= j_s_nest_end
    end function inNestedGridByRank

    logical function inNestedGridByCoord(i_s,j_s) result(in_grid)
            integer, intent(In) :: i_s, j_s
            in_grid = i_s >= i_s_nest_start .and. i_s <= i_s_nest_end .and. j_s >= j_s_nest_start .and. j_s <= j_s_nest_end
    end function inNestedGridByCoord

    logical function inNestedGrid() result(in_grid)
            integer :: local_rank
            call MPI_COMM_Rank(communicator, local_rank, ierror)
!            print *, 'rank in inNestedGrid():',local_rank,rank
            call checkMPIError()
            in_grid = inNestedGridByRank(local_rank)
    end function inNestedGrid

end module nesting_support
