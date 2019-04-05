module module_grid
#ifdef MPI
    use communication_helper_real
#ifdef NESTED_LES
      use nesting_support
      implicit none
#endif
#endif
contains

      subroutine grid(dx1,dxl,dy1,dyl,z2,dzn,dzs,dxs,dys)

#ifdef WV_NEW
    use params_common_sn
#else
    use common_sn ! create_new_include_statements() line 102
#endif
        real(kind=4), dimension(-1:ip+1) , intent(Out) :: dx1
        real(kind=4), dimension(0:ip) , intent(Out) :: dxl
        real(kind=4), dimension(0:ip) , intent(Out) :: dxs
        real(kind=4), dimension(0:jp+1) , intent(Out) :: dy1
        real(kind=4), dimension(0:jp) , intent(Out) :: dyl
        real(kind=4), dimension(0:jp) , intent(Out) :: dys
        real(kind=4), dimension(-1:kp+2) , intent(Out) :: dzn
        real(kind=4), dimension(-1:kp+2) , intent(Out) :: dzs
        real(kind=4), dimension(0:kp+2) , intent(Out) :: z2
#ifdef MPI
        real(kind=4), dimension(-1:(ip*procPerCol)+1) :: dx1Tot
        real(kind=4), dimension(0:(jp*procPerRow)+1) :: dy1Tot
        real(kind=4), dimension(0:ip*procPerCol) :: dxlTot
        real(kind=4), dimension(0:jp*procPerRow) :: dylTot
#endif

#ifdef NESTED_LES
        integer :: i_s,j_s ! subgrid coordinates
        integer i,ii,j,jj,k
#endif
!
! WV: I think the use of run-time im,jp,kp is dangerous, unless they are identical to ip,jp,kp
! WV: So I changed it to be that way
! --dx set; streamwise direction
! WV: so -1 and ip+1 are not set!!! I changed it analogous to dy1
!      do i = 0,ip
#ifdef MPI
#ifndef NESTED_LES
      do i = -1,ip+1
       dx1(i) = dxgrid
      end do
!    if (isMaster()) then
!        do i=-1,(ip*procPerCol)+1
!            dx1Tot(i) = dxgrid
!        end do
!    end if
!    call distribute1DRealRowWiseArray(dx1Tot,dx1, 2, 1, procPerRow)
#else
            call currentSubgridCoords(i_s,j_s)
            do i=-1,ip+1
                ii = i_s*ip !+i ! for example 1*75; start=75, end=225
                if (ii>=nested_grid_start_x .and. ii< nested_grid_end_x) then
                    ! In the nested grid
                    ! Now, what about the neightbours?
                    if (i<1) then
                        ! Left halo
                         if (i_s>0 .and. .not. inNestedGridByCoord(i_s-1,j_s)) then
                            ! Neighbour is orig
                            dx1(i) = dxgrid_orig
                         else
                            dx1(i) = dxgrid_nest
                         end if
                    else if (i>ip) then
                        ! Right halo
                         if (i_s<procPerRow-1 .and. .not. inNestedGridByCoord(i_s+1,j_s)) then
                            ! Neighbour is orig
                            dx1(i) = dxgrid_orig
                         else
                            dx1(i) = dxgrid_nest
                         end if
                    else
                    ! Core
                        dx1(i) = dxgrid_nest
                    end if
                else
                    ! In the original grid
                    ! Now, what about the neightbours?
                    if (i<1) then
                    ! Left halo
                         if (i_s>0 .and. .not. inNestedGridByCoord(i_s-1,j_s)) then
                            ! Neighbour is nest NN|OO
                            dx1(i) = dxgrid_nest
                         else
                           ! OO|OO
                            dx1(i) = dxgrid_orig
                         end if
                    else if (i>ip) then
                    ! Right halo
                         if (i_s<procPerRow-1 .and. .not. inNestedGridByCoord(i_s+1,j_s)) then
                            ! Neighbour is nest OO|NN
                            dx1(i) = dxgrid_nest
                         else
                            dx1(i) = dxgrid_orig
                         end if
                    else
                    ! Core
                        dx1(i) = dxgrid_orig
                    end if
                end if
            end do
#endif
#else
#ifndef NESTED_LES
        do i = -1,ip+1
            dx1(i) = dxgrid
        end do
#else
        do i=-1,ip+1
            if (i>=nested_grid_start_x .and. i< nested_grid_end_x) then
                ! In the nested grid
                        dx1(i) = dxgrid_nest
                        else
                ! In the orig grid
                        dx1(i) = dxgrid_orig
            end if
        end do
#endif
#endif

#ifdef MPI
#ifndef NESTED_LES
      dxl(0) = 0.
      do i = 1,ip
        dxl(i) = dxl(i-1)+dx1(i)
      end do
!    if (isMaster()) then
!        dxlTot(0) = 0.
!        do i = 1, ip*procPerCol
!            dxlTot(i) = dxlTot(i-1) + dx1Tot(i)
!        end do
!    end if
!    call distribute1DRealRowWiseArray(dxlTot,dxl, 1, 0, procPerRow)
#else
      dxl(0) = 0.
      do i = 1,ip
        dxl(i) = dxl(i-1)+dx1(i)
      end do
#endif
#else
      dxl(0) = 0.
      do i = 1,ip
       dxl(i) = dxl(i-1)+dx1(i)
      end do
#endif

! --dy set; spanwise direction
!WV: let's set the *whole* array to this value!
      !do j = 0,jp
#ifdef MPI
#ifndef NESTED_LES
      do j = 0,jp+1
       dy1(j) = dygrid
      end do
!    if (isMaster()) then
!        do j=0,(jp*procPerRow)+1
!            dy1Tot(j) = dygrid
!        end do
!    end if
!    call distribute1DRealColumnWiseArray(dy1Tot, dy1, 1, 1, procPerRow)
#else
!            call currentSubgridCoords(i_s,j_s)
!            do j=-1,jp+1
!                jj = j_s*jp+j
!                if (jj>nested_grid_start_y .and. jj< nested_grid_end_y) then
!                    dy1(i) = dygrid_nest
!                else
!                    dy1(i) = dygrid_orig
!                end if
!            end do

        do j=0,jp+1
            jj = j_s*jp !+j
            if (jj>=nested_grid_start_y .and. jj< nested_grid_end_y) then
                ! In the nested grid
                ! Now, what about the neightbours?
                if (j<1) then
                    ! Bottom halo
                     if (j_s>0 .and. .not. inNestedGridByCoord(i_s,j_s-1)) then
                        ! Neighbour is orig
                        dy1(j) = dygrid_orig
                     else
                        dy1(j) = dygrid_nest
                     end if
                else if (j>jp) then
                    ! Top halo
                     if (j_s<procPerCol-1 .and. .not. inNestedGridByCoord(i_s,j_s+1)) then
                        ! Neighbour is orig
                        dy1(j) = dygrid_orig
                     else
                        dy1(j) = dygrid_nest
                     end if
                else
                ! Core
                    dy1(j) = dygrid_nest
                end if
            else
                ! In the original grid
                ! Now, what about the neightbours?
                if (j<1) then
                 ! Bottom halo
                     if (j_s>0 .and. .not. inNestedGridByCoord(i_s,j_s-1)) then
                        ! Neighbour is nest NN|OO
                        dy1(j) = dygrid_nest
                     else
                       ! OO|OO
                        dy1(j) = dygrid_orig
                     end if
                else if (j>jp) then
                    ! Top halo
                     if (j_s<procPerRow-1 .and. .not. inNestedGridByCoord(i_s,j_s+1)) then
                        ! Neighbour is nest OO|NN
                        dy1(j) = dygrid_nest
                     else
                        dy1(j) = dygrid_orig
                     end if
                else
                 ! Core
                   dy1(j) = dygrid_orig
                end if
            end if
        end do

#endif
#else
#ifndef NESTED_LES
      do j = 0,jp+1
            dy1(j) = dygrid
      end do
#else
        do j=0,jp+1
            if (j>=nested_grid_start_y .and. j< nested_grid_end_y) then
                ! In the nested grid
                        dy1(j) = dygrid_nest
                        else
                ! In the orig grid
                        dy1(j) = dygrid_orig
            end if
        end do
#endif
#endif

#ifdef MPI
#ifndef NESTED_LES
      dyl(0) = 0.
      do j = 1,jp
       dyl(j) = dyl(j-1)+dy1(j)
      end do
!    if (isMaster()) then
!        dylTot(0) = 0.
!        do j=1,(jp*procPerRow)
!            dylTot(j) = dylTot(j-1) + dy1Tot(j)
!        end do
!    end if
!    call distribute1DRealColumnWiseArray(dylTot, dyl, 1, 0, procPerRow)
#else
      dyl(0) = 0.
      do j = 1,jp
       dyl(j) = dyl(j-1)+dy1(j)
      end do
#endif
#else
      dyl(0) = 0.
      do j = 1,jp
       dyl(j) = dyl(j-1)+dy1(j)
      end do
#endif
! --dz set; vertical direction
!WV: also define the first and last point in the array!
!      do k = 0,1
!        z2(k) = 2.5
!        dzn(k) = 2.5
!      end do
!      do k = 2,15
!        dzn(k) = dzn(k-1)*1.05
!      end do
!      do k = 16,44
!        dzn(k) = 5.
!      end do
!      do k = 45,kp+1
!        dzn(k) = dzn(k-1)*1.0459
!      end do
!      do k = 2,kp+2 ! WV: was kp+1
!        z2(k) = z2(k-1)+dzn(k)
!      end do
      ! so z2(kp+2) is not set, why?

!original
!      do k=0,1
!        z2(k)= 1.
!        dzn(k)= 1.
!        write(*,*) 'dzn=',dzn(k)
!      end do


        z2(0)= 0.
        dzn(0)= 1.

        z2(1)= 1.
        dzn(1)= 1.

      do k=2,15
        dzn(k)=dzn(k-1)*1.1
        ! write(*,*) 'dzn=',dzn(k)
      end do
      do k=16,44
        dzn(k)=4.
      end do
      do k=45,58
        dzn(k)=dzn(k-1)*1.1
      end do
      do k=59,kp+1
        dzn(k)=16.
      enddo
      do k=2,kp+2
        z2(k)=z2(k-1)+dzn(k)  !Height
      end do

#ifdef MPI
    if (isMaster()) then
#endif
      do k=1,kp
       ! write(*,*) 'z2grid=',z2(k)
      end do
#ifdef MPI
    end if
#endif

! --gaiten deno haba
      dzn(kp+1) = dzn(kp)
      !WV
      dzn(kp+2) = dzn(kp+1)
      dzn(0) = dzn(1)
      !WV
      dzn(-1)=dzn(0)
! -------------------------------------
      do k = 0,kp
        dzs(k) = dzn(k+1)/2.+dzn(k)/2.
      end do
! GR: dxs is defined from 0:ip but ip+1 is written to
! GR: dx1 is defined from -1 to ip+1 but ip+2 is read from
      !do i = 0,ip+1
      do i=0, ip
        dxs(i) = dx1(i)/2.+dx1(i+1)/2.
      end do
! WV: so the access to the undefine dy1(jp+2) seems to be what causes corruption of dy1(0)
! WV: In fact, dys is only defined (0 .. jp) so most likely they run into one another
      !do j = 0,jp+1
      do j = 0,jp
        dys(j) = dy1(j)/2.+dy1(j+1)/2.
      end do
!
      dzs(kp+1) = dzs(kp)
      dzs(kp+2) = dzs(kp+1) !WV
      dzs(-1) = dzs(0) !WV
!
#ifdef VERBOSE
      write(6,*) 'Computational Domain X,Y,Z=',dxl(ip),dyl(jp),z2(kp)
      do k = 1,kp
      write(6,*) 'Vertical grid size=',k,dzn(k),dzs(k)
      end do
#endif
!
      return
      end subroutine grid

end module module_grid
